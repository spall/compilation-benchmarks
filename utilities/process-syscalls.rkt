#lang racket

(require parser-tools/lex
	 (prefix-in : parser-tools/lex-sre)
	 c/parse
	 c/ast
	 "flags.rkt")

(provide process-in-out
	 process-all-pids
	 (struct-out syscall)
	 PROJ-DIR)

(define debug-hash (make-hash))

(struct syscall (name args retval) #:prefab)
(struct sc-open (file read? write? retval))
(struct sc-execve (file))
(struct sc-stat (name))
(struct sc-chown (name))
(struct sc-access (name))
(struct sc-mkdir (name))
(struct sc-chmod (name))
(struct sc-readlink (name))
(struct sc-fcntl (val flag))
(struct sc-setxattr (name))
(struct sc-symlink (content name))
(struct sc-rename (old new))
(struct sc-unlink (name))
(struct sc-fork (pid))

(define (remove-trailing-separator p)
  (string->path (string-trim (path->string p) "/" #:left? #f)))

(define (all-dirs fpath)
  (define (helper p)
    (define-values (base name _) (split-path p))

    (cond
     [(equal? base 'relative)
      (error 'all-dirs "Should always be an absolute path ~a" p)]
     [(equal? base (string->path "/"))
      (helper base)]
     [base
      (cons (path->string (remove-trailing-separator base)) (helper base))]
     [else
      '()]))
      
  (when (number? fpath)
    (error 'all-dirs "not a path ~a" fpath))

  (helper (simplify-path fpath)))


(define PROJ-DIR (make-parameter #f)) ;; to be set by caller
(define (files-to-ignore) (foldl (lambda (p accu)
			         (append (cons p (all-dirs (string->path p))) accu))
			       '()
			       (list "/dev/tty" "/dev/null" "/data/home.local/sjspall/compilation-benchmarks" (PROJ-DIR))))

(define (lookup-file-descriptor table fd func)
  (hash-ref table fd (lambda () fd)))

(define (lookup-file-descriptor-error table fd func)
  (hash-ref table fd (lambda ()
                       (error func "Did not find file descriptor ~a in table ~a" fd table))))

(define (maybe-create-absolute-path fname fd cdir table func)
  (cond
   [(absolute-path? fname)
    fname]
   [(equal? fd "AT_FDCWD")
    (path->complete-path fname cdir)]
   [else
    (define fdnum (string->number fd))
    (unless fdnum
      (error func "Failed to parse fd as a number ~a" fd))
    (path->complete-path fname  (lookup-file-descriptor-error table fdnum func))]))

(define get-string-token
    (lexer
     [(:~ #\" #\\) (cons (car (string->list lexeme))
                         (get-string-token input-port))]
     [(:: #\\ #\\) (cons #\\ (get-string-token input-port))]
     [(:: #\\ #\") (cons #\" (get-string-token input-port))]
     [#\" null]))

;; dumb lexer; can't handle comma's inside of parens
(define arg-lexer
  (lexer
   [(eof) ""]
   [whitespace (arg-lexer input-port)]
   [#\" (string-append
   	  (list->string (get-string-token input-port))
   	  (arg-lexer input-port))]
   [#\, ""]
   [any-char (string-append lexeme (arg-lexer input-port))]))

(define (args-parser n ip)
  (cond
    [(> n 0)
     (define tmp (arg-lexer ip))
     (cons tmp
           (args-parser (- n 1) ip))]
    [else
     '()]))

(define (parse-flags bops)
  (match bops
   [(expr:binop _ l op r)
    (append (parse-flags l) (parse-flags r))]
   [(expr:ref _ (id:var __ name))
    (list name)]
   [else
    (error 'parse-flags "Did not match ~a" bops)]))

(define (parse-access-args args)
  (match args
   [(expr:begin _ (expr:string __ str w?) right)
    (values str (parse-flags right))]
   [else
    (error 'parse-access-args "Did not match ~a" args)]))

(define (parse-open-args args)
   (match args
   [(expr:begin _ (expr:string __ str w?) right)
    (values str (parse-flags right))]
   [(expr:begin _ left right) ;; right is mode which we do not care about
    (parse-open-args left)]
   [else
    (error 'parse-open-args "Did not match ~a" args)]))

(define (parse-open-syscall scall cdir table)
  (define fd (string->number (string-trim (syscall-retval scall))))
  (define-values (fname flags) 
    (parse-open-args (parse-expression (open-input-string (syscall-args scall)))))

     ;; Figure out if this is a read or write, or both
  (define-values (read? write?)
    (for/fold ([read? #f] [write? #f])
       	      ([f flags])
      (match f
       ['O_RDONLY
	(values #t write?)]
       [(or 'O_RDWR 'O_APPEND)
	(values #t #t)]
       [(or 'O_WRONLY 'O_CREAT 'O_EXCL)
	(values read? #t)]
       ['O_TRUNC            ;; behavior of trunc with rdonly is undefined....
	(values #f #t)]     ;; say reading is false. may be a lie?
       [else
	(values read? write?)])))

     ;; Can't necessarily distinguish between file 
     ;; and directory so add to table regardless; if it isn't a directory then
     ;; a later call that uses this fd as a directory should fail.
     (define full-path (if (absolute-path? fname)
     	     	       	   fname
			   (path->complete-path fname cdir)))
     
     (values (sc-open full-path read? write? fd) #f (hash-set table fd full-path)))

(define (parse-openat-syscall scall cdir table)
  (define fd (string->number (string-trim (syscall-retval scall))))
  (unless fd
    (error 'parse-openat-syscall "Failed to parse return value as number ~a" (string-trim (syscall-retval scall))))

  (define args (args-parser 3 (open-input-string (syscall-args scall))))
  (define dirfd (car args))
  (define fname (cadr args))
  (define flags (string-split "|" (caddr args)))
  
  ;; Figure out if this is a read or write, or both
  (define-values (read? write?)
    (for/fold ([read? #f] [write? #f])
              ([f flags])
      (match f
       ["O_RDONLY"
	(values #t write?)]
       ["O_WRONLY"
	(values read? #t)]
       ["O_RDWR"
	(values #t #t)]
       [(or "O_CREAT" "O_EXCL")
	(values read? #t)]
       [else
	(values read? write?)])))

     ;; same as previous case I believe
     (define full-path (maybe-create-absolute-path fname dirfd cdir table 'parse-openat-syscall))

     (values (sc-open full-path read? write? fd) #f (hash-set table fd full-path)))

(define (parse-execve-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
  (values (sc-execve (if (absolute-path? fname)
			 fname
			 (path->complete-path fname cdir)))
          #f table))

(define (parse-chdir-syscall scall cdir table)
  ;; todo consider if fpath is relative.........
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (define fpath (match expr
     	     	 [(expr:string __ str w?)
		  str]
		 [else
		  (error 'parse-syscall "Did not match ~a" expr)]))

  (values #f (if (absolute-path? fpath)
      	     	 fpath
		 (path->complete-path fpath cdir))
	     table))

(define (parse-fchdir-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
     (define fdir (match expr
     	     	   [(expr:int __ val q)
		    val]
		   [else
		    (error 'parse-syscall "Did not match ~a" expr)]))
     (values #f (lookup-file-descriptor-error table fdir 'parse-fchdir-syscall) table))

(define (parse-stat-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
  (values (sc-stat (if (absolute-path? fname)
      	  	       fname
		       (path->complete-path fname cdir)))
	  #f table))

(define (parse-fstat-syscall scall cdir table)
  (define args (args-parser 1 (open-input-string (syscall-args scall))))
  (define fd (string->number (car args)))
  (unless fd
    (error 'parse-fstat-syscall "Failed to parse fd as a number ~a" (car args)))   
  
  (values (sc-stat (lookup-file-descriptor table fd 'parse-fstat-syscall))
  	   #f table))


;; fstatat
(define (parse-fstatat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define fname (cadr args))
  
  (values (sc-stat (maybe-create-absolute-path fname fd cdir table 'parse-fstatat-syscall))
	  #f table))

(define (parse-fcntl-args args_ table retval)
  (define (driver args rest)
    (match args
     [(expr:begin _ (expr:int __ val q) (expr:ref src (id:var src2 cmd)))
      (match cmd
       ['F_SETFL
        (define tmp (car (parse-flags (car rest))))
        (match tmp                 ;; set status flags
	 ['O_APPEND
	  (values (sc-fcntl (lookup-file-descriptor table val 'parse-fcntl-args)
	  	  	     'O_APPEND) #f table)]  
	 [else
	  (values #f #f table)])]
       ['F_DUPFD
        (values #f #f (hash-set table retval (lookup-file-descriptor table val 'parse-fcntl-args)))]
       [else
        (values #f #f table)])]                  
     [(expr:begin _ left right)
      (driver left (cons right rest))]
     [else
      (error 'parse-fcntl-args "Did not match ~a" args)]))
  (driver args_ '()))

(define (parse-fcntl-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (parse-fcntl-args expr table (string->number (string-trim (syscall-retval scall))))) 

(define (parse-readlink-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-readlink (if (absolute-path? fname)
  	  	       	   fname
			   (path->complete-path fname cdir)))
          #f table))

(define (parse-readlinkat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define fname (cadr args))
  
  (values (sc-readlink (maybe-create-absolute-path fname fd cdir table 'parse-readlinkat-syscall))
          #f table))

(define (parse-setxattr-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-setxattr (if (absolute-path? fname)
      	  	       	   fname
      			   (path->complete-path fname cdir)))
          #f table))

(define (parse-fsetxattr-syscall scall cdir table)
  (define args (args-parser 1 (open-input-string (syscall-args scall))))
  (define fd (string->number (car args)))
  (unless fd
    (error 'parse-fsetxattr-syscall "Failed to parse fd as a number ~a" (car args)))
  
  (values (sc-setxattr (lookup-file-descriptor-error table fd 'parse-fsetxattr-syscall))
   	  #f table))

(define (parse-chown-args args)
  (match args
   [(expr:begin src1 (expr:begin src2 (expr:string src3 str w?) (expr:int src4 uid q1)) (expr:int src5 gid q2))
    str]
   [else
    (error 'parse-chown-args "Did not match ~a" args)]))

(define (parse-chown-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (define fname (parse-chown-args expr))
  
  (values (sc-chown (if (absolute-path? fname)
  	  	    	fname
			(path->complete-path fname cdir)))
          #f table))

(define (parse-chmod-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-chmod (if (absolute-path? fname)
   	   	     	fname
			(path->complete-path fname cdir)))
	   #f table))

(define (parse-fchmod-syscall scall cdir table)
  (define args (args-parser 1 (open-input-string (syscall-args scall))))
  (define fd (string->number (car args)))
  (unless fd
    (error 'parse-fchmod-syscall "Failed to parse fd as a number ~a" (car args)))
  
  (values (sc-chmod (lookup-file-descriptor table fd 'parse-fchmod-syscall))
	  #f table))

(define (parse-fchmodat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define fname (cadr args))

  (values (sc-chmod (maybe-create-absolute-path fname fd cdir table 'parse-fchmodat-syscall))
	  #f table))

(define (parse-access-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (define-values (fname flags) (parse-access-args expr))

  (values (sc-access (if (absolute-path? fname)
  	  	     	 fname
			 (path->complete-path fname cdir)))
	  #f table))   

(define (parse-faccessat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define fname (cadr args))

  (values (sc-access (maybe-create-absolute-path fname fd cdir table 'parse-faccessat-syscall))
  	  #f table))

(define (parse-mkdir-args args)
  (match args
   [(expr:begin _ (expr:string __ str w?) (expr:int src val q))
    str]
   [else
    (error 'parse-mkdir-args "Did not match ~a" args)]))

(define (parse-mkdir-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (define path (parse-mkdir-args expr))
  ;; TODO need to do relative path versus absolute path.  Likely need to do this 
  ;; for some other system calls as well.

  (values (sc-mkdir (if (absolute-path? path)
  	  	    	path
			(path->complete-path path cdir)))
	  #f table))

(define (parse-mkdirat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define path (cadr args))
  
  (values (sc-mkdir (maybe-create-absolute-path path fd cdir table 'parse-mkdirat-syscall))
  	  #f table))  

(define (parse-symlink-args args)
  (match args
   [(expr:begin _ (expr:string __ str1 w?) (expr:string src str2 w2?))
    (values str1 str2)]
   [else
    (error 'parse-symlink-args "Did not match ~a" args)]))

(define (parse-symlink-syscall scall cdir table)
  (define-values (target lpath) (parse-symlink-args (parse-expression (open-input-string (syscall-args scall)))))

  (define nlpath (if (absolute-path? lpath)
  	  	      	         lpath
			         (path->complete-path lpath cdir)))
  (define-values (dir _ __) (split-path nlpath))
  (define ntarget (if (absolute-path? target)
  	  	      target
		      (path->complete-path target dir))) 

  (values (sc-symlink ntarget nlpath)
          #f table))

(define (parse-rename-args args)
  (match args
   [(expr:begin _ (expr:string __ str1 w?) (expr:string src str2 w2?))
    (values str1 str2)]
   [else
    (error 'parse-rename-args "Did not match ~a" args)]))

(define (parse-symlinkat-syscall scall cdir table)
  (define args (args-parser 3 (open-input-string (syscall-args scall))))
  (define target (car args))
  (define newdirfd (cadr args))
  (define lpath (caddr args))

  (define nlpath (maybe-create-absolute-path lpath newdirfd cdir table 'parse-symlinkat-syscall))
  (define-values (dir _ __) (split-path nlpath))
  (define ntarget (if (absolute-path? target)
  	  	      target
		      (path->complete-path target dir)))

  (values (sc-symlink ntarget nlpath)
          #f table))

(define (parse-rename-syscall scall cdir table)
  (define-values (old new) (parse-rename-args (parse-expression (open-input-string (syscall-args scall)))))

  (values (sc-rename
  	    (if (absolute-path? old)
	    	old
		(path->complete-path old cdir))
            (if (absolute-path? new)
	    	new
		(path->complete-path new cdir)))
          #f table))

(define (parse-unlink-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
  (values (sc-unlink (if (absolute-path? fname)
  	  	     	 fname
			 (path->complete-path fname cdir)))
          #f table))

(define (parse-unlinkat-syscall scall cdir table)
  (define args (args-parser 3 (open-input-string (syscall-args scall))))
  (define dirfd (car args))
  (define fname (cadr args))
  (define flags (caddr args))

  (values (sc-unlink (maybe-create-absolute-path fname dirfd cdir table 'parse-unlinkat-syscall))
          #f table))
       
(define (parse-fork-syscall scall cdir table)
  (values (sc-fork (string->number (string-trim (syscall-retval scall))))
  	  #f table))

(define (parse-dup-syscall scall cdir table)
  (define args (args-parser 1 (open-input-string (syscall-args scall))))
  (define oldfd (string->number (car args)))
  (unless oldfd
    (error 'parse-dup-syscall "Failed to parse ~a as a number" (car args)))

  (define newfd (string->number (string-trim (syscall-retval scall))))
  (unless newfd
    (error 'parse-dup-syscall "Failed to parse return value as a number ~a" (string-trim (syscall-retval scall))))

  (cond
   [(hash-ref table oldfd #f) =>
    (lambda (val)
      (values #f #f (hash-set table newfd val)))]
   [else
    (values #f #f table)]))

(define (parse-dup2-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define oldfd (string->number (car args)))
  (define newfd (string->number (cadr args)))

  (unless oldfd
    (error 'parse-dup2-syscall "Failed to parse ~a as a number" (car args)))
  (unless newfd
    (error 'parse-dup2-syscall "Failed to parse ~a as a number" (cadr args)))

  (cond
   [(hash-ref table oldfd #f) =>
    (lambda (val)
      (values #f #f (hash-set table newfd val)))]
   [else
    (values #f #f table)]))

(define (dispatch scall)
  (match (syscall-name scall)
    ["open"   parse-open-syscall]
    ["openat" parse-openat-syscall]

    ["chown" parse-chown-syscall]

    ["chmod"    parse-chmod-syscall]
    ["fchmod"   parse-fchmod-syscall]
    ["fchmodat" parse-fchmodat-syscall]

    ["dup"  parse-dup-syscall]
    [(or "dup2" "dup3") parse-dup2-syscall]

    ["access"    parse-access-syscall]
    ["faccessat" parse-faccessat-syscall]

    ["mkdir"  parse-mkdir-syscall]
    ["mkdirat" parse-mkdirat-syscall]

    ["execve" parse-execve-syscall]

    ["chdir"  parse-chdir-syscall]
    ["fchdir" parse-fchdir-syscall]

    [(or "lstat" "stat" "statfs") ;; these all behave the same
     parse-stat-syscall]
    [(or "fstat" "fstatfs") 
     parse-fstat-syscall]
    [(or "fstatat" "newfstatat")
     parse-fstatat-syscall] ;; fstatat  

    ["fcntl" parse-fcntl-syscall]

    ["readlink" parse-readlink-syscall]
    ["readlinkat" parse-readlinkat-syscall]
    
    [(or "setxattr" "lsetxattr")
     parse-setxattr-syscall]
    ["fsetxattr" parse-fsetxattr-syscall]
    
    ["symlink" parse-symlink-syscall]
    ["symlinkat" parse-symlinkat-syscall]
    
    ["rename" parse-rename-syscall]

    ["unlink" parse-unlink-syscall]
    ["unlinkat" parse-unlinkat-syscall]

    ["fork" parse-fork-syscall]
    ["clone" parse-fork-syscall]
    ["vfork" parse-fork-syscall]
    ;; add more here
    [else
     (lambda (scall cdir table)
       (values #f cdir table))]))


(define (inputs-outputs scall)
  (match scall
   [(sc-open f r? w? r)
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (if r? (cons f dirs) dirs)
	    (if w? (list f) '()))]
   [(sc-execve f) ;; input
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) '())]
   [(sc-stat f) ;; input
    (cond
     [(number? f)
      (when (debug?)
	    (printf "Warning: sc-stat file is ~a\n" f))
      (values '() '())]
     [else
      ;; need to consider directories
      (define dirs (all-dirs f))
      (values (cons f dirs) '())])]
   [(sc-chown f) ;; input and output
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) (list f))]
   [(sc-access f) ;; input
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) '())]
   [(sc-mkdir d) ;; output
    ;; need to consider directories
    (define dirs (all-dirs d))
    (values dirs (list d))]
   [(sc-chmod f) ;; input and output
    (cond
     [(or (equal? 0 f) (equal? 1 f) (equal? 2 f))
      (values '() '())]
     [else
      ;; need to consider directories
      (define dirs (all-dirs f))
      (values (cons f dirs) (list f))])]
   [(sc-readlink f)
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) '())]
   [(sc-fcntl v fl) ;; TODO FIX THIS
    (cond
     [(or (equal? 0 v) (equal? 1 v) (equal? 2 v))
      (values '() '())]
     [else
      (values (list v) (list v))])]	
   [(sc-setxattr f)
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) (list f))]
   [(sc-symlink g f)
    ;; need to consider directories
    (define dirs0 (cons g (all-dirs g))) 
    (define dirs (all-dirs f))
    (values (append dirs0 dirs) (list f))]
   [(sc-rename o n)
    ;; need to consider directories
    (define dirs1 (all-dirs o))
    (define dirs2 (all-dirs n))
    (values (cons o (append dirs1 dirs2)) (list n))]
   [(sc-unlink f)
    ;; need to consider directories
    (define dirs (all-dirs f))
    (values (cons f dirs) (list f))]
   [else
    (values '() '())]))

(define SPECIAL (list "unlink" "unlinkat" "rm" "rmdir" "rename" "mkdir"))

(define (parse-syscall scall cdir table)
 (if (and (string-prefix? (syscall-retval scall) "-1")
     	  (not (member (syscall-name scall) SPECIAL)))
     (values #f cdir table)
     ((dispatch scall) scall cdir table)))

(define (fork? scall)
  (cond
   [(string-prefix? (syscall-retval scall) "-1")
    #f]
   [(or (equal? "fork" (syscall-name scall)) 
	(equal? "clone" (syscall-name scall)) (equal? "vfork" (syscall-name scall)))
    (define-values (res _ __) ((dispatch scall) scall #f (hash)))
    (unless (sc-fork? res)
	    (error 'fork? "System call name is ~a but didn't receive expected struct." (syscall-name scall)))
    (sc-fork-pid res)]
   [else
    #f]))

;; starting pid
;; ordering of pids
;; hash table from pid -> hash table from n -> calls
;; returns list of calls and new ordering
(define (combine-forks pid ordering syscalls)
  (when (empty? ordering)
	(error 'combine-forks "ordering is empty!"))
  (cond
   [(hash-ref syscalls pid #f) =>
    (lambda (ntable)
      (define scalls (cond
		      [(hash-ref ntable 1 #f) ;; more than 1 process ran with pid
		       ;; first thing in ordering should correspond to this pid
		       (unless (equal? (caar ordering) pid)
			       (error 'combine-forks "First pid in ordering is ~a; expected ~a" (car ordering) pid))
		       (hash-ref ntable (cdar ordering))]
		      [else ;; only 1 process ran with this pid so easy
		       (hash-ref ntable 0)]))

	;; look through scalls for forks and follow them
	(let loop ([scs scalls]
		   [order (cdr ordering)])
	  (cond
	   [(empty? scs)
	    (values '() order)]
	   [(fork? (car scs)) =>
	    (lambda (fpid)
	      (when (empty? order)
		    (error 'combine-forks "forked pid ~a but ordering is empty!" fpid))
	      (define-values (fcalls norder) (combine-forks fpid order syscalls))
	      (define-values (calls norder2) (loop (cdr scs) norder))
	      (values (append fcalls calls) norder2))]
	   [else ;; not a fork
	    (call-with-values (lambda () (loop (cdr scs) order))
	      (lambda (calls norder)
		(values (cons (car scs) calls)
			norder)))])))]
      [else ;; no info shouldn't happen?
       (error 'combine-forks "No syscall info for pid ~a" pid)]))
   
;; convert generic system call into specific system call
;; ignore calls that resulted in an error.
(define (process-syscalls pid cdir_ table_ syscalls)
  (when (debug?)
	(printf "processing syscalls for pid ~a\n" pid))     	  
  (let loop ([cdir cdir_]
	     [table table_]
	     [scs syscalls])
    (cond
     [(empty? scs)
      '()]
     [else
      (define-values (res ndir ntable) (parse-syscall (car scs) cdir table))
      (cond
       [(or (equal? res #f) (void? res))
	(loop (or ndir cdir) ntable (cdr scs))]
       [(sc-fork? res)
	(error 'process-syscalls "Encountered a fork but these should have been processed and removed; pid ~a" pid)]
       [else
	(define recur (loop (or ndir cdir) ntable (cdr scs)))
	(if recur
	    (cons res recur)
	    #f)])])))

(define (next-n table)
  (+ 1 (for/fold ([max_ 0])
		 ([k (in-hash-keys table)])
		 (max max_ k))))

;; process pids in order they appear in syscalls
;; order is important here because pid's can be reused.
(define (process-all-pids ordering_ syscalls)

  (define new-table (make-hash)) ;; pid -> (hash from num -> calls - forks) ;; 0 indexed

  (define (helper ordering)
    (unless (empty? ordering)
	    (define pid (caar ordering))
	    (define-values (calls nordering) (combine-forks (caar ordering) ordering syscalls))
	    ;; add to new-table
	    (cond
	     [(hash-ref new-table pid #f) => ;; there is already a table in new-table for pid
	      (lambda (tmp)
		(define n (next-n tmp))
		(hash-set! new-table pid (hash-set tmp n calls)))]
	     [else
	      (hash-set! new-table pid (hash 0 calls))])
	    
	    (helper nordering)))
  
  (helper ordering_) ;; populates new-table
  new-table)

;; determines what the inputs/outputs are.
;; also returns ins and outs for pid's created via fork/clone by pid
(define (process-in-out pid cdir syscalls)
  (define calls (process-syscalls pid cdir (hash) syscalls))
  (if calls
      (for/fold ([in '()]
                 [out '()])
                ([call calls])

        (define-values (is os) (inputs-outputs call))

	;; turn all paths into strings
	(define string-is (map (lambda (i)
			         (if (path? i)
				     (path->string i)
				     i))
			       is))
        (define string-os (map (lambda (o)
			         (if (path? o)
				     (path->string o)
				     o))
                               os))

	;; remove things that should be ignored
	(define nos (foldl (lambda (o accu)
	              	     (if (member o (files-to-ignore))
		      	     	 accu
		     	  	 (cons o accu)))
                           '()
	          	   string-os))	
	
        (values (append (filter (lambda (x)
     	     	     	          (not (member x in))) string-is)
	                in)
	        (append (filter (lambda (x)
	     	     	          (not (member x out))) nos)
		        out)))
    (values #f #f)))

        
  




  
