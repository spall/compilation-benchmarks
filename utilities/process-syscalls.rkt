#lang errortrace racket

(require parser-tools/lex
	 (prefix-in : parser-tools/lex-sre)
	 c/parse
	 c/ast
	 "flags.rkt")

(provide process-in-out-pid
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

;; resolve-symlink to a fixed point
(define (resolve-symlink p)
  (define tmp (resolve-path p))
  (cond
   [(equal? tmp p)
    p]
   [(absolute-path? tmp)
    (resolve-symlink tmp)]
   [else
    (define-values (base name _) (split-path p))
    (when (equal? 'up name)
	  (error 'resolve-symlink "Previous directory is symlink? ~a" p))
    (resolve-symlink (simplify-path (path->complete-path tmp base)))]))
			      
;; follow symlinks
(define (resolve-symlinks p_)
  (define (helper p)
    (define rp (resolve-symlink (simplify-path p)))
    (define-values (base name _) (split-path rp))
    (cond
     [(equal? base 'relative)
      (error 'resolve-symlinks "Should always be an absolute path ~a" p)]
     [(equal? base (string->path "/"))
      p]
     [base
      (path->string (path->complete-path name (resolve-symlinks base)))]
     [else
      p]))
  (helper p_))

;(resolve-symlinks (string->path "/data/home.local"))
;(resolve-symlinks (string->path "/data/home.local/sjspall/tmp/tmpfilesym"))


(define (remove-trailing-separator p)
  (string->path (string-trim (path->string p) "/" #:left? #f)))

(define (all-dirs fpath)
  (define (helper p)
    (define-values (base name _) (split-path p))

    (cond
     [(equal? base 'relative)
      (error 'all-dirs "Should always be an absolute path ~a" p)]
     [(equal? base (string->path "/"))
      '()]
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
			       (list "/tmp" "/dev/tty" "/dev/null" "/nix/other_data/home.local/sjspall/compilation-benchmarks" (resolve-symlinks (PROJ-DIR)))))

(define (lookup-file-descriptor table fd func)
  (hash-ref table fd (lambda () fd)))

(define (lookup-file-descriptor-error table fd func)
  (hash-ref table fd (lambda ()
                       (error func "Did not find file descriptor ~a in table ~a" fd table))))

(define (maybe-create-absolute-path fname fd cdir table func)
  (cond
   [(absolute-path? fname)
    (resolve-symlinks fname)]
   [(or (not fd) (equal? fd "AT_FDCWD"))
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
  (define full-path (maybe-create-absolute-path fname #f cdir table 'parse-open-syscall)) 
  
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
  
  (values (sc-execve (maybe-create-absolute-path fname #f cdir table 'parse-execve-syscall))
          #f table))

(define (parse-chdir-syscall scall cdir table)
  ;; todo consider if fpath is relative.........
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (define fpath (match expr
     	     	 [(expr:string __ str w?)
		  str]
		 [else
		  (error 'parse-syscall "Did not match ~a" expr)]))

  (values #f (maybe-create-absolute-path fpath #f cdir table 'parse-chdir-syscall)
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
  
  (values (sc-stat (maybe-create-absolute-path fname #f cdir table 'parse-stat-syscall))
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

  (values (sc-readlink (maybe-create-absolute-path fname #f cdir table 'parse-readlink-syscall))
          #f table))

(define (parse-readlinkat-syscall scall cdir table)
  (define args (args-parser 2 (open-input-string (syscall-args scall))))
  (define fd (car args))
  (define fname (cadr args))
  
  (values (sc-readlink (maybe-create-absolute-path fname fd cdir table 'parse-readlinkat-syscall))
          #f table))

(define (parse-setxattr-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-setxattr (maybe-create-absolute-path fname #f cdir table 'parse-setxattr-syscall))
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
  
  (values (sc-chown (maybe-create-absolute-path fname #f cdir table 'parse-chown-syscall))
          #f table))

(define (parse-chmod-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-chmod (maybe-create-absolute-path fname #f cdir table 'parse-chmod-syscall))
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

  (values (sc-access (maybe-create-absolute-path fname #f cdir table 'parse-access-syscall))
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

  (values (sc-mkdir (maybe-create-absolute-path path #f cdir table 'parse-mkdir-syscall))
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

  (define nlpath (maybe-create-absolute-path lpath #f cdir table 'parse-symlink-syscall))
  (define-values (dir _ __) (split-path nlpath))
  (define ntarget (maybe-create-absolute-path target #f dir table 'parse-symlink-syscall))

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
  (define ntarget (maybe-create-absolute-path target #f dir table 'parse-symlinkat-syscall))

  (values (sc-symlink ntarget nlpath)
          #f table))

(define (parse-rename-syscall scall cdir table)
  (define-values (old new) (parse-rename-args (parse-expression (open-input-string (syscall-args scall)))))

  (values (sc-rename
	   (maybe-create-absolute-path old #f cdir table 'parse-rename-syscall)
	   (maybe-create-absolute-path new #f cdir table 'parse-rename-syscall))
          #f table))

(define (parse-unlink-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
  (values (sc-unlink (maybe-create-absolute-path fname #f cdir table 'parse-unlink-syscall))
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
   
;; convert generic system call into specific system call
;; ignore calls that resulted in an error.
(define (process-syscalls-pid pid cdir_ table_ syscalls sorted-keys)
  (when (debug?)
	(printf "processing syscalls for pid ~a\n" pid))
  (cond
   [(hash-ref syscalls pid #f) =>
    (lambda (scalls)
      (let loop ([cdir cdir_]
		 [table table_]
		 [scs scalls])
	(cond
	 [(empty? scs)
	  '()]
	 [else
	  (define-values (res ndir ntable) (parse-syscall (car scs) cdir table))
	  (cond
	   [(or (equal? res #f) (void? res))
	    (loop (or ndir cdir) ntable (cdr scs))]
	   [(sc-fork? res)
	    (define last-tstamp (cond
				 [(string-contains? pid "_")
				  (second (string-split pid "_"))]
				 [else
				  (error 'process-syscalls-pid "Don't expect to process pid's that don't have unique ids attached.")]))
	    (when (= (string->number (car (string-split pid "_"))) 25640)
		  (printf "sc-fork-pid is ~a\n" (sc-fork-pid res)))
	    (define newpid (get-next-pid (sc-fork-pid res) last-tstamp sorted-keys))
	    (define fork-calls (process-syscalls-pid newpid cdir ntable syscalls sorted-keys))
	    (define recur (loop (or ndir cdir) ntable (cdr scs)))
	    (if (and fork-calls recur)
		(append (cons res fork-calls) recur)
		#f)]
	   [else
	    (define recur (loop (or ndir cdir) ntable (cdr scs)))
	    (if recur
		(cons res recur)
		#f)])])))]
   [else
    #f]))

(define (get-next-pid pid last-tstamp sorted-list-of-keys)
  ;; search list of keys for pid_closet-timestamp-to-last-tstamp

  (define (this-one? k)
    (define ls (string-split k "_"))
    (cond
     [(= 2 (length ls))
      (cond
       [(equal? pid (string->number (car ls)))
        (when (debug?)
	      (printf "pid is equal ~a\n" pid)
	      (printf "last tstamp is ~a\n" last-tstamp)
	      (printf "considering tstamp ~a\n" (cadr ls)))
	(> (string->number (cadr ls))
	   (string->number last-tstamp))]
       [else
	(when (debug?)
	      (printf "pids are not equal ~a\n" (car ls)))
	#f])]
     [else
      (when (debug?)
	    (printf "not correct length\n"))
      #f]))

  (define (helper lok)
    (cond
     [(empty? lok)
      (error 'get-next-pid "Did not find a matching key for ~a ~a." pid last-tstamp)]
     [(this-one? (car lok))
      (car lok)]
     [else
      (helper (cdr lok))]))

  (helper sorted-list-of-keys))

;; will need to expand this.
(define (cond? cmd_)
  ;; does it begin with if or test?
  (define cmd (string-trim cmd_))
  (or (string-prefix? cmd "if") (string-prefix? cmd "test")))

;; determines what the inputs/outputs are.
;; also returns ins and outs for pid's created via fork/clone by pid
(define (process-in-out-pid pid cdir_ syscalls cmd)
  (cond
   [(cond? cmd)
    (values #f #f)]
   [else
    (define sorted-keys (sort (sequence->list (in-hash-keys syscalls))
			      < #:key (lambda (k)
					(define tmp (string-split k "_"))
					(if (= 2 (length tmp))
					    (string->number (cadr tmp))
					    0))))
    (define cdir (resolve-symlinks cdir_))
    (define calls (process-syscalls-pid pid cdir (hash) syscalls sorted-keys))
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
	(values #f #f))]))

        
  




  
