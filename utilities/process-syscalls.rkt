#lang racket

(require parser-tools/lex
	 (prefix-in : parser-tools/lex-sre)
	 c/parse
	 c/ast)

(provide process-in-out-pid
	 (struct-out syscall))


(struct syscall (name args retval))
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
(struct sc-symlink (name))
(struct sc-rename (old new))
(struct sc-unlink (name))
(struct sc-fork (pid))

(define files-to-ignore (list "/dev/tty")) ;; add more here

(define (lookup-file-descriptor table fd func)
  (hash-ref table fd (lambda () fd)))

(define (lookup-file-descriptor-error table fd func)
  (hash-ref table fd (lambda ()
                       (error func "Did not find file descriptor ~a" fd))))

(define (maybe-create-absolute-path fname fd cdir table func)
  (cond
   [(absolute-path? fname)
    fname]
   [(equal? fd "AT_FDCWD")
    (path->complete-path fname cdir)]
   [else
    (path->complete-path fname  (lookup-file-descriptor-error table fd func))]))

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

     (if (member full-path files-to-ignore)
     	 (values #f #f table)	      
         (values (sc-open full-path read? write? fd) #f (hash-set table fd full-path))))

(define (parse-openat-syscall scall cdir table)
  (define fd (string->number (string-trim (syscall-retval scall))))

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
  
  (values (sc-execve fname) #f table))

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
  (define fd (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
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
       [else
        (values #f #f table)])]                  
     [(expr:begin _ left right)
      (driver left (cons right rest))]
     [else
      (error 'parse-fcntl-args "Did not match ~a" args)]))
  (driver args_ '()))

(define (parse-fcntl-syscall scall cdir table)
  (define expr (parse-expression (open-input-string (syscall-args scall))))
  (parse-fcntl-args expr table (syscall-retval scall))) 

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
  (define fd (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
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
  
  (values (sc-chown fname) #f table))

(define (parse-chmod-syscall scall cdir table)
  (define fname (car (args-parser 1 (open-input-string (syscall-args scall)))))

  (values (sc-chmod (if (absolute-path? fname)
   	   	     	fname
			(path->complete-path fname cdir)))
	   #f table))

(define (parse-fchmod-syscall scall cdir table)
  (define fd (car (args-parser 1 (open-input-string (syscall-args scall)))))
  
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

  (values (sc-access fname) #f table))   

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
    str2]
   [else
    (error 'parse-symlink-args "Did not match ~a" args)]))

(define (parse-symlink-syscall scall cdir table)
  (define lpath (parse-symlink-args (parse-expression (open-input-string (syscall-args scall)))))

  (values (sc-symlink (if (absolute-path? lpath)
  	  	      	  lpath
			  (path->complete-path lpath cdir)))
          #f table))

(define (parse-rename-args args)
  (match args
   [(expr:begin _ (expr:string __ str1 w?) (expr:string src str2 w2?))
    (values str1 str2)]
   [else
    (error 'parse-rename-args "Did not match ~a" args)]))

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

(define (dispatch scall)
  (match (syscall-name scall)
    ["open"   parse-open-syscall]
    ["openat" parse-openat-syscall]

    ["chown" parse-chown-syscall]

    ["chmod"    parse-chmod-syscall]
    ["fchmod"   parse-fchmod-syscall]
    ["fchmodat" parse-fchmodat-syscall]

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
    
    ["rename" parse-rename-syscall]

    ["unlink" parse-unlink-syscall]
    ["unlinkat" parse-unlinkat-syscall]

    ["fork" parse-fork-syscall]
    ["clone" parse-fork-syscall]
    ;; add more here
    [else
     (lambda (scall cdir table)
       (values #f cdir table))]))

(define (inputs-outputs scall)
  (match scall
   [(sc-open f r? w? r)
    (values (if r? (list f) '())
	    (if w? (list f) '()))]
   [(sc-execve f) ;; input
    (values (list f) '())]
   [(sc-stat f) ;; input
    (cond
     [(or (equal? 0 f) (equal? 1 f) (equal? 2 f))
      (values '() '())]
     [else
      (values (list f) '())])]
   [(sc-chown f) ;; input and output
    (values (list f) (list f))]
   [(sc-access f) ;; input
    (values (list f) '())]
   [(sc-mkdir d) ;; output
    (values '() (list d))]
   [(sc-chmod f) ;; input and output
    (cond
     [(or (equal? 0 f) (equal? 1 f) (equal? 2 f))
      (values '() '())]
     [else
      (values (list f) (list f))])]
   [(sc-readlink f)
    (values (list f) '())]
   [(sc-fcntl v fl) ;; fl should be o_append; only option now
    (cond
     [(or (equal? 0 v) (equal? 1 v) (equal? 2 v))
      (values '() '())]
     [else
      (values (list v) (list v))])]	
   [(sc-setxattr f)
    (values (list f) (list f))]
   [(sc-symlink f)
    (values '() (list f))]
   [(sc-rename o n)
    (values (list o) (list n))]
   [(sc-unlink f)
    (values (list f) (list f))]
   [else
    (values '() '())]))

(define (parse-syscall scall cdir table)
 (if (string-prefix? (syscall-retval scall) "-1")
     (values #f cdir table)
     ((dispatch scall) scall cdir table)))

;; convert generic system call into specific system call
;; ignore calls that resulted in an error.
(define (process-syscalls-pid pid cdir_ table_ syscalls)
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
	        (define fork-calls (process-syscalls-pid (sc-fork-pid res) cdir ntable syscalls))
		(define recur (loop (or ndir cdir) ntable (cdr scs)))
		(if (and fork-calls recur)
		    (append (cons res fork-calls)
		    	    recur)
		    #f)]
               [else
	        (define recur (loop (or ndir cdir) ntable (cdr scs)))
		(if recur
		    (cons res recur)
		    #f)])])))]
    [else
     #f]))

;; determines what the inputs/outputs are.
;; also returns ins and outs for pid's created via fork/clone by pid
;; for example: (open "f" 1)   would currently mark "f" as an input and an output. temporarily
(define (process-in-out-pid pid cdir syscalls)
  (define calls (process-syscalls-pid pid cdir (make-hash) syscalls))
  (if calls
    (for/fold ([in '()]
               [out '()])
              ([call calls])

      (define-values (is os) (inputs-outputs call))

      (values (append (filter (lambda (x)
     	     	     	        (not (member x in))) is)
	              in)
	      (append (filter (lambda (x)
	     	     	        (not (member x out))) os)
		      out)))
    (values #f #f)))

        
  




  
