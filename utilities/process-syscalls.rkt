#lang errortrace racket

(require c/parse
	 c/ast)

(provide process-syscalls-pid
         process-in-out-pid
         process-syscalls
         (struct-out syscall))

(struct syscall (name args retval))
(struct sc-open (dir file read? write? retval))
(struct sc-execve (file))

(define files-to-ignore (list "/dev/tty")) ;; add more here

(define (parse-flags bops)
  (match bops
   [(expr:binop _ l op r)
    (append (parse-flags l) (parse-flags r))]
   [(expr:ref _ (id:var __ name))
    (list name)]
   [else
    (error 'parse-flags "Did not match ~a" bops)]))

(define (parse-open-args args)
   (match args
   [(expr:begin _ (expr:string __ str w?) right)
    (values str (parse-flags right))]
   [(expr:begin _ left right) ;; right is mode which we do not care about
    (parse-open-args left)]
   [else
    (error 'parse-open-args "Did not match ~a" args)]))

(define (parse-openat-args args)
  (match args
   [(expr:begin _ (expr:int __ val q) (expr:string src str w?))
    (values val str '())]
   [(expr:begin _ (expr:ref __ (id:var src name)) (expr:string src2 str w?))
    (values name str '())]
   [(expr:begin _ left right)
    (if (expr:int? right) ;; right is mode which we do not care about
    	(parse-openat-args left)
	(call-with-values (lambda () (parse-openat-args left))
			  (lambda (a b c)
			    (values a b (parse-flags right)))))]
   [else
    (error 'parse-openat-args "Did not match ~a" args)]))

(define (parse-syscall scall cdir table)
 (if (string-prefix? (syscall-retval scall) "-1")
     (values #f #f table)
   (match (syscall-name scall)
    ["open"
     (define fd (string->number (syscall-retval scall)))
     (define expr (parse-expression (open-input-string (syscall-args scall))))
     (define-values (fname flags) (parse-open-args expr))

     (define dir (if (absolute-path? fname)
		     #f
		     cdir))

     ;; Figure out if this is a read or write, or both
     (define-values (read? write?)
       (for/fold ([read? #f] [write? #f])
       		 ([f flags])
         (match f
	  ['O_RDONLY
	   (values #t write?)]
	  ['O_WRONLY
	   (values read? #t)]
	  ['O_RDWR
	   (values #t #t)]
	  ['O_CREAT
	   (values read? #t)]
	  ['O_EXCL
	   (values read? #t)]
	  ['O_APPEND
	   (values #t #t)]
	  ['O_TRUNC            ;; behavior of trunc with rdonly is undefined....
	   (values #f #t)]     ;; say reading is false. may be a lie?
	   ;; add more here
	  [else
	   (values read? write?)])))

     ;; Can't necessarily distinguish between file and directory so add to table regardless; if it isn't a directory then
     ;; a later call that uses this fd as a directory should fail.
     (define full-path (if dir (path->complete-path fname dir)
     	     	       	       fname))

     (if (member fname files-to-ignore)
     	 (values #f #f table)	      
         (values (sc-open dir fname read? write? fd) #f (hash-set table fd full-path)))]
    ["openat"
     (define fd (string->number (syscall-retval scall)))
     (define expr (parse-expression (open-input-string (syscall-args scall))))
     (define-values (dirfd fname flags)
            (parse-openat-args expr))
     ;; 1. Let's do the absolute path case
     (define dir (cond
               	  [(absolute-path? fname)
		   #f]
		  [(equal? 'AT_FDCWD dirfd)
		   cdir]
		  [else ;; TODO: general case
		   (hash-ref table dirfd (lambda () (error 'parse-syscall "Did not find file descriptor ~a in table" dirfd)))
		  ]))

     ;; Figure out if this is a read or write, or both
     (define-values (read? write?)
       (for/fold ([read? #f] [write? #f])
       		 ([f flags])
         (match f
	  ['O_RDONLY
	   (values #t write?)]
	  ['O_WRONLY
	   (values read? #t)]
	  ['O_RDWR
	   (values #t #t)]
	  ['O_CREAT
	   (values read? #t)]
	  ['O_EXCL
	   (values read? #t)]
	   ;; add more here
	  [else
	   (values read? write?)])))

     ;; same as previous case I believe
     (define full-path (if dir
     	     	       	   (path->complete-path fname dir)
			   fname))

     (values (sc-open dir fname read? write? fd) #f (hash-set table fd full-path))]
    ["execve" 
     ;; c parser doesn't seem compatible with this
     (define fname (read (open-input-string (string-trim (string-trim (syscall-args scall) "(") ")"))))
     (values (sc-execve "fname") #f table)]
    ["chdir"
     (define expr (parse-expression (open-input-string (syscall-args scall))))
     (define fpath (match expr
     	     	    [(expr:string __ str w?)
		     str]
		    [else
		     (error 'parse-syscall "Did not match ~a" expr)]))
     (values #f fpath table)]
    ["fchdir"
     (define expr (parse-expression (open-input-string (syscall-args scall))))
     (define fdir (match expr
     	     	   [(expr:int __ val q)
		    val]
		   [else
		    (error 'parse-syscall "Did not match ~a" expr)]))
     (values #f (hash-ref table fdir (lambda () (error 'parse-syscall "Did not find file descriptor ~a in table" fdir))) table)]
    ;; add more here	  
    [else (values #f #f table)])))


;; ignore calls that resulted in an error.
(define (process-syscalls-pid pid syscalls)
  (cond
    [(hash-ref syscalls pid #f) =>
     (lambda (scalls)
        (define-values (calls _ __)
	  (for/fold ([ls '()]
		     [cdir  #f]
		     [table (hash)])
                    ([scall scalls])
          (define-values (res ndir ntable) (parse-syscall scall cdir table)) 
          (if (or (equal? res #f)
                  (void? res))
              (values ls (and ndir cdir) ntable)
              (values (cons res ls) (and ndir cdir) ntable))))

	  calls)]
    [else
     #f]))

(define (process-syscalls syscalls)
  (define processed (make-hash))
  (for ([(pid scalls) (in-hash syscalls)])
    (define res (process-syscalls-pid pid syscalls))
    (unless res
      (error 'process-syscalls "No syscalls for pid ~a" pid))
    
    (hash-set! processed pid res))
  processed)

;; accepts a list of sc-* structures
;; determines what the inputs/outputs are.
;; for example: (open "f" 1)   would currently mark "f" as an input and an output. temporarily
(define (process-in-out-pid pid syscalls)
  (define calls (process-syscalls-pid pid syscalls))
  (for/fold ([in '()]
             [out '()])
            ([call calls])
    (match call
      [(sc-open d f r? w? r)
       (values (if (and r? (not (member f in)))
       	           (cons f in)
		   in)
	       (if (and w? (not (member f out)))
	           (cons f out)
		   out))]
      [(sc-execve f)
       (values (if (member f in)
       	       	   in
		   (cons f in)) out)]
      [else
       (values in out)])))

;; compares contents of outs and ins; returns the intersection
(define (shared-depencencies outs ins)
  (filter (lambda (l)
            (member l ins)) outs))


        
  




  
