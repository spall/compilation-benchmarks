#lang errortrace racket

(require c/parse
	 c/ast)

(provide process-syscalls-pid
         process-in-out-pid
         process-syscalls
         (struct-out syscall))

(struct syscall (name args retval))
(struct sc-open (file read? write? retval))
(struct sc-execve (file))

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
   [(expr:begin _ (expr:ref __ (id:var src name)) (expr:string src str w?))
    (values name str '())]
   [(expr:begin _ left right)
    (if (expr:int? right) ;; right is mode which we do not care about
    	(parse-openat-args left)
	(call-with-values (lambda () (parse-openat-args left))
			  (lambda (a b c)
			    (values a b (parse-flags right)))))]
   [else
    (error 'parse-openat-args "Did not match ~a" args)]))

#;(define (parse-execve-args args)
  (match args
   [(expr:begin _ (expr:begin __ (expr:string a str w?) ))]))

(define (parse-syscall scall)
  (match (syscall-name scall)
    ["open"
     (unless (string-prefix? (syscall-retval scall) "-1")
       (define expr (parse-expression (open-input-string (syscall-args scall))))
       (define-values (fname flags)
         (parse-open-args expr))

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

       (sc-open fname read? write? (string->number (syscall-retval scall))))]
    #;["openat"
     (unless (string-prefix? (syscall-retval scall) "-1")
       (define expr (parse-expression (open-input-string (syscall-args scall))))
       (define-values (dirfd fname flags)
         (parse-openat-args expr))

       ;; TODO: Not sure how to do general case
       
     )]
    ["execve" 
     (unless (string-prefix? (syscall-retval scall) "-1")
       ;; c parser doesn't seem compatible with this
       (define fname (read (open-input-string (string-trim (string-trim (syscall-args scall) "(") ")"))))
       (sc-execve "fname"))]
    ;[]
    ;[]
    ;;...
    [else #f]))

;; Begin by processing open system call
;; ignoring open calls that resulted in an error.
(define (process-syscalls-pid pid syscalls)
  (cond
    [(hash-ref syscalls pid #f) =>
     (lambda (scalls)
        (for/fold ([ls '()])
                  ([scall scalls])
          (define res (parse-syscall scall)) 
          (if (or (equal? res #f)
                  (void? res))
              ls
              (cons res ls))))]
    [else
     #f]))

(define (process-syscalls syscalls)
  (define processed (make-hash))
  (for ([(pid scalls) (in-hash syscalls)])
    (hash-set! processed pid (for/fold ([ls '()])
    	       		     	       ([scall scalls])
			       (define res (parse-syscall scall))
			       (if (or (equal? res #f) (void? res))
			       	   ls
				   (cons res ls)))))
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
      [(sc-open f r? w? r)
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


        
  




  
