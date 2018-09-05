#lang errortrace racket

(require c/parse
	 c/ast)

(provide process-syscalls-pid
         process-in-out-pid
         process-syscalls
         (struct-out syscall))

(struct syscall (name args retval))
(struct sc-open (file read? write? retval))

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
   [(expr:begin _ left right)
    (parse-open-args left)]
   [else
    (error 'parse-open-args "Did not match ~a" args)]))

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
    ;["openat" ]
    ;["write" ]
    ;["read" ]
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
       (values (if r?
       	           (cons f in)
		   in)
	       (if w?
	           (cons f out)
		   out))]
      [else
       (values in out)])))

;; compares contents of outs and ins; returns the intersection
(define (shared-depencencies outs ins)
  (filter (lambda (l)
            (member l ins)) outs))


        
  




  
