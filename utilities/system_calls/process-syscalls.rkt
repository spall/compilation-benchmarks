#lang errortrace racket

(require c/parse
	 c/ast)

(require "system_calls.rkt")

(provide process-syscalls-pid
         process-in-out-pid)

(define (parse-syscall scall cdir table)
 (if (string-prefix? (syscall-retval scall) "-1")
     (values #f cdir table)
     ((dispatch scall) scall cdir table)))


;; ignore calls that resulted in an error.
(define (process-syscalls-pid pid cdir_ syscalls)
  (cond
    [(hash-ref syscalls pid #f) =>
     (lambda (scalls)
        (define-values (calls _ __)
	  (for/fold ([ls '()]
		     [cdir  cdir_]  ;; todo need a better value than #f
		     [table (hash)])
                    ([scall scalls])
          (define-values (res ndir ntable) (parse-syscall scall cdir table))
          (if (or (equal? res #f) (void? res))
              (values ls (or ndir cdir) ntable)
              (values (cons res ls) (or ndir cdir) ntable))))
	  calls)]
    [else
     #f]))

;; accepts a list of sc-* structures
;; determines what the inputs/outputs are.
;; for example: (open "f" 1)   would currently mark "f" as an input and an output. temporarily
(define (process-in-out-pid pid cdir syscalls)
  (define calls (process-syscalls-pid pid cdir syscalls))
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

;; compares contents of outs and ins; returns the intersection
(define (shared-depencencies outs ins)
  (filter (lambda (l)
            (member l ins)) outs))


        
  




  
