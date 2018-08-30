#lang racket

(provide process-syscalls-pid
         process-in-out-pid
         process-syscalls
         (struct-out syscall))

(struct syscall (name args retval))
(struct sc-open (file retval))

(define (parse-syscall scall)
  (match (syscall-name scall)
    ["open"
     (unless (string-prefix? (syscall-retval scall) "-1")
       (define f (read (open-input-string
                        (string-trim
                         (string-trim (syscall-args scall) "(")
                         ")"))))
       (sc-open f (string->number (syscall-retval scall))))]
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
    (hash-set! processed pid (process-syscalls-pid pid scalls)))
  make-hash)

;; accepts a list of sc-* structures
;; determines what the inputs/outputs are.
;; for example: (open "f" 1)   would currently mark "f" as an input and an output. temporarily
(define (process-in-out-pid pid syscalls)
  (define calls (process-syscalls-pid pid syscalls))
  (for/fold ([in '()]
             [out '()])
            ([call calls])
    (match call
      [(sc-open f r)
       (values (cons f in) (cons f out))]
      [else
       (values in out)])))

;; compares contents of outs and ins; returns the intersection
(define (shared-depencencies outs ins)
  (filter (lambda (l)
            (member l ins)) outs))


        
  




  
