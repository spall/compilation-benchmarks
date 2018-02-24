#lang racket

(require racket/struct)

(provide (struct-out target)
         (struct-out makegraph)
         (struct-out rusage-data)

         create-makegraph
         create-target
         create-rusage-data
         add-data-to-target!
         add-mdata-to-target!
         add-child
         add-dep
         all-fields-set?
         add-target-to-makegraph
         target-in-graph?
         get-target)

(define tid 0)
(define (get-tid)
  (begin0 tid
    (set! tid (+ 1 tid))))

(struct target (id name mfile deps children data mdata remake?) #:mutable #:transparent)

(define (create-target name)
  (target (get-tid)
          name
          #f  ; mfile
          '() ; dependencies
          '() ; children (strands) (run sequentially). children are stored in reverse order.
              ;; first child in list is last child run. 
          '() ; rusage data
          '() ; rusage data for recursive make calls
          #f)) ; remake?

(define (add-data-to-target! t data)
  (set-target-data! t (append (target-data t) (list data))))

(define (add-mdata-to-target! t mdata)
  (set-target-mdata! t (append (target-mdata t) (list mdata))))

(define (add-child t child)
  (set-target-children! t (cons child (target-children t))))

(define (add-dep t dep)
  (set-target-deps! t (cons dep (target-deps t))))

(struct makegraph (targets root) #:mutable #:transparent)

(define (create-makegraph)
  (makegraph (make-hash) #f))

(define (add-target-to-makegraph graph tid t)
  (hash-set! (makegraph-targets graph) tid t))

(define (target-in-graph? graph tid)
  (if (hash-ref (makegraph-targets graph) tid #f)
      #t
      #f))

(define (get-target graph tid)
  (hash-ref (makegraph-targets graph) tid #f))

(struct rusage-data (cmd rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata)
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data cmd #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (all-fields-set? rds)
  (andmap identity
          (struct->list rds)))
