#lang racket

(require racket/struct)

(provide (struct-out target)
         (struct-out makegraph)
         (struct-out rusage-data)

         create-makegraph
         create-target
         create-rusage-data
         add-data-to-target!
         add-child
         add-dep
         all-fields-set?
         add-target-to-makegraph
         target-in-graph)

(define tid 0)
(define (get-tid)
  (begin0 tid
    (set! tid (+ 1 tid))))

(struct target (id name deps children data remake?) #:mutable #:transparent)

(define (create-target name)
  (target (get-tid) name '() '() '() #f))

(define (add-data-to-target! t data)
  (set-target-data! t (append (target-data t) (list data))))

(define (add-child t child)
  (set-target-children! t (cons child (target-children t))))

(define (add-dep t dep)
  (set-target-deps! t (cons dep (target-deps t))))

(struct makegraph (targets root) #:mutable #:transparent)

(define (create-makegraph)
  (makegraph '() #f))

(define (add-target-to-makegraph graph t)
  (set-makegraph-targets! graph (cons t (makegraph-targets graph))))

;; returns #f or target
(define (target-in-graph graph tname)
  (let loop ([targets (makegraph-targets graph)])
    (cond
      [(empty? targets)
       #f]
      [(equal? tname (target-name (car targets)))
       (car targets)]
      [else
       (loop (cdr targets))])))

(struct rusage-data (cmd rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata)
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data cmd #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (all-fields-set? rds)
  (andmap identity
          (struct->list rds)))
