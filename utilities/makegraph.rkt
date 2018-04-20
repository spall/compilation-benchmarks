#lang racket

(require racket/struct)

(provide (struct-out target)
         (struct-out edge)
         (struct-out makegraph)
         (struct-out rusage-data)
         (struct-out mnode)
         (struct-out mgraph)

         create-mnode
         create-mgraph

         create-makegraph
         create-target
         create-rusage-data
         add-recipe
         add-dependency
         all-fields-set?
         add-target-to-makegraph
         target-in-graph?
         get-target)

(define EDGE 0) ;; edge counter. to create ordering on edges
(define (get-edge-id)
  (begin0 EDGE
    (set! EDGE (- EDGE 1))))
(define TID 0)
(define (get-tid)
  (begin0 TID
    (set! TID (+ 1 TID))))

;; ----------- Node to represent makefile target ------------------
(struct target (id name mfile deps recipes) #:mutable #:transparent)
(struct edge (end data type id) #:mutable #:transparent)
;; end is name?

;; mfile children data mdata remake? time
(struct node (id name deps) #:mutable #:transparent)

(define (create-target name)
  (target (get-tid)
          name
          #f  ; mfile
          '() ; dependencies
          '())) ; recipes

;; adds a recipe edge
(define (add-recipe t recipe info)
  (set-target-recipes! t (cons (edge recipe info 'seq (get-edge-id))
                               (target-recipes t))))

;; adds a dependency edge
(define (add-dependency t dep info)
  (set-target-deps! t (cons (edge dep info 'dep (get-edge-id))
                            (target-deps t))))

;; ----------------- End of target code ------------------------------

;; ----------------- graph to represent makefile graph ---------------

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

;; -------------- end make graph code --------------------------

;; -------------- structure to represent rusage data -----------

(struct rusage-data (cmd rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata)
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data cmd #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (all-fields-set? rds)
  (andmap identity
          (struct->list rds)))

;; ---------------- end of rusage data code --------------------

;; ---------------- node for racket modules --------------------

(struct mnode (id name deps time) #:mutable #:transparent)

(define (create-mnode name deps)
  (mnode (get-tid) name deps #f))

;; ---------------- end of module node code --------------------


;; ---------------- graph for racket modules -------------------

(struct mgraph (nodes roots) #:mutable #:transparent)

(define (create-mgraph)
  (mgraph (make-hash) (make-hash)))

;; ---------------- end of racket module graph code ------------

