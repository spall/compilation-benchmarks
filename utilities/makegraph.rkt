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
         add-edge
         remove-edge
         all-fields-set?
         add-target-to-makegraph
         target-in-graph?)

(define EDGE 0) ;; edge counter. to create ordering on edges
(define (get-edge-id)
  (begin0 EDGE
    (set! EDGE (- EDGE 1))))
(define TID 0)
(define (get-tid)
  (begin0 TID
    (set! TID (+ 1 TID))))

;; ----------- Node to represent makefile target ------------------
(define (target-print t port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))
  
  (write (target-id t) port)
  (write-string ", " port)
  (write (target-name t) port)
  (write-string ", " port)
  (write (target-mfile t) port)
  (write-string "\n" port)
  (write-string "Dependencies and recipes:\n" port)
  (for ([d (target-out-edges t)])
    (recur d port)
    (write-string "\n" port))
  (write-string "In edges:\n" port)
  (for ([r (target-in-edges t)])
    (recur r port)
    (write-string "\n" port))
  (when mode (write-string ">" port)))

;; out edges are recipes and deps; in-edges are edges where this target is the end.
(struct target (id name mfile out-edges in-edges)
  #:methods gen:custom-write
  [(define write-proc target-print)]
  #:mutable #:transparent)

(define (edge-print e port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))

  (write (edge-id e) port)
  (write-string "\n" port)
  (write-string "End: " port)
  (recur (edge-end e) port)
  (write-string "\n" port)
  (write-string "Data:\n" port)
  (for ([d (edge-data e)])
    (recur d port)
    (write-string "\n" port))
  (when mode (write-string ">" port)))

(struct edge (end data type id) ;; end is name?
  #:methods gen:custom-write
  [(define write-proc edge-print)]
  #:mutable #:transparent)

;; mfile children data mdata remake? time
(struct node (id name deps) #:mutable #:transparent)

(define (create-target name)
  (target (get-tid)
          name
          #f  ; mfile
          '() ; out-edges
          '())) ; in-edges

(define (insert-edge e ls)
  (cond
    [(empty? ls)
     (list e)]
    [(< (edge-id e) (edge-id (car ls)))
     (cons e ls)]
    [else
     (cons (car ls)
           (insert-edge e (cdr ls)))]))

(define (remove-edge t e direction)
  (define (helper ls)
    (cond
      [(empty? ls)
       ls]
      [(equal? e (car ls))
       (cdr ls)]
      [else
       (cons (car ls) (helper (cdr ls)))]))
  
  (if (equal? direction 'out)
      (set-target-out-edges! t (helper (target-out-edges t)))
      (set-target-in-edges! t (helper (target-in-edges t)))))

(define (add-edge t e direction [t2 #f])
  (when t2
    (set-edge-type! e t2))

  (if (equal? direction 'out)
      (set-target-out-edges! t (insert-edge e (target-out-edges t)))
      (set-target-in-edges! t (insert-edge e (target-in-edges t)))))

;; adds a recipe edge
(define (add-recipe t recipe-t info)
  (define tmp (edge recipe-t info 'seq (get-edge-id)))
  (set-target-in-edges! recipe-t (cons tmp (target-in-edges recipe-t)))
  (set-target-out-edges! t (cons tmp (target-out-edges t))))

;; adds a dependency edge
(define (add-dependency t dep-t info)
  (define tmp (edge dep-t info 'dep (get-edge-id)))
  (set-target-in-edges! dep-t (cons tmp (target-in-edges dep-t)))
  (set-target-out-edges! t (cons tmp (target-out-edges t))))

;; ----------------- End of target code ------------------------------

;; ----------------- graph to represent makefile graph ---------------
(define (makegraph-print mg port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))
  (write-string "Root: " port)
  (recur (makegraph-root mg) port)
  (write-string "\n" port)
  (write-string "Targets: \n" port)
  (for ([t (makegraph-targets mg)])
    (recur t port)
    (write-string "\n"))
  (when mode (write-string ">" port)))

(struct makegraph (targets root)
  #:methods gen:custom-write
  [(define write-proc makegraph-print)]
  #:mutable #:transparent)

(define (create-makegraph)
  (makegraph (make-hash) #f))

(define (add-target-to-makegraph graph t)
  (hash-set! (makegraph-targets graph) t #t))

(define (target-in-graph? graph t)
  (hash-ref (makegraph-targets graph) t #f))
 
;; -------------- end make graph code --------------------------

;; -------------- structure to represent rusage data -----------

(define (rusage-data-print rd port mode)
  (when mode (write-string "<" port))
  (write-string "todo" port)
  (when mode (write-string ">" port)))

(struct rusage-data (cmd rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata submake?)
  #:methods gen:custom-write
  [(define write-proc rusage-data-print)]
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data cmd #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (all-fields-set? rds)
  (andmap identity
          (reverse (cdr (reverse (struct->list rds))))))

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

