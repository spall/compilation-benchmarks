#lang errortrace racket/base

(require "makegraph.rkt"
         racket/list
         racket/system)
(provide run-graph-sequential)

;; graph -> error?
(define (run-graph-sequential graph)

  (run-target-sequential (makegraph-root graph)))

(define (separate-edges edges)
  (cond
    [(empty? edges)
     (values '() '())]
    [else
     (call-with-values (lambda () (separate-edges (cdr edges)))
                       (lambda (ds rs)
                         (if (equal? (edge-type (car edges)) 'dep)
                             (values (cons (car edges) ds) rs)
                             (values ds (cons (car edges) rs)))))]))

(define (run-target-sequential t)
  ;; target has dependencies
  ;; add all dependencies to queue.
  ;; when last dependency terminates

  ;; how to tell when last dependency terminates
  
  ;; add series of sequential tasks to queue.
  (cond
    [(empty? (target-out-edges t))
     (run-leaf-sequential t)]
    [else
     (define-values (deps recipes)
       (separate-edges (target-out-edges t)))
     
     (for ([d (shuffle deps)]) ;; run these in strange order
       (run-target-sequential (edge-end d)))
     
     (for ([r recipes])
       (run-target-sequential (edge-end r)))]))

;; what about current directory
(define (run-leaf-sequential t)
  ;; store directory in rusage data; so
  ;; so set directory before running command
  (current-directory (rusage-data-dir (target-data t)))
  (if (system (rusage-data-cmd (target-data t)) #:set-pwd? #t)
      (printf "CMD: ~a succeeded.\n" (rusage-data-cmd (target-data t)))
      (printf "CMD: ~a failed.\n" (rusage-data-cmd (target-data t)))))

