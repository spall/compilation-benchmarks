#lang racket/base

(require racket/list
	 "makegraph.rkt")

(provide leaf?
	 intersection
	 work-of-target)

(define (leaf? node)
  (empty? (target-out-edges node)))

(define (intersection ls1 ls2)
  (cond
   [(empty? ls1)
    '()]
   [(member (car ls1) ls2)
    (cons (car ls1) (intersection (cdr ls1) ls2))]
   [else
    (intersection (cdr ls1) ls2)]))


(define (work-of-target tid graph [subs (hash)] [print? #f])
  ;; for each edge call work of target
  (define t (get-target graph tid))
  (cond
   [(hash-ref subs tid #f) =>
    (lambda (t_)
      t_)]
   [(empty? (target-out-edges t)) ;; leaf
    (define data (target-data t))
    (cond
     [(rusage-data? data)
      (rusage-data-elapsed data)]
     [(number? data)
      data]
     [else
      0])]
   [else
    (when print?
	  (printf "Target <~a,~a>: \n\n" (target-name t) (target-mfile t)))
    (define tmp (for/sum ([e (target-out-edges t)])
			 (define t2 (get-target graph (edge-end e)))
			 (define tmp (work-of-target t2 graph))
			 (when print?
			       (printf "Target <~a,~a> is ~a and it's work is ~a.\n" (target-name t2) (target-mfile t2)
				       (if (equal? 'dep (edge-type e))
					   "Dependency"
					   "Recipe") tmp)
			       tmp)))
    (when print?
	  (printf "Done with target <~a,~a> \n\n" (target-name t) (target-mfile t)))
    tmp]))
