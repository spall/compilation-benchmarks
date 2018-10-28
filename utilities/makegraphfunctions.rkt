#lang errortrace racket/base

(require racket/list
         racket/string
         racket/match
         "makegraph.rkt"
         "flags.rkt"
         "system_calls/process-syscalls.rkt")

(provide get-targets
	 get-last-edge
         create-dotfile-string
         print-all-targets-and-mfiles
         collapse-targets
         work
         span
         parallel-slackness
         predicted-speed-upper
         predicted-speed-lower
	 predicted-speed-perfect-linear
         verify-edges
	 longest-leaf         
         print-graph)

(define (get-last-edge t)
  (cond
    [(empty? (target-out-edges t))
     #f]
    [else
     (car (target-out-edges t))]))

;; return a list of possible targets
(define (get-targets graph tname)
  (define targets (makegraph-targets graph))
  (for/fold ([tgs '()])
            ([val (in-hash-keys targets)])
    (if (equal? (target-name val) tname)
        (cons val tgs)
        tgs)))

(define (leaf-node? node)
  (empty? (target-out-edges node)))

(define (work node)
  (cond
    [(leaf-node? node)
     (leaf-node-work node)]
    [else
     (non-leaf-node-work node)]))

(define (leaf-node-work node)
  (define data (target-data node))
  (cond
    [(rusage-data? data)
     (rusage-data-elapsed data)]
    [(number? data)
     data]
    [else
     (error 'leaf-node-work "Unrecognized data ~a ~a" data node)]))

(define (non-leaf-node-work node)
  (for/sum ([e (reverse (target-out-edges node))])
    (define n (edge-end e))
    (work n)))

;; Span(root) = longest time down deps path + sum of times of recipes paths
(define (span node)
  (cond
    [(leaf-node? node)
     (leaf-node-span node)]
    [else
     (non-leaf-node-span node)]))

(define (leaf-node-span node)
  (define data (target-data node))
  (cond
    [(rusage-data? data)
     (rusage-data-elapsed data)]
    [(number? data)
     data]
    [else
     (error 'leaf-node-span "Unrecognized data ~a" data)]))

(define (non-leaf-node-span node)
  (define-values (m s)
    (for/fold ([max_ 0]
               [sum 0])
              ([e (reverse (target-out-edges node))])
      (define n (edge-end e))
      (define tmpspan (span n))
      
      (cond
        [(equal? 'dep (edge-type e))
         (values (max max_ tmpspan) sum)]
        [else
         (values max_ (+ sum tmpspan))])))
  (+ m s))


(define (longest-target root_ graph)
  (void)) ;; todo

(define (longest-leaf graph)
  (void))

(define (longest-recipe root_ graph)
  (void))

;; factor by which the parallelism of the computation exceeds the number of processors
(define (parallel-slackness graph pcount)
  (define work_ (work (makegraph-root graph)))
  (define span_ (span (makegraph-root graph)))
  (exact->inexact (/ work_ (* pcount span_))))

#|
   brent's law 
   Should be an UPPER BOUND on the amount of time to execute work with
   pcount processors.
|#
(define (predicted-speed-upper graph pcount [work_ #f] [span_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph))))
  (unless span_
    (set! span_ (span (makegraph-root graph))))

  (exact->inexact (+ span_ (/ work_ pcount))))

(define (predicted-speed-lower graph pcount [work_ #f] [span_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph))))
  (unless span_
    (set! span_ (span (makegraph-root graph))))
  (define speed (exact->inexact (/ work_ pcount)))

  (if (< speed span_)
      span_
      speed))

(define (predicted-speed-perfect-linear graph pcount [work_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph))))
  (exact->inexact (/ work_ pcount)))

(define (print-all-targets-and-mfiles graph name)
  (define targets (hash-keys (makegraph-targets graph)))
  (let loop ([ts targets])
    (unless (empty? ts)
      (when (equal? name (mcar (car ts)))
        (printf "target: ~a; mfile: ~a\n\n" (mcar (car ts)) (mcdr (car ts))))
      (loop (cdr ts)))))

(define (collapse-targets graph)
  (define targets (makegraph-targets graph))
  (for ([t (in-hash-keys targets)])
    (set-target-out-edges! t
      (let loop ([es (target-out-edges t)])
        (cond
	  [(empty? es)
	   '()]
	  [(member (car es) (cdr es))
	   (loop (cdr es))]
	  [else
	   (cons (car es) (loop (cdr es)))])))))

(define (verify-edges graph)

  (for ([t (in-hash-keys (makegraph-targets graph))])
    (define es (make-hash))
    (unless (empty? (target-out-edges t))
      (hash-set! es (car (target-out-edges t)) #t)
      (for/fold ([last (edge-id (car (target-out-edges t)))])
                ([e (cdr (target-out-edges t))])
        (when (hash-ref es e #f)
          (printf "Target <~a,~a> has more than one copy of edge ~a\n" (target-name t) (target-mfile t) (edge-id e)))
        (hash-set! es e #t)
        (when (< (edge-id e) last)
          (printf "edge id ~a is less than last edge id ~a\n" (edge-id e) last))
        (edge-id e)))))
    
;; ------------------------ graphviz dot file --------------------------------

(define child-color "red") ;"blue")
(define dep-color "green") ;"red")

;; returns a string represting a graph in the dot language
(define (create-dotfile-string g)
  (define targets (makegraph-targets g))
  (define (helper v targets color)
    (lambda (c)
      (define t (hash-ref targets c (lambda ()
                                      (error 'create-dotfile "Failed to find ~a among graph's targets" c))))
      (format "\"~a~a\" -> \"~a~a\" [color=~a];\n" (target-name v) (target-id v)
              (target-name t) (target-id t) color)))
      
  (define (create-dotfile-edges v)
     (map (lambda (c i)
            (define t (hash-ref targets c (lambda ()
                                            (error 'create-dotfile "Failed to find ~a among graph's targets" c))))
            (format "\"~a~a\" -> \"~a~a\" [label=~a, color=~a];\n" (target-name v) (target-id v)
                    (target-name t) (target-id t) i child-color))
          (reverse (target-out-edges v))
          (range 1 (+ 1 (length (target-out-edges v))))))
     
  (apply string-append
         (cons "strict digraph {\n"
               (append (for/fold ([accu '()])
                                 ([val (in-hash-keys targets)])
                         (append accu
                                 (create-dotfile-edges val)))
                       (list "}\n")))))
;; ----------------------------------------------------------------------
            
         

(define (print-graph graph)
  (for ([n (in-hash-keys (makegraph-targets graph))])
    (printf "Processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))
    (printf "target-out-edges: ~a\n" (target-out-edges n))
    (for ([e (target-out-edges n)])
      (let ([tmp (edge-end e)])
        (printf "Dependency edge between <~a,~a,~a> and <~a,~a,~a> with ID ~a\n\n"
                (target-name n) (target-mfile n) (target-id n)
                (target-name tmp) (target-mfile tmp) (target-id tmp) (edge-id e))))
    
    (printf "Finished processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))))
  
