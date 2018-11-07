#lang errortrace racket/base

(require racket/list
         racket/string
         racket/match
         "makegraph.rkt"
         "flags.rkt"
         "process-syscalls.rkt"
	 racket/hash)

(provide get-last-edge
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

(define (hash-add h1 h2)  
  (for ([(key value) (in-hash h2)])
    (hash-set! h1 key value)))

(define (get-last-edge t)
  (cond
    [(empty? (target-out-edges t))
     #f]
    [else
     (car (target-out-edges t))]))

(define (leaf-node? node)
  (empty? (target-out-edges node)))

(define (work build)
  (for/sum ([graph (buildgraph-makegraphs build)])
    (work-graph graph)))

(define (work-graph graph)
  (define visited (make-hash))
  (define cache (make-hash))  

  (define (node-work node-id)
    (define node (get-target graph node-id))
    (cond
      [(and (hash-ref visited node-id #f)
      	    (or (target-phony? node) (equal? target-type 'name)))
       (hash-ref cache node-id)]
      [(hash-ref visited node-id #f)
       0]
      [else
       (define node (get-target graph node-id))
       (hash-set! visited node-id #t)
       (define tmp (if (leaf-node? node)
       	               (leaf-node-work node)
	               (non-leaf-node-work node)))
       (hash-set! cache node-id tmp)
       tmp]))

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
      (node-work (edge-end e))))

  (node-work (makegraph-root graph)))

(define (span build)
  (for/sum ([graph (buildgraph-makegraphs build)])
    (span-graph graph)))

(define (span-graph graph)
  (define cache (make-hash))  

  (define (node-span node-id ancestors)
    (define node (get-target graph node-id))
    (cond
     [(and (hash-ref ancestors node-id #f)
           (or (target-phony? node) (equal? target-type 'name)))
      (values (hash-ref cache node-id) (hash-set ancestors node-id #t))]
     [(hash-ref ancestors node-id #f)
      (values 0 ancestors)]
     [(hash-ref cache node-id #f) =>
      (lambda (val)
      (values val (hash-set ancestors node-id #t)))]
     [else
      (cond
        [(leaf-node? node)
	 (define tmp (leaf-node-span node))
	 (hash-set! cache node-id tmp)
         (values tmp (hash-set ancestors node-id #t))]
        [else
	 (define-values (tmp a) (non-leaf-node-span node (hash-set ancestors node-id #t)))
	 (hash-set! cache node-id tmp)
	 (values tmp a)])]))
       
  (define (leaf-node-span node)
    (define data (target-data node))
    (cond
      [(rusage-data? data)
       (rusage-data-elapsed data)]
      [(number? data)
       data]
      [else
       (error 'leaf-node-span "Unrecognized data ~a" data)]))

  (define (non-leaf-node-span node ancestors)
    (define-values (m s a) ;; m is span of dependencies and s is sum of span of recipes
      (for/fold ([max_ 0]
                 [sum 0]
		 [ancestors_ ancestors])
                ([e (reverse (target-out-edges node))])
        (define nid (edge-end e))
	(cond
	  [(equal? 'dep (edge-type e))
	   (define-values (span_ a) (node-span nid ancestors))
	   (values (max max_ span_) sum (merge-ancestors ancestors_ a))]
	  [else
           (define-values (span_ a) (node-span nid ancestors_))
	   (values max_ (+ sum span_) a)])))

    (values (+ m s) a))

  (define-values (val _) (node-span (makegraph-root graph) (hash)))
  val)

#|
(define (all-paths graph)
  (define startnode (get-target (makegraph-targets) (makegraph-root graph)))

  (define (all-paths-leaf-node node)
    ;; only one path...
    (define data (target-data node))
    (cond
      [(rusage-data? data)
       (list (rusage-data-elapsed data))]
      [(number? data)
       (list data)]
      [else
       (error 'all-paths-leaf-node "Did not recognize data ~a" data)]))

  (define (all-paths-non-leaf-node node)
  |#  

(define (merge-ancestors h1 h2)
  (hash-union h1 h2 #:combine/key (lambda (k a b) #t)))

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
  
