#lang racket/base

(require racket/list
         racket/string
         racket/match
         "makegraph.rkt"
         "flags.rkt")

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
         verify-edge-times
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
            ([val (in-hash-values targets)])
    (if (equal? (target-name val) tname)
        (cons val tgs)
        tgs)))

(define (work root_ graph)
  (define visited (make-hash))
  (define (driver root lbound ubound)
    
    (define-values (workdeps workrecipes leid_)
      (for/fold ([dsum 0]
    	       	 [wsum 0]
	         [leid ubound])
	        ([e (reverse (target-out-edges root))])
        (cond
       	 [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
          (when (hash-ref visited e #f)
	    (error 'work "Visited edge ~a a 2nd time from <~a,~a>!" e (target-name root) (target-mfile root)))
	  (hash-set! visited e #t)

	  (define tmpwork (+ (sum-times (edge-data e))
			   (driver (get-target graph (edge-end e))
			   	   (edge-id e) leid)))
	(when DEBUG
             (when (> tmpwork 0)
               (printf "For root dep ~a; work is ~a\n" (edge-end e) tmpwork)))
        
	(cond
	 [(equal? 'dep (edge-type e))
	  (values (+ dsum tmpwork)
	  	  wsum
		  (edge-id e))]
	 [else
	  (values dsum
	  	  (+ wsum tmpwork)
		  (edge-id e))])]
       [else
        (values dsum wsum leid)])))

    (+ workdeps workrecipes))


  (define w (driver root_ (- (edge-id (car (target-out-edges root_))) 1) 1))

  ;; Checking that we visited each edge once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  (for ([t (in-hash-values (makegraph-targets graph))])
    (for ([e (target-out-edges t)])
      (unless (hash-ref visited e #f)
        (error 'work "dep Never visited edge ~a; target is ~a during work calculation!" e t))))
  w)
    
(define (sum-times ttimes)
  (cond
    [(empty? ttimes)
     0]
    [(rusage-data? (car ttimes))
     (cond
       [(not (rusage-data-submake? (car ttimes)))
        (+ (rusage-data-elapsed (car ttimes))
           (sum-times (cdr ttimes)))]
       [else
        (sum-times (cdr ttimes))])]
    [else
     (unless (number? (car ttimes))
       (error 'sum-times "neither rusage-data nor a number ~a" (car ttimes)))
       #;(printf "summing ~a\n" (car ttimes))
     (+ (car ttimes)
        (sum-times (cdr ttimes)))]))

;; Span(root) = longest time down deps path + sum of times of recipes paths
(define (span root_ graph)
  (define visited (make-hash))
  (define (driver root lbound ubound)

    (define-values (spandeps workdeps leid_)
      (for/fold ([max_ 0]
      		 [sum 0]
		 [leid ubound])
		([e (reverse (target-out-edges root))])
	(cond
	 [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
	  (when (hash-ref visited e #f)
             (error 'span "Visited edge ~a a 2nd time!" e))
           (hash-set! visited e #t)
	   
	   (define tmpspan (+ (sum-times (edge-data e))
	   	   	      (driver (get-target graph (edge-end e))
			      	      (edge-id e) leid)))
	
	   (cond
	    [(equal? 'dep (edge-type e))
	     (values (max max_ tmpspan)
	     	     sum
		     (edge-id e))]
	    [else
	     (values max_
	     	     (+ sum tmpspan)
		     (edge-id e))])]
	 [else
	  (values max_ sum leid)])))

      (+ spandeps workdeps))
  
  (define s (driver root_ (- (edge-id (car (target-out-edges root_))) 1) 1))

  ;; Checking that we visited each edge once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  #;(for ([t (in-hash-values (makegraph-targets graph))])
    (for ([e (target-deps t)])
      (unless (hash-ref visited e #f)
        (error 'span "Never visited edge ~a during span calculation!" e)))
    (for ([e (target-recipes t)])
      (unless (hash-ref visited e #f)
        (error 'span "Never visited edge ~a during span calculation!" e))))
  
  s)

(define (longest-target root_ graph)
  (void)) ;; todo

(define (longest-leaf graph)
  
  (define-values (target_ time_)
    (for/fold ([maxt #f]
  	     [max   0])
  	    ([t (in-hash-values (makegraph-targets graph))])
    ;; test if target is a leaf
      (cond
        [(empty? (target-out-edges t))
         ;; is a leaf
	 ;; TODO: Now add up times on edge into it. 
	 (define total
	   (for/fold ([max 0])
	   	    ([e (target-in-edges t)])
	      (define tmp (sum-times (edge-data e)))
	      (if (> tmp max)
	      	 tmp
	 	 max)))
	
	 (if (> total max)
	     (values t total)
	     (values maxt max))]
	[else
	 (values maxt max)])))

  time_)

(define (longest-recipe root_ graph)
  (void))

;; factor by which the parallelism of the computation exceeds the number of processors
(define (parallel-slackness graph pcount)
  (define work_ (work (makegraph-root graph) graph))
  (define span_ (span (makegraph-root graph) graph))
  (exact->inexact (/ work_ (* pcount span_))))

#|
   brent's law 
   Should be an UPPER BOUND on the amount of time to execute work with
   pcount processors.
|#
(define (predicted-speed-upper graph pcount [work_ #f] [span_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph) graph)))
  (unless span_
    (set! span_ (span (makegraph-root graph) graph)))

  (exact->inexact (+ span_ (/ work_ pcount))))

(define (predicted-speed-lower graph pcount [work_ #f] [span_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph) graph)))
  (unless span_
    (set! span_ (span (makegraph-root graph) graph)))
  (define speed (exact->inexact (/ work_ pcount)))

  (if (< speed span_)
      span_
      speed))

(define (predicted-speed-perfect-linear graph pcount [work_ #f])
  (unless work_
    (set! work_ (work (makegraph-root graph) graph)))
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
  (for ([t (in-hash-values targets)])
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

  (for ([t (in-hash-values (makegraph-targets graph))])
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

(define (verify-edge-times graph)
  (define (driver root lbound ubound)
    
    (define-values (workdeps leid_)
      (for/fold ([sum 0]
                 [leid ubound])
                ([e (reverse (target-out-edges root))])
        ;; each edge has a list of data. Want to verify data that is a "submake" shell call.
        (cond
          [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
           (define t (driver (get-target graph (edge-end e))
                             (edge-id e)
                             leid))
           (define sumtimes (sum-times (edge-data e)))
           
           
           (for ([info (edge-data e)])
             (when (and (rusage-data? info) (rusage-data-submake? info))
               (printf "Going to verify time for ~a\n\n" (rusage-data-cmd info)) 
               (let ([diff (- (rusage-data-elapsed info) (+ t sumtimes))])
                 (when (> diff 0)
                   (printf "Difference is ~a\n" diff))
                 (when (< diff 0)
                   (printf "LESS THAN ZERO; difference is ~a\n" diff)))))
           (values (+ sum sumtimes t) (edge-id e))]
          [else
           (values sum leid)])))

    workdeps)
  
  (driver (makegraph-root graph) (- (edge-id (car (target-out-edges (makegraph-root graph)))) 1) 1))
    
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
                                 ([val (in-hash-values targets)])
                         (append accu
                                 (create-dotfile-edges val)))
                       (list "}\n")))))
;; ----------------------------------------------------------------------
            
         

(define (print-graph graph)
  (for ([n (in-hash-values (makegraph-targets graph))])
    (printf "Processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))
    (printf "target-out-edges: ~a\n" (target-out-edges n))
    (for ([e (target-out-edges n)])
      (let ([tmp (get-target graph (edge-end e))])
        (printf "Dependency edge between <~a,~a,~a> and <~a,~a,~a> with ID ~a\n\n"
                (target-name n) (target-mfile n) (target-id n)
                (target-name tmp) (target-mfile tmp) (target-id tmp) (edge-id e))))
    
    (printf "Finished processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))))
  
