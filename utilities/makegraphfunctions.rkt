#lang racket/base

(require racket/list
         racket/string
         racket/match
         "makegraph.rkt"
         "flags.rkt"
         "process-syscalls.rkt")

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
         print-graph
         check-dependencies)

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

;; TODO: what about transitive dependencies?
;; TODO: also want to check if recipe 2 depends on something recipe 1 produced etc.

;; What does this function do?
;; for each target
;; checks how much the outs of its dependencies overlap with the ins of its recipes
;; How do we calculate the ins and outs of a target?
;; If the target is a leaf target
;;   ins = all of the ins of the target's recipes; which are stored in target
;;   outs = all of the outs of the target's recipes; which are stored in target

;; If the target is NOT a leaf target
;;   ins = all of the ins of recipes that are NOT submakes
;;       + ins of submake recipes
;;   outs = all of the outs of recipes that are NOT submakes
;;        + outs of submake recipes
(define (check-dependencies root_ graph syscalls)
  (define visited (make-hash))

  ;; calculates the ins and outs of a target
  (define (driver root)
    (define-values (ins outs) ;; ins of recipes that are not submakes
      (for/fold ([in '()]     ;; outs of recipes that are not submakes
                 [out '()])
                ([info (target-data root)])
        (define-values (i o) (if (rusage-data? info)
		       	     	 (process-in-out-pid (rusage-data-pid info) syscalls)
				 (values '() '())))
		
        (values (if (member i in)
		    in
		    (append i in)) 
		(if (member o out)
		    out
		    (append o out)))))

    (define-values (ins2 outs2 douts)
      (for/fold ([in '()]
                 [out '()]
                 [dout '()])
                ([e (reverse (target-out-edges root))])
        (define n (edge-end e))
        
        (when (hash-ref visited n #f)
          (error 'check-dependencies "visited node <~a,~a> a 2nd time\n" (target-name n)
                 (target-mfile n)))
        (hash-set! visited n #t)

        (define-values (nins nouts) (driver n))
	
        (cond
          [(equal? (edge-type e) 'dep) ;; dependency so we care about what it is writing
           (values in out (cons (cons e (list nouts)) dout))]
          [else
           (values (append nins in) (append nouts out) dout)])))

    (define all-ins (let loop ([ls (append ins ins2)])
    	    	      (cond
		       [(empty? ls)
		        '()]
		       [(member (car ls) (cdr ls))
		        (loop (cdr ls))]
		       [else
		        (cons (car ls) (loop (cdr ls)))])))

    (define all-outs (let loop ([ls (append outs outs2)])
    	    	      (cond
		       [(empty? ls)
		        '()]
		       [(member (car ls) (cdr ls))
		        (loop (cdr ls))]
		       [else
		        (cons (car ls) (loop (cdr ls)))])))

    (for ([p douts])
      (define outs (cadr p))
      (unless (empty? outs) 
        (define used#
          (for/sum ([o outs])
            (if (member o all-ins)
		  1
                0)))
        (printf "<~a,~a>'s dependency <~a,~a> used ~a times\n" (target-name root) (target-mfile root) 
			   	      	      	      	       (target-name (edge-end (car p)))
      	      		       	    	       	       	       (target-mfile (edge-end (car p))) used#)))
    (values all-ins all-outs))

  (driver root_))

(define (work root_ graph)
  (define visited (make-hash))
  (define (driver root)
    (+ (sum-times (target-data root))
       (for/sum ([e (reverse (target-out-edges root))])
         (define n (edge-end e))
         
         (when (hash-ref visited n #f)
           (error 'work "Visited node <~a,~a> a 2nd time from <~a,~a>!" (target-name n) (target-mfile n) (target-name root) (target-mfile root)))
         
         (hash-set! visited n #t)
         (driver n))))
  (hash-set! visited root_ #t)
  (define w (driver root_))

  ;; Checking that we visited each node once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  (for ([t (in-hash-keys (makegraph-targets graph))])
    (unless (hash-ref visited t #f)
      (error 'work "dep Never visited node ~a!" t)))
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
  (define (driver root)
    (define-values (sd wd)
      (for/fold ([max_ 0]
                 [sum (sum-times (target-data root))])
                ([e (reverse (target-out-edges root))])
        (define n (edge-end e))
        (when (hash-ref visited n #f)
          (error 'span "Visited node ~a a 2nd time!" n))

        (define tmpspan (driver n))

        (cond
          [(equal? 'dep (edge-type e))
           (values (max max_ tmpspan) sum)]
          [else
           (values max_ (+ sum tmpspan))])))

    (+ sd wd))

  (hash-set! visited root_ #t)
  (define s (driver root_))

  ;; Checking that we visited each node once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  #;(for ([t (in-hash-keys (makegraph-targets graph))])
      (unless (hash-ref visited t #f)
        (error 'span "Never visited node ~a during span calculation!" t)))  
  s)

(define (longest-target root_ graph)
  (void)) ;; todo

(define (longest-leaf graph)
  (define-values (target_ time_)
    (for/fold ([maxt #f]
               [max 0])
              ([t (in-hash-values (makegraph-targets graph))])
      ;; test if target is a leaf
      (cond
        [(empty? (target-out-edges t)) ;; leaf
         (define tmp (sum-times (target-data t)))
         (if (> tmp max)
             tmp
             max)]
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
      (let ([tmp (edge-end e)])
        (printf "Dependency edge between <~a,~a,~a> and <~a,~a,~a> with ID ~a\n\n"
                (target-name n) (target-mfile n) (target-id n)
                (target-name tmp) (target-mfile tmp) (target-id tmp) (edge-id e))))
    
    (printf "Finished processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))))
  
