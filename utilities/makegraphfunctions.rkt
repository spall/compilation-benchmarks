#lang errortrace racket/base

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
         check-dependencies
	 get-all-in-out)

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

(define (get-all-in-out root_ syscalls)
  (define (driver root)
    ;; calculate the ins and outs of its not submake recipes
    ;; calculate the ins and outs of its depedencies
    ;; calculate the ins and outs of its submake recipes
    
    ;; non submake recipes
    (define-values (ins1 outs1)
      (for/fold ([ins '()]
      		 [outs '()])
 		([r (target-data root)])
	(define-values (ti to)
          (if (rusage-data? r)
	      (process-in-out-pid (rusage-data-pid r) syscalls)
	      (values '() '())))
        (values (append ti ins) (append to outs))))

    ;; ins and outs of dependencies + submake recipes
    (define-values (ins2 outs2)
      (for/fold ([ins '()]
       		 [outs '()])
		([e (target-out-edges root)])
	(define-values (ti to)
	  (get-all-in-out (edge-end e) syscalls))
  	(values (append ti ins) (append to outs))))

    (values (append ins1 ins2) (append outs1 outs2)))

  (driver root_))

(define (check-dependencies-recipe root_ graph syscalls)
  (define (driver root)
    ;; combine recipes with "edges"
    ;; Want to go through them in order.
    ;; compare inputs of edge to the previous edge's outputs
    (define recipe-edges (filter (lambda (x)
    	    		 	   (equal? (edge-type x) 'seq))
				 (target-out-edges root)))
 
    (define all (sort (append (filter rusage-data? (target-data root)) recipe-edges)
    	    	      	      >
			      #:key
			      (lambda (x)
			        (if (edge? x)
				    (edge-id x)
				    (rusage-data-id x)))))
    (define ins (make-hash))
    (define outs (make-hash))

    (for ([e all])
     
      (define rid (if (edge? e)
      	      	      (edge-id e)
		      (rusage-data-id e)))
      
      (define-values (i o)
        (if (edge? e)
	    (get-all-in-out (edge-end e) syscalls)
	    (process-in-out-pid (rusage-data-pid e) syscalls)))

      (hash-set! ins rid i)
      (hash-set! outs rid o))

    ;; loop through the recipes reverse order;
    (let loop ([comparing '()]
    	       [recipes (reverse all)])
      (cond
       [(empty? recipes)
        (printf "Need to compare recipes: ~a against dependencies; may be able to run in parallel\n\n" comparing)]
       [else
        ;; compare output of car of recipes
	;; to input of all the recipes in comparing
	(define tmpouts (if (edge? (car recipes))
	 		    (hash-ref outs (edge-id (car recipes)))
			    (hash-ref outs (rusage-data-id (car recipes)))))

       
	(define pparallel (cons (car recipes) (for/fold ([ls '()])
		  ([r comparing])
          (define rins (if (edge? r)
	  	       	   (hash-ref ins (edge-id r))
			   (hash-ref ins (rusage-data-id r))))

          (define uses? (let inner ([tmp tmpouts])
	      	    	  (cond
	       		   [(empty? tmp)
	        	    #f]
	       		   [(member (car tmp) rins)
	       		    #t]
	       		   [else
	        	    (inner (cdr tmp))])))
	  ;; if r uses car of recipes, then cannot run in parallel with car of recipes
	  ;; may want to print something here, but what
	  (if uses? 
	      ls
	      (cons r ls)))))
        (when (> (length pparallel) 1) 
        (printf "Recipes ~a may be able to run in parallel\n\n" pparallel))
	(loop pparallel (cdr recipes))]))


    ;; recur on edges
    (for ([r (target-out-edges root)])
      (driver (edge-end r))))    

  (for ([r (target-out-edges root_)])
    (driver (edge-end r))))

     

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
           (values in (append nouts out) (cons (cons e (list nouts)) dout))]
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

  (driver root_)

  (printf "Calling check-dependencies-recipe now\n")
  (check-dependencies-recipe root_ graph syscalls))

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
  
