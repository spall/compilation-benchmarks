#lang errortrace racket/base

(require racket/list
         racket/string
         racket/match
	 racket/sequence
	 data/bit-vector
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
	 build-span-graph
         parallel-slackness
         predicted-speed-upper
         predicted-speed-lower
	 predicted-speed-perfect-linear
         verify-edges
         print-graph
	 print-targets-most-to-least-build
	 print-make-targets-most-to-least-build
	 print-intersections-of-span-build)


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

  (define (node-work node-id)
    (define node (get-target graph node-id))
    (cond
     [(hash-ref visited node #f)
      0]
     [(leaf-node? node)
      (hash-set! visited node #t)
      (leaf-node-work node)]
     [else
      (hash-set! visited node #t)
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
	     (node-work (edge-end e))))

  (node-work (makegraph-root graph)))

(define (intersect ls1 ls2)
  (cond
   [(empty? ls1)
    '()]
   [(member (car ls1) ls2)
    (cons (car ls1) (intersect (cdr ls1) ls2))]
   [else
    (intersect (cdr ls1) ls2)]))

(define (print-intersections-of-target t graph syscalls)
  (cond
   [(empty? (target-out-edges t)) ;; leaf
    (define data_ (target-data t))
    (cond
     [(empty? data_)
      (printf "<~a,~a> doesn't have rusage-data\n\n" (target-name t) (target-mfile t))
      (values '() '())]
     [else
      (define data (car data_))
      (cond
       [(rusage-data? data) ;;  Get ins and outs
	;; look up calls in syscalls
	(cond
	 [(hash-ref syscalls (rusage-data-pid data) #f)
	  (define-values (tmp1 tmp2) (process-in-out-pid (rusage-data-pid data) (rusage-data-dir data) syscalls (rusage-data-cmd data)))
	  (values tmp1 tmp2)]
	 [else
	  (printf "no entry in syscalls for pid ~a\n\n" (rusage-data-pid data))
	  (values #f #f)])] ;; no entry 
       [else
	(printf "<~a,~a> doesn't have rusage-data\n\n" (target-name t) (target-mfile t))
	(values '() '())])])] ;; we don't know
   [else 
    (printf "Target <~a,~a>: \n\n" (target-name t) (target-mfile t))
    ;; loop through the edges; they should be in the correct order
    ;; want to go in reverse order 
    (let loop ([es (target-out-edges t)]
	       [lastt #f]
	       [last-ins '()]
	       [last-outs '()])
      (cond
       [(empty? es)
	(values '() '())]
       [else
	(define t2 (get-target graph (edge-end (car es))))
	(define-values (ins outs) (print-intersections-of-target t2 graph syscalls))
	(define t2-cmd (if (and (not (empty? (target-data t2)))
				(rusage-data? (car (target-data t2))))
			   (rusage-data-cmd (car (target-data t2)))
			   ""))
	(if outs
	    (when lastt
	    	  (define lastt-cmd (if (and (not (empty? (target-data lastt)))
					     (rusage-data? (car (target-data lastt))))
					(rusage-data-cmd (car (target-data lastt)))
					""))
		  (cond
		   [last-ins
		    (define in (intersect outs last-ins))
		    (printf "Intersection of target <~a,~a, cmd: ~a> which ran first and target <~a,~a, cmd: ~a> which ran second is: ~a\n\n" (target-name t2) (target-mfile t2) t2-cmd (target-name lastt) (target-mfile lastt) lastt-cmd in)]
		   [else
		    (printf "Don't have input information for target <~a,~a> so cnanot consider moving it to run in parallel with target <~a,~a>.\n\n" (target-name lastt) (target-mfile lastt) (target-name t2) (target-mfile t2))]))
	    (when lastt
		  (printf "Don't have output information for target <~a,~a> so cannot consider moving target <~a,~a> to be in parallel.\n\n" (target-name t2) (target-mfile t2) (target-name lastt) (target-mfile lastt))))

	(cond
	 [(= 1 (length es))
	  (values ins outs)]
	 [else
	  (call-with-values (lambda () (loop (cdr es)
					     t2 ins outs))
	    (lambda (ins2 outs2)
	      (values (and ins ins2 (append ins ins2))
		      (and outs outs2 (append outs outs2)))))])]))]))

(define (print-intersections-of-span-graph g syscalls)
  (printf "Printing intersections of root target\n\n")
  (print-intersections-of-target (get-target g (makegraph-root g)) g syscalls))

(define (print-intersections-of-span-build b syscalls)
  (for ([mg (buildgraph-makegraphs b)])
       (print-intersections-of-span-graph mg syscalls)))

(define (work-of-target t graph)
  ;; for each edge call work of target
  (cond
   [(empty? (target-out-edges t)) ;; leaf
    (define data (target-data t))
    (cond
     [(empty? data)
      0]
     [(rusage-data? (car data))
      (rusage-data-elapsed (car data))]
     [(number? (car data))
      (car data)]
     [else
      0])]
   [else
    (printf "Target <~a,~a>: \n\n" (target-name t) (target-mfile t))
    (define tmp (for/sum ([e (target-out-edges t)])
			 (define t2 (get-target graph (edge-end e)))
			 (define tmp (work-of-target t2 graph))
			 (printf "Target <~a,~a> is ~a and it's work is ~a.\n" (target-name t2) (target-mfile t2)
				 (if (equal? 'dep (edge-type e))
				     "Dependency"
				     "Recipe") tmp)
			 tmp))
    (printf "Done with target <~a,~a> \n\n" (target-name t) (target-mfile t))
    tmp]))

(define (span build)
  (for/sum ([graph (buildgraph-makegraphs build)])
    (span-graph graph)))

(define (span-graph graph)
  (define cache (make-hash))

  (define (node-span node-id ancestors)
    (define node (get-target graph node-id))
    (cond
     [(hash-ref ancestors node-id #f)
      (values 0 ancestors)]
     [(hash-ref cache node-id #f) =>
      (lambda (val)
      (values val (hash-set ancestors node-id #t)))]
     [(leaf-node? node)
      (define tmp (leaf-node-span node))
      (hash-set! cache node-id tmp)
      (values tmp (hash-set ancestors node-id #t))]
     [else
      (define-values (tmp a) (non-leaf-node-span node (hash-set ancestors node-id #t)))
      (hash-set! cache node-id tmp)
      (values tmp a)]))
       
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
	 [(equal? 'dep (edge-type e)) ;; dependency/prereq
	  (define-values (span_ a) (node-span nid ancestors))
	  (values (max max_ span_) sum (merge-ancestors ancestors_ a))]
	 [else ;; recipe
	  (define-values (span_ a) (node-span nid ancestors_))
	  (values max_ (+ sum span_) a)])))

    (values (+ m s) a))

  (define-values (val _) (node-span (makegraph-root graph) (hash)))
  val)

(define (print-make-targets-most-to-least-graph g total)
  (define targets (makegraph-targets g))
  (define ls (for/fold ([ls '()])
		       ([t (in-hash-values targets)])
		       ;; caculate span of target that aren't shellcalls
		       (cond
			[(empty? (target-out-edges t))
			 ls]
			[else
			 (define s (work-of-target t g))
			 (cons (cons s t) ls)])))

  ;; sort ls
  (define sorted (sort ls > #:key car))
  ;; print them out
  (for ([p sorted])
       (define prctg (* 100 (exact->inexact (/ (car p) total))))
       (printf "Target <~a,~a> is ~a% of total.\n\n" (target-name (cdr p)) (target-mfile (cdr p)) prctg)))

(define (print-make-targets-most-to-least-build build total)
  (for ([mg (buildgraph-makegraphs build)])
       (printf "Targets of top level make\n")
       (print-make-targets-most-to-least-graph mg total)))

(define (print-targets-most-to-least-build build total)
  (for ([mg (buildgraph-makegraphs build)])
       (printf "Targets of top level make\n")
       (print-targets-most-to-least mg total)))

(define (sort-targets-by-time graph)
  (define targets (makegraph-targets graph))
  (sort (sequence->list (in-hash-values targets))
	> #:key (lambda (t)
		  (define data (target-data t))
		  (cond
		   [(rusage-data? data)
		    (rusage-data-elapsed data)]
		   [(number? data)
		    data]
		   [else
		    0]))))

(define (print-targets-most-to-least graph total)
  (define sorted (sort-targets-by-time graph))
  (for ([t sorted])
       (define ttime (cond
		      [(rusage-data? (target-data t))
		       (rusage-data-elapsed (target-data t))]
		      [(number? (target-data t))
		       (target-data t)]
		      [else
		       #f]))
       (define cmd-run (cond
			[(empty? (target-data t))
			 "No command saved as run."]
			[(rusage-data? (target-data t))
			 (rusage-data-cmd (target-data t))]
			[else
			 "No command saved as run."]))
       (when ttime
	     (define prctg (* 100 (exact->inexact (/ ttime total))))
	     (printf "Target <~a,~a> is ~a% of total: ran ~a.\n\n" (target-name t) (target-mfile t) prctg cmd-run))))

(define (build-span-graph build)
  (define new-build (create-buildgraph))
  (define span (for/sum ([graph (buildgraph-makegraphs build)])
			(define-values (time new-makegraph) (build-span-makegraph graph))
			(add-makegraph new-build new-makegraph)
			time)) 
  (values span new-build))

(define (build-span-makegraph graph)
  (define new-graph (create-makegraph))

  (define cache (make-hash))  

  ;; returns time and new target; list of new targets in its span and ancestors
  (define (build-span-node node-id ancestors)
    (define node (get-target graph node-id))
    (cond
     [(hash-ref ancestors node-id #f)
      (values 0 #f '() ancestors)]
     [(hash-ref cache node-id #f) =>
      (lambda (tri)
	(define val (car tri))
	(define new-node (cadr tri))
	(define pairs (cddr tri))
	(values val new-node pairs (hash-set ancestors node-id #t)))]
     [(leaf-node? node)
      (define-values (tmp new-node) (build-span-leaf-node node))
      (hash-set! cache node-id (cons tmp (cons new-node '()))) ;; what else should go in cache?
      (values tmp new-node '() (hash-set ancestors node-id #t))]
     [else
      (define-values (tmp new-node ls a) (build-span-non-leaf-node node (hash-set ancestors node-id #t)))
      (hash-set! cache node-id (cons tmp (cons new-node ls)))
      (values tmp new-node ls a)]))
       
  ;; returns time and new target object
  (define (build-span-leaf-node node)
    (define new-node (target (target-id node) (target-name node) (target-mfile node) (target-phony? node) (target-type node) '() (target-data node)))
    (define data (target-data node))
    (cond
     [(rusage-data? data)
      (values (rusage-data-elapsed data) new-node)]
     [(number? data)
      (values data new-node)]
     [else
      (error 'leaf-node-span "Unrecognized data ~a" data)]))

  ;; returns time and new target object list of new-targets in its span and ancestors
  (define (build-span-non-leaf-node node ancestors)
    
    (define-values (m mnid mnode mpairs s recipeids recipenodes pairs a) ;; m is span of dependencies and s is sum of span of recipes
      (for/fold ([max_ 0]
		 [maxnid #f]
		 [maxnode #f]
		 [maxpairs '()]
                 [sum 0]
		 [recipeids '()]
		 [recipenodes '()]
		 [pairs '()]
		 [ancestors_ ancestors])
                ([e (reverse (target-out-edges node))])
        (define nid (edge-end e))
	(cond
	 [(equal? 'dep (edge-type e))
	  (define-values (span_ newnode ps a) (build-span-node nid ancestors))
	  (cond
	   [(> span_ max_)
	    (unless newnode 
		    (error 'build-span-non-leaf-node "newnode is false"))
	    (values span_ nid newnode ps sum recipeids recipenodes pairs
		    (merge-ancestors ancestors_ a))]
	   [else	     
	    (values max_ maxnid maxnode maxpairs sum recipeids recipenodes pairs  
		    (merge-ancestors ancestors_ a))])]
	 [else
	  (define-values (span_ newnode ps a) (build-span-node nid ancestors_))
	  
	  (values max_ maxnid maxnode maxpairs (+ sum span_) (cons nid recipeids)
		  (cons newnode recipenodes) (append ps pairs) a)])))
    
    (define new-node (target (target-id node) (target-name node) (target-mfile node) (target-phony? node)
			     (target-type node) '() (target-data node)))
    (when mnid
	  (add-dependency new-node mnid))
    (for-each (lambda (id)
		(add-recipe new-node id))
	      (reverse recipeids))
    (values (+ m s) new-node (append (foldl (lambda (id t accu)
					      (if t
						  (cons (cons id t) accu)
						  accu))
					    '()
					    recipeids
					    recipenodes) (if mnid
							     (cons (cons mnid mnode) (append mpairs pairs))
							     (append mpairs pairs)))
	    a))

  (define-values (time new-root span-path-nodes _)
    (build-span-node (makegraph-root graph) (hash)))
  ;; add span-path-nodes to new graph
  (for-each (lambda (p)
	      (define id (car p))
	      (define t (cdr p))
	      (hash-set! (makegraph-targets new-graph) id t))
	    span-path-nodes)
  (hash-set! (makegraph-targets new-graph) (makegraph-root graph) new-root)
  (set-makegraph-root! new-graph (makegraph-root graph))
  (values time new-graph))

(define (merge-ancestors h1 h2)
  (hash-union h1 h2 #:combine/key (lambda (k a b) #t)))

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
  
