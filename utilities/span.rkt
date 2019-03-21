#lang racket/base

(require racket/hash
	 racket/list

	 "makegraph.rkt"
	 "common.rkt")

(provide span
	 build-span-graph
	 top-n-span)

#| returns the span time of the build
|#
(define (span build)
  (for/sum ([graph (buildgraph-makegraphs build)])
	   (span-graph graph)))

#| returns the span time of the makegraph
|#
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
     [(leaf? node)
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


#|  takes a build and an optional hash table of substitutes [tid -> time]
    and find the span path through the build and returns that as a new build.
|#
(define (build-span-graph build [subs (hash)])
  (define new-build (create-buildgraph))
  (values (for/sum ([graph (buildgraph-makegraphs build)])
		   (define-values (time new-makegraph) (build-span-makegraph graph subs))
		   (add-makegraph new-build new-makegraph)
		   time)
	  new-build))

#|  takes a makegraph and an optional hash table of substitutes [tid -> time]
    and finds the span path through the graph and returns that as a new graph
|#
(define (build-span-makegraph graph [subs (hash)])
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
	(values (car tri) (cadr tri) (cddr tri) (hash-set ancestors node-id #t)))]
     [(leaf? node)
      (define-values (tmp new-node) (build-span-leaf-node node))
      (cond
       [(hash-ref subs node-id #f) =>
	(lambda (t)
	  (hash-set! cache node-id (cons t (cons new-node '())))
	  (values t new-node '() (hash-set ancestors node-id #t)))]
       [else
	(hash-set! cache node-id (cons tmp (cons new-node '()))) ;; what else should go in cache?
	(values tmp new-node '() (hash-set ancestors node-id #t))])]
     [else
      (define-values (tmp new-node ls a) (build-span-non-leaf-node node (hash-set ancestors node-id #t)))
      (cond
       [(hash-ref subs node-id #f) =>
	(lambda (t)
	  (hash-set! cache node-id (cons t (cons new-node ls)))
	  (values t new-node ls a))]
       [else
	(hash-set! cache node-id (cons tmp (cons new-node ls)))
	(values tmp new-node ls a)])]))
       
  ;; returns time and new target object
  (define (build-span-leaf-node node)
    (define new-node (target (target-id node) (target-name node) (target-mfile node) 
			     (target-phony? node) (target-type node) '() (target-data node)))
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
    
    (define new-node (target (target-id node) (target-name node) (target-mfile node) 
			     (target-phony? node) (target-type node) '() (target-data node)))
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
					    recipenodes) 
				     (if mnid
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


#| takes a build and sets the top n dominate span targets to 0 to see
   what the new span would be if they were no longer part of the span.
|#
(define (top-n-span build n_)
;; first find out with span is and what the most expensive target is.
  (define (loop n substitutes)
    (cond
     [(> n 0)
      (define-values (s new-build) (build-span-graph build substitutes))
      (define-values (metid me) (most-expensive-leaf new-build substitutes))
      
      ;; todo: print additional information about the target because if it
      ;; isnt a leaf node then we want to know which parts of it are the most expensive.
      (cond
       [metid
	(printf "[span: ~a; ~a] Longest part of span is ~a; cmd: ~a\n Without this: \n" s (if (rusage-data? (target-data me))
											    (rusage-data-elapsed (target-data me))
											    "no data") metid (if (rusage-data? (target-data me))
										 (rusage-data-cmd (target-data me))
										 "No command run"))
	(loop (- n 1) (hash-set substitutes metid 0))]
       [else
	(printf "No member of span has time greater than 0.")])]
     [else
      (define-values (s new-build) (build-span-graph build substitutes))
      (printf "~a\n" s)]))
	     
  (loop n_ (hash)))

		
(define (merge-ancestors h1 h2)
  (hash-union h1 h2 #:combine/key (lambda (k a b) #t)))


#|  returns the targetid of the most expensive leaf in the build
|#
(define (most-expensive-leaf build [subs (hash)])
  (define-values (_ p) (for/fold ([max 0]
				  [maxtid #f])
				 ([mg (buildgraph-makegraphs build)])
				 (define-values (w me) (most-expensive-leaf-graph mg subs))
				 (if (> w max)
				     (values w (cons me (get-target mg me)))
				     (values max maxtid))))
  (values (car p) (cdr p)))

#|  returns the targetid of the most expensive leaf in the makegraph
|#
(define (most-expensive-leaf-graph graph [subs (hash)])
  (define targets (makegraph-targets graph))
  (define ls (for/fold ([ls '()])
		       ([tid (in-hash-keys (makegraph-targets graph))])
		       (define t (hash-ref (makegraph-targets graph) tid))
		       (cond
			[(empty? (target-out-edges t))
			 (define s (work-of-target tid graph subs))
			 (cons (cons s tid) ls)]
			[else
			 ls])))
  
  ;; sort ls
  (define sorted (sort ls > #:key car))
  ;; return first thing
  (if (empty? sorted)
      (error 'most-expensive-target-graph "No non-leaf targets in graph\n")
      (values (caar sorted)
	      (cdar sorted))))
