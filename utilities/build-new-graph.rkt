#lang errortrace racket/base

(require racket/list
	 "makegraph.rkt"
	 "makegraphfunctions.rkt"
	 "process-syscalls.rkt")

(provide build-new-graph)

(define (process-recipes recipes syscalls)
  
  (define ins (make-hash))
  (define outs (make-hash))

  (for ([recipe recipes])
    (when (rusage-data? recipe)
      (define-values (i o) (process-in-out-pid (rusage-data-pid recipe) syscalls))
      (hash-set! ins (rusage-data-id recipe) i)
      (hash-set! outs (rusage-data-id recipe) o)))

  (let loop ([lss (list '())]
  	     [rs recipes])
    (cond
     [(empty? rs)
      lss]
     [else
      (define recipe (car rs))
      (cond
       [(rusage-data? recipe)
        (define rid (rusage-data-id recipe))
	(define ls (car lss))
	;; compare output of recipe to all of the recipes in ls
	(define routs (hash-ref outs rid))

	(define-values (nls ols)
	  (for/fold ([nls '()]
		     [ols '()])
		    ([l ls]) ;; l is an rusage-data structure
            (define lin (hash-ref ins (rusage-data-id l)))
	    (if (let inner ([tmp routs])
	          (cond
		   [(empty? tmp)
		    #f]
		   [(member (car tmp) lin)
		    #t]
		   [else
		    (inner (cdr tmp))]))
	         (values nls (cons l ols))
	         (values (cons l nls) ols))))
       (loop (cons (cons recipe nls) (if (empty? ols)
       	     	   	 	     	 (cdr lss)
					 (cons ols (cdr lss))))
	     (cdr rs))]
      [else
       (printf "Recipe is ~a not rusage-data\n" recipe)
       (loop lss (cdr rs))])])))

#| leaf is a target that has no out edges

   1. consider each recipe.
   2.  build list of lists of recipes.  
       for example: '( (1 2 3) ) means that recipes 1 2 and 3 can run in parallel and there are 
       no other recipes
       '( (1 2) (3 4))  means that recipes 1 and 2 can run in parallel and 3 and 4 can run in
       parallel, after 1 and 2 finish.
   3. Using the above lists, for each sublist; create a fake target with dependency edges for
      each thing in the sublist;  so create a target for each thing in sublist....
   4. Create a 'seq edge from copy of current target to new fake targets;
   5. return this new target 
|#
(define (build-new-leaf t syscalls)
  
  ;; 4. create copy of target
  (define nt (target (target-id t) (target-name t) (target-mfile t) '() '() '())) ;; throw away in edges and data
 
  ;; 3. 
  (for ([ls (process-recipes (target-data t) syscalls)])
    (define tmp-fake (create-target (symbol->string (gensym "FAKE"))))
    (set-target-mfile! tmp-fake (target-mfile t))
    (cond
     [(= 1 (length ls))
      (set-target-data! tmp-fake ls)]
     [else
      (for ([l ls])
        (define tmp (create-target (symbol->string (gensym "FAKE"))))
	(set-target-mfile! tmp (target-mfile t))
	(set-target-data! tmp (list l))
	(add-dependency tmp-fake tmp))])
    (add-recipe nt tmp-fake))      

  nt)

(define (process-recipes2 recipes deps syscalls)
  
  (define ins (make-hash))
  (define outs (make-hash))

  (for ([recipe (append deps recipes)])
    (cond
     [(rusage-data? recipe)
      (define-values (i o) (process-in-out-pid (rusage-data-pid recipe) syscalls))
      (hash-set! ins (rusage-data-id recipe) i)
      (hash-set! outs (rusage-data-id recipe) o)]
     [else ;; edge
      (define-values (i o) (get-all-in-out (edge-end recipe) syscalls))
      (hash-set! ins (edge-id recipe) i)
      (hash-set! outs (edge-id recipe) o)]))

  (let loop ([lss (list '())]
  	     [rs recipes])
    (cond
     [(empty? rs)
      ;; compare car of lss to deps
      (define-values (ndeps ols)
        (for/fold ([ndeps '()]
      		   [ols '()])
                  ([l (car lss)])
          (define lins (hash-ref ins (if (rusage-data? l)
		     	       	         (rusage-data-id l)
				         (edge-id l))))
          (let outer ([ds deps])
	    (cond
	     [(empty? ds)
	      (values (cons l ndeps) ols)]
	     [(let inner ([ds2 (hash-ref outs (edge-id (car ds)))])
	        (cond
	         [(empty? ds2)
	      	  #f]
	         [(member (car ds2) lins)
	      	  #t]
	         [else
	      	  (inner (cdr ds2))]))
	      (values ndeps (cons l ols))]
	     [else
	      (outer (cdr ds))]))))
      (values ndeps (if (empty? ols)
       	      	    	(cdr lss)
			(cons ols (cdr lss))))] 
     [else
      (define recipe (car rs))
      (define rid (if (edge? recipe)
      	      	      (edge-id recipe)
		      (rusage-data-id recipe)))
      (define ls (car lss))
      ;; compare output of recipe to all of the recipes in ls
      (define routs (hash-ref outs rid))
      (define-values (nls ols)
	  (for/fold ([nls '()]
		     [ols '()])
		    ([l ls]) ;; l is an rusage-data structure or an edge
            (define lin (hash-ref ins (if (edge? l)
	    	    		      	  (edge-id l)
					  (rusage-data-id l))))
	    (if (let inner ([tmp routs])
	          (cond
		   [(empty? tmp)
		    #f]
		   [(member (car tmp) lin)
		    #t]
		   [else
		    (inner (cdr tmp))]))
	         (values nls (cons l ols))
	         (values (cons l nls) ols))))
      (loop (cons (cons recipe nls) (if (empty? ols)
       	     	   	 	     	 (cdr lss)
					 (cons ols (cdr lss))))
	     (cdr rs))])))

#|
   non-leaf node has and/or dependencies and recipes
   1. Need to recur on all dependencies and recursive make recipes
   2. First thing is to probably create a list of dependencies
      and create a list of recipes; which are combination of target-data and submakes
   3. Do the same analysis   
|#
(define (build-new-non-leaf t syscalls)
  ;; 4. create copy of target
  (define nt (target (target-id t) (target-name t) (target-mfile t) '() '() '())) ;; throw away in edges and data
  
  (define-values (deps reps)
    (for/fold ([ds '()]
    	       [rs '()])
	      ([e (target-out-edges t)])
      (cond
       [(equal? (edge-type e) 'dep)
        (values (cons e ds) rs)]
       [else
        (values ds (cons e rs))])))

  (for ([d deps])
    ;; want to recur on d and add as a dependency to nt
    (add-dependency nt (build-new-target (edge-end d) syscalls)))

  ;; need to combine reps with any target-data
  (define recipes (sort (append (filter rusage-data? (target-data t)) reps)
  	  	  	>
			#:key (lambda (x)
			        (if (edge? x)
				    (edge-id x)
				    (rusage-data-id x)))))

  (define-values (ndeps nreps) (process-recipes2 (reverse recipes) deps syscalls))

  (for ([d ndeps])
    (cond
     [(edge? d)
      (add-dependency nt (build-new-target (edge-end d) syscalls))]
     [else
      (define tmp (create-target (symbol->string (gensym "FAKE"))))
      (set-target-mfile! tmp (target-mfile t))
      (set-target-data! tmp (list d))
      (add-dependency nt tmp)]))

  (for ([ls nreps])
    (cond
     [(= 1 (length ls))
      (cond
       [(edge? (car ls))
        (add-recipe nt (build-new-target (edge-end (car ls)) syscalls))]
       [else
        (define tmp-fake (create-target (symbol->string (gensym "FAKE"))))
    	(set-target-mfile! tmp-fake (target-mfile t))
	(set-target-data! tmp-fake ls)
	(add-recipe nt tmp-fake)])]
     [else
      (define tmp-fake (create-target (symbol->string (gensym "FAKE"))))
      (set-target-mfile! tmp-fake (target-mfile t))
      (for ([l ls])
        (cond
	 [(edge? l)
	  (add-dependency tmp-fake (build-new-target (edge-end l) syscalls))]
	 [else
	  (define tmp (create-target (symbol->string (gensym "FAKE"))))
	  (set-target-mfile! tmp (target-mfile t))
	  (set-target-data! tmp (list l))
	  (add-dependency tmp-fake tmp)]))
       (add-recipe nt tmp-fake)]))

  nt)  
			 
(define (leaf? t)
  (empty? (target-out-edges t)))

(define (build-new-target t syscalls)
  (if (leaf? t)
      (begin (printf "processing leaf\n")
             (build-new-leaf t syscalls))
      (begin (printf "processing non-leaf\n")
      (build-new-non-leaf t syscalls))))

(define (build-new-graph graph syscalls)
  (define ngraph (create-makegraph))
  (set-makegraph-root! ngraph (build-new-target (makegraph-root graph) syscalls))
  ngraph)  
