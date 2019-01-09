#lang errortrace racket/base

(require racket/list
         racket/sequence
	 racket/string
	 "makegraph.rkt"
	 "makegraphfunctions.rkt"
	 "process-syscalls.rkt"
	 "flags.rkt")

(provide build-new-graph)

(define RUNNING 0)
(define NOT-RUNNING 0)

(define (intersect ls1 ls2)
  (cond
   [(empty? ls1)
    '()]
   [(member (car ls1) ls2)
    (cons (car ls1) (intersect (cdr ls1) ls2))]
   [else
    (intersect (cdr ls1) ls2)]))

(define (intersect? ls1 ls2)
  (cond
   [(empty? ls1)
    #f]
   [(member (car ls1) ls2)
    #t]
   [else
     (intersect? (cdr ls1) ls2)]))

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
  
(define (build-new-non-leaf tid graph syscalls new-targets ins-cache outs-cache)
  
  ;; 4. create copy of target
  (define t (get-target graph tid))
  (define nt (target (target-id t) (target-name t) (target-mfile t) (target-phony? t) (target-type t) '() #f))
  (define ntid (create-targetid (targetid-name tid) (targetid-mfile tid)))
  
  (define-values (deps reps)
    (let split ([es (target-out-edges t)])
      (cond
       [(empty? es)
	(values '() '())]
       [else
	(call-with-values (lambda () (split (cdr es)))
	  (lambda (ds rs)
	    (if (equal? 'dep (edge-type (car es)))
		(values (cons (car es) ds) rs)
		(values ds (cons (car es) rs)))))])))
  
  ;; collect all of the ins and outs of all of the dependencies
  ;; rebuild all of the dependencies and add to graph
  (define-values (dins douts)
    (let process-deps ([ds (reverse deps)])
      (cond
       [(empty? ds)
	(values '() '())]
       [else
	(define d (car ds))
	(define-values (ndid ins outs)
	  (build-new-target (edge-end d) graph syscalls new-targets ins-cache outs-cache))
	
	(cond
	 [ndid
	  (add-dependency nt ndid)]
	 [(debug?)
	  (printf "Dependency <~a,~a> did not do anything so deleting from graph\n" (target-name (edge-end d)) (target-mfile (edge-end d)))])
	
	(call-with-values (lambda () (process-deps (cdr ds)))
	  (lambda (is os)
	    (values (and ins is (append ins is))
		    (and outs os (append outs os)))))])))
  
  ;; Want to process recipes in reverse order of how they were run.
  ;; Example: run a, run b, run c;
  ;; Want to process c; then compare ins of c to outs o b;
  ;; if they do not share ins and outs;
  ;; then want to compare ins of b and c to outs of a.
  ;; etc.
  ;; then if c uses outs of a; but b does not
  ;; want to compare ins of a and b to outs of dependencies
  (define rep-ins (make-hash))
  (define rep-outs (make-hash))
  (define all-ins? #t)
  (define all-outs? #t)

  (define die? #f)
  
  (define-values (new-deps new-recipes)
    (let process-reps ([cr '()]
                       [rr '()]
                       [rs reps])   ;; want to go through these in reverse order
      (cond
       [(empty? rs) ;; compare cr to  deps
	(cond
	 [douts
	  (define-values (cans cants)
	    (let loop ([crs cr])	        
	      (cond
	       [(empty? crs)
		(values '() '())]
	       [(hash-ref rep-ins (car crs) #f) =>
		(lambda (is)
		  (call-with-values
                      (lambda () (loop (cdr crs)))
		    (lambda (can cant)
		      (cond
		       [(debug?)
			(define itmp (intersect douts is))
			(cond
			 [(not (empty? itmp))
			  (printf "Intersection of ~a's dependencies and ~a is ~a\n" tid (car crs) itmp) 
			  (values can (cons (car crs) cant))]
			 [else
			  (values (cons (car crs) can) cant)])]
		       [(intersect? douts is)
			(values can (cons (car crs) cant))]
		       [else
			(values (cons (car crs) can) cant)]))))]
			
	       [else
		(call-with-values
                    (lambda () (loop (cdr crs)))
		  (lambda (can cant)
		    (values can (cons (car crs) cant))))])))
	  
	  (values cans (cons cants rr))]
	 [else ;; can't move antyhing
	  (values '() (cons cr rr))])]
       [else ;; compare (car rs) to each thing in cr
	(define-values (ntid ins outs)
	  (build-new-target (edge-end (car rs)) graph syscalls new-targets ins-cache outs-cache))
	(define nt (hash-ref new-targets ntid))
	
	(cond
	 [(and ntid ins outs) ;; enough information to move 
	  (hash-set! rep-ins ntid ins)
	  (hash-set! rep-outs ntid outs)
	  
	  ;; What does loop do?
	  
	  ;; compare outs to ins of stuff in cr
	  (define-values (cans cants)
	    (let loop ([crs cr])
	      (cond
	       [(empty? crs)
		(values '() '())] ;; cans and cants
	       [(hash-ref rep-ins (car crs) #f) =>
		(lambda (is)
		  (call-with-values
                      (lambda () (loop (cdr crs)))
		    (lambda (can cant)
		      (cond
		       [(debug?)
			(define itmp (intersect outs is))
			(cond
			 [(not (empty? itmp))
			  (printf "Intersection of ~a and ~a is ~a\n" (car crs) ntid itmp)
			  (values can (cons (car crs) cant))]
			 [else
			  (values (cons (car crs) can) cant)])]
		       [(intersect? outs is)
			(values can (cons (car crs) cant))]
		       [else
		       	(values (cons (car crs) can) cant)]))))]
	       [else ;; no info for this one
		(call-with-values (lambda () (loop (cdr crs)))
		  (lambda (can cant)
		    (values can (cons (car crs) cant))))])))
	  
	  
	  ;; have determined what can run in parallel with (car rs)
	  ;; and have determined what cant run in parallel with (car rs)
	  (process-reps (cons ntid cans)
			(cons cants rr)
			(cdr rs))]
	 [nt ;; not enough information to move
	  
	  (set! all-ins? #f) (set! all-outs? #f)
	  (process-reps (list ntid)
			(cons cr rr)
			(cdr rs))]
	 [else ;; target didn't do anything so ignore it
	  (when (debug?)
		(printf "Recipe <~a,~a> did not do anything so deleting from graph\n" (target-name (edge-end (car rs))) (target-mfile (edge-end (car rs)))))
	  (process-reps cr rr (cdr rs))])])))

  ;; have new deps if there are any and have new recipe ordering
  (for ([nd new-deps])
       (add-dependency nt nd))

  (for ([nrs new-recipes])
       (unless (empty? nrs)
	       (cond
		[(= 1 (length nrs))
		 (add-recipe nt (car nrs))]
		[else
		 (define tmpid (create-targetid (symbol->string (gensym "FAKE")) (target-mfile nt)))
		 (define tmp (create-target (targetid-name tmpid)))
		 (set-target-mfile! tmp (target-mfile nt))
		 (for ([nr nrs])
		      (add-dependency tmp nr))
		 (hash-set! new-targets tmpid tmp)
		 (add-recipe nt tmpid)])))

  ;; done
  (hash-set! new-targets ntid nt)

  (cond
   [(empty? (target-out-edges nt))
    (when (debug?)
	  (printf "this non leaf node <~a,~a> is now a leaf\n" (target-name nt) (target-mfile nt)))
    (values #f (and all-ins? dins (combine dins rep-ins))
	    (and all-outs? douts (combine douts rep-outs)))]
   [else
    (values ntid (and all-ins? dins (combine dins rep-ins))
	    (and all-outs? douts (combine douts rep-outs)))]))

;; takes a list and a hash table;; collapses hash table and list into one
(define (combine ls h)
  (define new (make-hash))
  (for ([hv (in-hash-values h)])
       (for ([v hv])
	    (hash-set! new v #t)))
  (for ([lv ls])
       (hash-set! new lv #t))

  (for/fold ([tmp '()])
  	    ([k (in-hash-keys new)])
	    (if (list? k)
		(error 'combine "k is a list")
		(cons k tmp))))

(define (leaf? t)
  (empty? (target-out-edges t)))

(define (build-new-graph bgraph syscalls)
  (define new-bgraph (create-buildgraph))
  (for ([graph (buildgraph-makegraphs bgraph)])
       (add-makegraph new-bgraph (build-new-makegraph graph syscalls)))
  new-bgraph)

(define (build-new-leaf tid graph syscalls new-targets)  
  ;; 4. create copy of target
  (define new-tid (create-targetid (targetid-name tid) (targetid-mfile tid)))
  (define t (get-target graph tid))
  
  (hash-set! new-targets new-tid
    	     (target (target-id t) (target-name t) (target-mfile t) (target-phony? t) (target-type t) '() (target-data t)))

  (define data_ (target-data t))
  (define-values (ins outs)
    (cond
     [(empty? data_)
      (values #f #f)]
     [else
      (cond
       [(not (rusage-data? data))
	(values '() '())]
       [(hash-ref syscalls (rusage-data-pid data) #f)
	(printf "processing in/outs <~a,~a>\n" (target-name t) (target-mfile t))
	(process-in-out-pid (rusage-data-pid data) (rusage-data-dir data) syscalls)]
       [else ;; no info
	(values #f #f)])]))

  (values new-tid ins outs))

(define (build-new-target tid graph syscalls new-targets ins-cache outs-cache)
  (define t (get-target graph tid))
  (cond
   [(hash-ref new-targets tid #f) ;; we have already rebuilt this target so do nothing
    (values (create-targetid (targetid-name tid) (targetid-mfile tid))
	    (hash-ref ins-cache tid) (hash-ref outs-cache tid))]
   [(leaf? t)
    (set! RUNNING (+ 1 RUNNING))
    (define-values (new-tid ins outs)
      (build-new-leaf tid graph syscalls new-targets))
    (hash-set! ins-cache new-tid ins)
    (hash-set! outs-cache new-tid outs)
    (values new-tid ins outs)]
   [else
    (define-values (a ins outs)
      (build-new-non-leaf tid graph syscalls new-targets ins-cache outs-cache))
    (hash-set! ins-cache a ins)
    (hash-set! outs-cache a outs)
    (values a ins outs)]))


(define (build-new-makegraph graph syscalls)
  (define new-graph (create-makegraph))
  
  (printf "Building new makegraph.")

  (define-values (nroot _ __)
    (build-new-target (makegraph-root graph) graph syscalls (makegraph-targets new-graph) (make-hash) (make-hash)))
  
  (unless nroot
	  (error 'build-new-graph "Did not create new root target!"))
  
  (set-makegraph-root! new-graph nroot)
  new-graph)
