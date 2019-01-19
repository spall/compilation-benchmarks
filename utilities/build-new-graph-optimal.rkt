#lang errortrace racket/base

(require racket/list
         racket/sequence
	 racket/string
	 "makegraph.rkt"
	 "makegraphfunctions.rkt"
	 "process-syscalls.rkt"
	 "flags.rkt")

(provide build-new-graph)

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

(struct inode (tid ins outs in-edges out-edges) #:mutable)
(struct dnode (tids ins outs out-edges) #:mutable)

(define (build-intersection-graph edges graph syscalls new-targets ins-cache outs-cache tname)
  
  (define igraph (make-hash))
  
  (define-values (root reps)
    (let-values ([(tids ins outs reps)
		  (let split ([es edges])
		    (cond
		     [(empty? es)
		      (values '() '() '() '())]
		     [else
		      (call-with-values (lambda () (split (cdr es)))
			(lambda (ts is os rs)
			  (define t (edge-end (car es)))
			  (define-values (ins outs)
			    (build-new-target t graph syscalls new-targets ins-cache outs-cache))
			  (cond
			   [(equal? 'dep (edge-type (car es)))
			    (values (cons t ts) 
				    (and ins is (append ins is))
				    (and outs os (append outs os)) rs)]
			   [else
			    (hash-set! igraph t (inode t ins outs (make-hash) (make-hash)))
			    (values ts is os (cons (car es) rs))])))]))])
      (values (dnode tids ins outs '()) reps)))
	
  ;; now construct the graph
  (let construct-igraph ([rs reps]) ;; want to go through reps in reverse order.
    (unless (empty? rs)
	    (compare (car rs) (cdr rs) igraph root)
	    (construct-igraph (cdr rs))))

  ;; create ordering based on graph
  
  ;; find inodes with no in-edges
  (define ndeps (let loop ([ins (sequence->list (in-hash-values igraph))])
		  (cond
		   [(empty? ins)
		    '()]
		   [else
		    (define in (car ins))
		    (if (hash-empty? (inode-in-edges in))
			(cons (inode-tid in) (loop (cdr ins)))
			(loop (cdr ins)))])))
		   

  (define l0 (append ndeps (dnode-tids root)))

  (define levels (make-hash)) ;; tid -> level
  
  (define (levels-bfs r l)
    (cond
     [(hash-ref levels (inode-tid r) #f) =>
      (lambda (cl) ;; current-level
	(when (> l cl)
	      (hash-set! levels (inode-tid r) l)))]
     [else
      (hash-set! levels (inode-tid r) l)])

    (for ([oe (in-hash-keys (inode-out-edges r))])
	 (levels-bfs (hash-ref igraph oe) (+ 1 l))))


  (for ([oe (dnode-out-edges root)])
       (levels-bfs (hash-ref igraph oe) 1))
  ;; repeat for each thing in new deps
  (for ([ndep ndeps])
       (for ([oe (in-hash-keys (inode-out-edges (hash-ref igraph ndep)))])
	    (levels-bfs (hash-ref igraph oe) 1)))
  
  (define levels-ls (make-hash)) ;; level -> list of tids
  ;; make sure each target is only at the lowest level it appears
  (for ([p (in-hash-pairs levels)])
       (define tid (car p))
       (define l (cdr p))
       (cond
	[(hash-ref levels-ls l #f) =>
	 (lambda (ls)
	   (hash-set! levels-ls l (cons tid ls)))]
	[else
	 (hash-set! levels-ls l (list tid))]))

  (define ls (let loop ([i 1])
	       (cond
		[(hash-ref levels-ls i #f) =>
		 (lambda (ls_)
		   (cons ls_ (loop (+ 1 i))))]
		[else
		 '()])))
    
  (define-values (all-ins all-outs)
    (for/fold ([ins (dnode-ins root)]
	       [outs (dnode-outs root)])
	      ([in (in-hash-values igraph)])
	      (values (and (inode-ins in) ins (append ins (inode-ins in)))
		      (and (inode-outs in) outs (append outs (inode-outs in))))))

  (for ([l l0])
       (when (edge? l)
	     (error 'build-graph "Error ~a is an edge\n" l)))
  (for ([l ls])
       (when (edge? l)
	     (error 'build-graph2 "Error ~a is an edge\n" l)))

  (values l0 ls all-ins all-outs))

(define (compare e es igraph root)
  (cond
   [(empty? es)
    ;; compare to root
    (define in1 (hash-ref igraph (edge-end e)))
    (when (or (not (inode-ins in1)) (not (dnode-outs root))
	      (intersect? (dnode-outs root) (inode-ins in1))
	      (intersect? (dnode-ins root) (inode-outs in1)))
	  ;; create an edge from root to in1
	  (set-dnode-out-edges! root (cons (edge-end e) (dnode-out-edges root)))
	  (hash-set! (inode-in-edges in1) 'root #t))]
   [else
    ;; compare e to first thing in es
    (define in1 (hash-ref igraph (edge-end e)))
    (define in2 (hash-ref igraph (edge-end (car es))))
    
    (when (or (not (inode-ins in1)) (not (inode-outs in2))
	      (intersect? (inode-ins in1) (inode-outs in2))
	      (intersect? (inode-outs in1) (inode-ins in2)))
	  ;; create an edge from in2 to in1
	  (hash-set! (inode-out-edges in2) (edge-end e) #t)
	  (hash-set! (inode-in-edges in1) (edge-end (car es)) #t))
    (compare e (cdr es) igraph root)]))

  
(define (build-new-non-leaf tid graph syscalls new-targets ins-cache outs-cache)
  
  ;; 4. create copy of target
  (define t (get-target graph tid))
  (define nt (target (target-id t) (target-name t) (target-mfile t) (target-phony? t) (target-type t) '() (list #f)))

  	
  (define-values (new-deps new-recipes ins outs) (build-intersection-graph (target-out-edges t) graph syscalls new-targets ins-cache outs-cache (targetid-name tid)))

  ;; have new deps if there are any and have new recipe ordering
  (for ([nd new-deps])
       (add-dependency nt nd))

  (for ([nrs new-recipes])
       (unless (empty? nrs)
	       (cond
		[(= 1 (length nrs))
		 (add-recipe nt (car nrs))]
		[else
		 (define tmpid (create-targetid (symbol->string (gensym "FAKE")) (target-mfile nt) 0))
		 (define tmp (create-target (targetid-name tmpid)))
		 (set-target-mfile! tmp (target-mfile nt))
		 (for ([nr nrs])
		      (add-dependency tmp nr))
		 (hash-set! new-targets tmpid tmp)
		 (add-recipe nt tmpid)])))

  ;; done
  (hash-set! new-targets tid nt)

  (when (and (empty? (target-out-edges nt)) (debug?))
	(printf "this non leaf node <~a,~a> is now a leaf\n" (target-name nt) (target-mfile nt)))

  (values ins outs))

(define (leaf? t)
  (empty? (target-out-edges t)))

(define (build-new-graph bgraph syscalls)
  (define new-bgraph (create-buildgraph))
  (for ([graph (buildgraph-makegraphs bgraph)])
       (add-makegraph new-bgraph (build-new-makegraph graph syscalls)))
  new-bgraph)

  (define (fix-cmd cmd)
    (cond
     [(string-prefix? cmd SHELL) 
      (string-trim (string-trim cmd SHELL))]
     [else
      cmd]))

;; Returns ins and outs of target; and creates a copy of target and puts it in new-targets
(define (build-new-leaf tid graph syscalls new-targets)  
  ;; 4. create copy of target
  (define t (get-target graph tid))
  
  (hash-set! new-targets tid
    	     (target (target-id t) (target-name t) (target-mfile t) 
		     (target-phony? t) (target-type t) '() (target-data t)))

  (define data (target-data t))
  (cond
   [(not (rusage-data? data))
    (values '() '())]
   [(hash-ref syscalls (rusage-data-pid data) #f)
    (process-in-out-pid (rusage-data-pid data) (rusage-data-dir data) syscalls (fix-cmd (rusage-data-cmd data)))]
   [else ;; no info
    (values #f #f)]))

(define (build-new-target tid graph syscalls new-targets ins-cache outs-cache)
  (define t (get-target graph tid))
  (cond
   [(hash-ref new-targets tid #f) ;; we have already rebuilt this target so do nothing
    (values (hash-ref ins-cache tid) (hash-ref outs-cache tid))]
   [(leaf? t)
    (define-values (ins outs)
      (build-new-leaf tid graph syscalls new-targets))
    (hash-set! ins-cache tid ins)
    (hash-set! outs-cache tid outs)
    (values ins outs)]
   [else
    (define-values (ins outs)
      (build-new-non-leaf tid graph syscalls new-targets ins-cache outs-cache))
    (hash-set! ins-cache tid ins)
    (hash-set! outs-cache tid outs)
    (values ins outs)]))


(define (build-new-makegraph graph syscalls)
  (define new-graph (create-makegraph))
  (set-makegraph-root! new-graph (makegraph-root graph))

  (printf "Building new makegraph.\n")

  (build-new-target (makegraph-root graph) graph syscalls (makegraph-targets new-graph) (make-hash) (make-hash))
  new-graph)
