#lang errortrace racket/base

(require racket/list
         racket/string
	 racket/sequence
	 racket/hash
         "makegraph.rkt"
         "flags.rkt"
         "process-syscalls.rkt"
	 "work.rkt"
	 "span.rkt"
	 "common.rkt")

(provide create-dotfile-string
         print-all-targets-and-mfiles
         work
         span
	 build-span-graph
	 top-n-span
         parallel-slackness
         predicted-speed-upper
         predicted-speed-lower
	 predicted-speed-perfect-linear
         print-graph
	 print-targets-most-to-least-build
	 print-make-targets-most-to-least-build
	 print-intersections-of-build)

(define (print-intersections-of-target t graph syscalls)
  (cond
   [(leaf? t) ;; leaf
    (define data (target-data t))
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
      (values '() '())])] ;; we don't know
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
	(define t2-cmd (if (rusage-data? (target-data t2))
			   (rusage-data-cmd (target-data t2))
			   ""))
	(if outs
	    (when lastt
	    	  (define lastt-cmd (if (rusage-data? (target-data lastt))
					(rusage-data-cmd (target-data lastt))
					""))
		  (cond
		   [last-ins
		    (define in (intersection outs last-ins))
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

#|  print the intersections of every target in the graph
|#
(define (print-intersections-of-graph g syscalls)
  (printf "Printing intersections of root target\n\n")
  (print-intersections-of-target (get-target g (makegraph-root g)) g syscalls))

#|  print the intersections of every graph in build
|#
(define (print-intersections-of-build b syscalls)
  (for ([mg (buildgraph-makegraphs b)])
       (print-intersections-of-graph mg syscalls)))


(define (print-make-targets-most-to-least-graph g total)
  (define targets (makegraph-targets g))
  (define ls (for/fold ([ls '()])
		       ([t (in-hash-values targets)])
		       ;; caculate span of target that aren't shellcalls
		       (cond
			[(leaf? t)
			 ls]
			[else
			 (define s (work-of-target t g (hash) #t))
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
			[(rusage-data? (target-data t))
			 (rusage-data-cmd (target-data t))]
			[else
			 "No command saved as run."]))
       (when ttime
	     (define prctg (* 100 (exact->inexact (/ ttime total))))
	     (printf "Target <~a,~a> is ~a% of total: ran ~a.\n\n" (target-name t) (target-mfile t) prctg cmd-run))))

#|  returns the targetid of the most expensive target in the build
|#
(define (most-expensive-target build [subs (hash)])
  (define-values (_ tid) (for/fold ([max 0]
				    [maxtid #f])
				   ([mg (buildgraph-makegraphs build)])
				   (define-values (w me) (most-expensive-target-graph mg subs))
				   (if (> w max)
				       (values w me)
				       (values max maxtid))))
  tid)


#|  returns the targetid of the most expensive target in the makegraph
|#
(define (most-expensive-target-graph graph [subs (hash)])
  (define targets (makegraph-targets graph))
  (define ls (for/fold ([ls '()])
		       ([tid (in-hash-keys (makegraph-targets graph))])
		       (define t (hash-ref (makegraph-targets graph) tid))
		       (cond
			[(leaf? t)
			 ls]
			[else
			 (define s (work-of-target tid graph subs))
			 (cons (cons s tid) ls)])))
  
  ;; sort ls
  (define sorted (sort ls > #:key car))
  ;; return first thing
  (if (empty? sorted)
      (error 'most-expensive-target-graph "No non-leaf targets in graph\n")
      (values (caar sorted)
	      (cdar sorted))))

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
  
