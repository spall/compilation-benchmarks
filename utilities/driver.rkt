#lang racket

(require racket/cmdline
         racket/list
         "rusage-parser.rkt"
         "strace-parser.rkt"
         "process-syscalls.rkt"
         "makegraphfunctions.rkt"
         "build-new-graph.rkt"
         "makegraph.rkt"
         "run-graph.rkt"
	 "flags.rkt")

(define create-dotfile? (make-parameter #f)) 
(define rusage? (make-parameter #f)) ;; is there rusage data?
(define rusage-dir? (make-parameter #f)) ;; is path a directory with rusage data files?
(define work? (make-parameter #f))
(define span? (make-parameter #f))
(define pspeed? (make-parameter #f))
(define slackness? (make-parameter #f))
(define strace? (make-parameter #f))
(define new-graph? (make-parameter #f))
(define run-new-graph? (make-parameter #f))
(define dry-run-graph? (make-parameter #f))
(define run-orig-graph? (make-parameter #f))

(define file-path
  (command-line
   #:once-each
   [("--debug") level
    "Turn on debug printing at provided level" ;; TODO add an explanation of the levels
    (debug? #t)]
   [("-d" "--dotfile") df
    "Produce a graphviz dot file"
    (create-dotfile? df)]
   [("-w" "--work")
    "Print work for make"
    (work? #t)]
   [("-s" "--span")
    "Print span for make"
    (span? #t)]
   [("--predicted-speed") pcount
    "Print predicted speed when using provided processor count"
    (pspeed? pcount)]
   [("--parallel-slackness") pcount
    "Calculates parallel slackness with provided processor count"
    (slackness? pcount)]
   [("--strace-data") stfile
    "Parse strace data and connect with build graph"
    (strace? stfile)]
   [("--new-graph")
    "rebuilds graph using strace data"
    (new-graph? #t)]
   [("--dry-run-graph")
    "prints commands that would be run when building graph" 
    (dry-run-graph? #t)]
   [("--run-orig-graph")
    "runs all shell commands in graph constructed from make data.  Helps verify graph was constructed correctly."
    (run-orig-graph? #t)]
   [("--run-new-graph")
    "rebuilds graph using strace data and then runs it"
    (run-new-graph? #t)]
   ;; add other things here.
   #:once-any
   [("-r" "--rusage-data")
    "Parse make output with rusage data"
    (rusage? #t)]
   [("--rusage-dir")
    "Provided path is to directory with rusage files in it"
    (rusage-dir? #t)]
   ;; add other things here.
   #:args (path)
   path))

(cond
  [(rusage-dir?)
   (define dir-path file-path)
   (unless (directory-exists? dir-path)
     (error 'driver "~a is not a directory path" dir-path))
   
   ;; calculate work and sum them
   (define-values (wsum ssum lsum len)
     (for/fold ([wsum 0]
     	        [ssum 0]
		[lsum 0]
     	        [len 0])
	       ([ru-file (in-directory dir-path)])
       (let ([graph (parse-rusage ru-file)])
         (let ([work_ (work graph)]
	       [span_ (span graph)]
	       [leaf_ (longest-leaf graph)])
	 (printf "~a ~a ~a\n" work_ span_ leaf_)
	 (values (+ wsum work_) (+ ssum span_) (+ lsum leaf_) (+ 1 len))))))
           
   ;; average predictions
   (define avg-work (/ wsum len))
   (define avg-span (/ ssum len))
   (define avg-leaf (/ lsum len))
   (printf "pcount, upper, lower, leaf-upper, leaf-lower, perfect\n")
   (for ([tc (in-range 1 129)])
     (let ([upred (predicted-speed-upper #f tc avg-work avg-span)]
     	   [lpred (predicted-speed-lower #f tc avg-work avg-span)]
	   [uleaf (predicted-speed-upper #f tc avg-work avg-leaf)]
	   [lleaf (predicted-speed-lower #f tc avg-work avg-leaf)]
	   [plspeed (predicted-speed-perfect-linear #f tc avg-work)])
       (printf "~a, ~a, ~a, ~a, ~a, ~a\n" tc upred lpred uleaf lleaf plspeed)))
   
   (printf "Average work is ~a nanoseconds\n" (exact->inexact (/ wsum len)))]
  [else
   (define graph
     (cond
       [(rusage?)
        (parse-rusage file-path)]
       [else
        (error 'driver "Expected '--rusage-data' flag")]))

   (when (dry-run-graph?)
	 (printf "Doing a dry run of original graph.\n")
	 (run-graph-dry graph))

   (when (run-orig-graph?)
	 (printf "Running original graph.\n")
	 (run-graph-sequential graph))
   
   (define syscall-info 
     (when (strace?)
       (printf "Parsing strace\n")
       (parse-strace (strace?))))
   
   (define work_ (if (work?)
                     (work graph)
		     #f))
   (when work_
     (printf "Work for original graph is ~a\n" work_))
   
   (define span_ (if (span?)
                     (span graph)
		     #f))
   (when span_
     (printf "Span for original graph is ~a\n" span_))
   
   (define parallelism_ (if (and work_ span_)
     	                    (/ work_ span_)
			    #f))
   (when parallelism_
     (printf "Parallelism for original graph is ~a\n" parallelism_))

   (when (or (new-graph?) (run-new-graph?))
	 ;; set to be the directory of the top level make
	 ;; TODO: allow user to provide more directories
	 (unless (empty? (buildgraph-makegraphs graph))
		 (PROJ-DIR (targetid-mfile (makegraph-root (car (buildgraph-makegraphs graph))))))
	  
     (define new-graph (build-new-graph graph syscall-info))

     (define nwork_ (if (work?)
     	     	        (work new-graph)
			#f))
     (when nwork_
       (printf "Work for new graph is ~a\n" nwork_))


     (define nspan_ (if (span?)
     	     	    	(span new-graph)
			#f))
     (when nspan_
       (printf "Span for new graph is ~a\n" nspan_))

     (define nparallelism_ (if (and nwork_ nspan_)
     	     		       (/ nwork_ nspan_)
			       #f))
     (when nparallelism_
       (printf "Parallelism for new graph is ~a\n" nparallelism_))


     (when (run-new-graph?)
       (printf "Running new graph\n")
       (run-graph-sequential new-graph)))
   
   (when (pspeed?)
     (define pu (predicted-speed-upper graph (string->number (pspeed?))))
     (define pl (predicted-speed-lower graph (string->number (pspeed?))))
     (define pp (predicted-speed-perfect-linear graph (string->number (pspeed?))))
     (printf "Predicted upper bound for ~a processors is ~a seconds; lower bound ~a seconds; perfect linear speed ~a seconds.\n" (pspeed?) pu pl pp))
   
   (when (slackness?)
     (define s (parallel-slackness graph (slackness?)))
     (printf "Parallel slackness for ~a processors is ~a\n" (slackness?) s))
   
   (when (create-dotfile?)
     (define fp (open-output-file (create-dotfile?) #:exists 'replace))
     (display (create-dotfile-string graph) fp)
     (printf "Wrote graph to ~a\n" (create-dotfile?))
     (close-output-port fp))
   ])
   
