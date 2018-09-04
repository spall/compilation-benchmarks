#lang errortrace racket

(require racket/cmdline
         racket/list
         "rusage-parser.rkt"
         "strace-parser.rkt"
         "process-syscalls.rkt"
         "makegraphfunctions.rkt"
         "makegraph.rkt")

(define create-dotfile? (make-parameter #f)) 
(define rusage? (make-parameter #f)) ;; is there rusage data?
(define rusage-dir? (make-parameter #f)) ;; is path a directory with rusage data files?
(define racket? (make-parameter #f)) ;; build graph of racket modules built during raco setup portion of build
(define nd? (make-parameter #f))
(define time-target? (make-parameter #f))
(define work? (make-parameter #f))
(define span? (make-parameter #f))
(define pspeed? (make-parameter #f))
(define slackness? (make-parameter #f))
(define strace? (make-parameter #f))
(define check-deps? (make-parameter #f))

(define file-path
  (command-line
   #:once-each
   [("-d" "--dotfile") df
    "Produce a graphviz dot file"
    (create-dotfile? df)]
   [("-t" "--target-time") tname
    "Print target time for target with provided name"
    (time-target? tname)]
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
   [("-m" "--modules-graph")
    "Build graph of racket modules build during build"
    (racket? #t)]
   [("--strace-data") stfile
    "Parse strace data and connect with build graph"
    (strace? stfile)]
   [("--check-dep")
    "Checks dependencies using strace data"
    (check-deps? #t)]
   ;; add other things here.
   #:once-any
   [("-r" "--rusage-data")
    "Parse make output with rusage data"
    (rusage? #t)]
   [("-n" "--dry-run-output")
    "Parse output from 'make -nd'"
    (nd? #t)]
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
         (let ([work_ (work (makegraph-root graph) graph)]
	       [span_ (span (makegraph-root graph) graph)]
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
   
   (define syscall-info (when (strace?)
                          (printf "Parsing strace\n")
                          (parse-strace (strace?))))
   
   (when (work?)
     (define w (work (makegraph-root graph) graph))
     (printf "Work is ~a\n" w))
   
   (when (span?)
     (define s (span (makegraph-root graph) graph))
     (printf "span is ~a\n" s))

   (when (check-deps?)
     (when (void? syscall-info)
       (error 'driver "No system call info"))
     (check-dependencies (makegraph-root graph) graph syscall-info))
   
   (when (and (span?) (work?))
     (define parallelism (/ (work (makegraph-root graph) graph)
                            (span (makegraph-root graph) graph)))
     (printf "parallelism is ~a\n" parallelism))
   
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
   
