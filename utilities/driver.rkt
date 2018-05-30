#lang racket/base

(require racket/cmdline
         racket/list
         ;"make2graph.rkt"
         "rusage-parser.rkt"
         ;"parse-deps.rkt"  ;; change name of this file?
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
   
   (define-values (sum predictions len)
     (for/fold ([sum 0]
                [preds '()]
                [len 0])
               ([ru-file (in-directory dir-path)])
       (let ([graph (parse-rusage ru-file)])
         (let ([work_ (work (makegraph-root graph) graph)])
           (values (+ sum work_)
                   (append (for/list ([tc (in-range 1 33)])
                             (list tc (predicted-speed-upper graph tc work_)
                                   (predicted-speed-lower graph tc work_)))
                           preds)
                   (+ 1 len))))))
   
   ;; average predictions
   (for ([tc (in-range 1 33)])
     (let ([upred (exact->inexact
                   (/ (apply +
                             (map cadr (filter (lambda (p)
                                                 (equal? tc (car p)))
                                               predictions)))
                      len))]
           [lpred (exact->inexact
                   (/ (apply + (map caddr (filter (lambda (p)
                                                    (equal? tc (car p)))
                                                  predictions)))
                      len))])
       ;; TODO: write result to file
       (printf "~a, ~a, ~a\n" tc upred lpred)))
   
   (printf "Average work is ~a nanoseconds\n" (exact->inexact (/ sum len)))]
   
  [else
   (define graph
     (cond
       [(rusage?)
        (parse-rusage file-path)]
       [(nd?)
        (void)
        #;(parse-dry-run file-path)]
       [else
        (error 'driver "Expected either '--rusage-data' or '--dry-run-output' flags")]))
   
   ;(printf "going to verify graph edges\n")
   ;(verify-edges graph)
   ;(printf "Going to verify times in graph\n")
   ;(verify-edge-times graph)
     
   (define (parse-ts fp)
     (void))
   (define (build-module-graph a1 a2)
     (void))
   (define (populate-with-timing-info a1 a2)
     (void))
   
   (define rgraph
     (cond
       [(racket?)
        (let-values ([(tinfo cpaths ppaths) (parse-ts file-path)])
          (let ([g (build-module-graph cpaths ppaths)])
            (populate-with-timing-info g tinfo)
            g))]
       [else
        #f]))
   
   (when (work?)
     (define w (work (makegraph-root graph) graph))
     (printf "Work is ~a\n" w))
   
   (when (span?)
     (define s (span (makegraph-root graph) graph))
     (printf "span is ~a\n" s))
   
   (when (and (span?) (work?))
     (define parallelism (/ (work (makegraph-root graph) graph)
                            (span (makegraph-root graph) graph)))
     (printf "parallelism is ~a\n" parallelism))
   
   (when (pspeed?)
     (define p (predicted-speed-upper graph (string->number (pspeed?))))
     (printf "Predicted upper bound for ~a processors is ~a seconds.\n" (pspeed?) p))
   
   (when (slackness?)
     (define s (parallel-slackness graph (slackness?)))
     (printf "Parallel slackness for ~a processors is ~a\n" (slackness?) s))
   
   (when (create-dotfile?)
     (define fp (open-output-file (create-dotfile?) #:exists 'replace))
     (display (create-dotfile-string graph) fp)
     (printf "Wrote graph to ~a\n" (create-dotfile?))
     (close-output-port fp))
   ])
   
