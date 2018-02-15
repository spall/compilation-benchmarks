#lang racket/base

(require racket/cmdline
         "make2graph.rkt"
         "rusage-parser.rkt"
         "makegraphfunctions.rkt"
         "makegraph.rkt")

(define create-dotfile? (make-parameter #f))
(define rusage? (make-parameter #f))
(define nd? (make-parameter #f))
(define time-target? (make-parameter #f))

(define file-path
  (command-line
   #:once-each
   [("-d" "--dotfile") df
    "Produce a graphviz dot file"
    (create-dotfile? df)]
   [("-t" "--target-time") tname
    "Print target time for target with provided name"
    (time-target? tname)]
   ;; add other things here.
   #:once-any
   [("-r" "--rusage-data")
    "Parse make output with rusage data"
    (rusage? #t)]
   [("-n" "--dry-run-output")
    "Parse output from 'make -nd'"
    (nd? #t)]
   ;; add other things here.
   #:args (path)
   path))

(define graph
  (cond
    [(rusage?)
     (parse-rusage file-path)]
    [(nd?)
     (parse-dry-run file-path)]
    [else
     (error 'driver "Expected either '--rusage-data' or '--dry-run-output' flags")]))

(when (time-target?)
  (let ([t (get-target graph (time-target?))])
    (if t
        (printf "Time for target ~a is ~a\n" (target-name t) (calculate-target-time t))
        (printf "Couldn't find target ~a in graph\n" (time-target?)))))

(when (create-dotfile?)
  (define fp (open-output-file (create-dotfile?) #:mode 'text #:exists 'replace))
  (write (create-dotfile-string graph) fp)
  (printf "Wrote graph to ~a\n" (create-dotfile?))
  (close-output-port fp))
