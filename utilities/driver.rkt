#lang racket/base

(require racket/cmdline
         racket/list
         "make2graph.rkt"
         "rusage-parser.rkt"
         "makegraphfunctions.rkt"
         "makegraph.rkt")

(define create-dotfile? (make-parameter #f))
(define rusage? (make-parameter #f))
(define nd? (make-parameter #f))
(define time-target? (make-parameter #f))
(define span? (make-parameter #f))

(define file-path
  (command-line
   #:once-each
   [("-d" "--dotfile") df
    "Produce a graphviz dot file"
    (create-dotfile? df)]
   [("-t" "--target-time") tname
    "Print target time for target with provided name"
    (time-target? tname)]
   [("-s" "--span")
    "Print span for make"
    (span? #t)]
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
  (let ([ts (get-target graph (time-target?))])
    (cond
      [ts
       (let loop ([ts_ ts])
         (unless (empty? ts_)
           (let ([t (car ts_)])
             (printf "Time for target ~a in makefile ~a is ~a seconds.\n" (target-name t) (target-mfile t) (calculate-target-time t)))
           (loop (cdr ts_))))]
      [else
       (printf "Couldn't find target ~a in graph\n" (time-target?))])))

(when (span?)
  (define fakeroot (makegraph-root graph))
  (define realroot (get-target graph (car (target-children fakeroot))))
  (define s (span realroot graph))
  (printf "span is ~a\n" s))

(when (create-dotfile?)
  (define fp (open-output-file (create-dotfile?) #:exists 'replace))
  (display (create-dotfile-string graph) fp)
  (printf "Wrote graph to ~a\n" (create-dotfile?))
  (close-output-port fp))
