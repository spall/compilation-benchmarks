#lang racket/base

(require racket/cmdline
         "make2graph.rkt"
         "rusage-parser.rkt")

(define create-dotfile? (make-parameter #f))
(define rusage? (make-parameter #f))
(define nd? (make-parameter #f))

(define file-path
  (command-line
   #:once-each
   [("-d" "--dotfile") df
    "Produce a graphviz dot file"
    (create-dotfile? df)]
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

(cond
  [(rusage?)
   (parse-rusage file-path)]
  [(nd?)
   (parse-dry-run file-path)]
  [else
   (error 'driver "Expected either '--rusage-data' or '--dry-run-output' flags")])
