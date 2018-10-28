#lang racket/base

(require "makegraph.rkt"
         racket/list
	 racket/string
         racket/system
	 parser-tools/lex
	 (prefix-in : parser-tools/lex-sre))

(provide run-graph-sequential
	 run-graph-dry)

(define SHELL "/bin/sh -c")

(define (separate-edges edges)
  (cond
    [(empty? edges)
     (values '() '())]
    [else
     (call-with-values (lambda () (separate-edges (cdr edges)))
                       (lambda (ds rs)
                         (if (equal? (edge-type (car edges)) 'dep)
                             (values (cons (car edges) ds) rs)
                             (values ds (cons (car edges) rs)))))]))

(define (run-graph-dry graph)
  (run-target-dry (makegraph-root graph)))

(define (run-target-dry t)
  (cond
   [(empty? (target-out-edges t))
    (run-leaf-dry t)]
   [else
    (printf "Running non-leaf target <~a,~a>\n" (target-name t) (target-mfile t))

    (define-values (deps recipes)
      (separate-edges (target-out-edges t)))

    (printf "Running dependencies for target <~a,~a>\n" (target-name t) (target-mfile t))

    (for ([d (reverse deps)]) ;; reversing dependencies is the original order they were run make
      (run-target-dry (edge-end d)))

    (printf "Running recipes for target <~a,~a>\n" (target-name t) (target-mfile t))

    (for ([r (reverse recipes)]) ;; must reverse recipes to get correct order
      (run-target-dry (edge-end r)))]))

(define (run-leaf-dry t)
  (printf "running leaf target <~a,~a>\n" (target-name t) (target-mfile t))

  (if (rusage-data? (target-data t))
      (printf "Would run command : ~a ;  ~a\n" (rusage-data-id (target-data t))
      	      	     	 	      	       (rusage-data-cmd (target-data t)))
      (printf "Leaf target does not have an associated command\n")))

;; graph -> error?
(define (run-graph-sequential graph)
  (run-target-sequential (makegraph-root graph)))

(define (run-target-sequential t)
  (cond
    [(empty? (target-out-edges t))
     (run-leaf-sequential t)]
    [else
     (define-values (deps recipes)
       (separate-edges (target-out-edges t)))
     
     (printf "Running dependencies for target <~a,~a>\n" (target-name t) (target-mfile t))

     (for ([d (shuffle deps)]) ;; run these in strange order to stress test
       (run-target-sequential (edge-end d)))

     (printf "Running recipes for target <~a,~a>\n" (target-name t) (target-mfile t))
     
     (for ([r (reverse recipes)])
       (run-target-sequential (edge-end r)))]))

(define (fix-cmd cmd)
  (cond
   [(string-prefix? cmd SHELL) 
    (string-trim (string-trim cmd SHELL))]
   [else
    cmd]))

;; what about current directory
(define (run-leaf-sequential t)

  ;; so set directory before running command
  (cond
   [(rusage-data? (target-data t))
    (current-directory (rusage-data-dir (target-data t)))

    (define new-cmd (fix-cmd (rusage-data-cmd (target-data t))))

    (printf "Running CMD: ~a\n" (rusage-data-id (target-data t)))
    (cond
     [(system new-cmd #:set-pwd? #t)
      (printf "CMD: ~a succeeded.\n" new-cmd)]
     [else
      (error 'run-leaf-sequential "Command failed ~a" new-cmd)])]
   [else
    (printf "Did not run leaf target <~a,~a> because there was no command.\n" (target-name t) (target-mfile t))]))