#lang errortrace racket/base

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

(define (run-graph-dry bgraph)
  (for ([graph (buildgraph-makegraphs bgraph)])
    (run-makegraph-dry graph)))

(define (run-makegraph-dry graph)
  (define already-run (make-hash))

  (define (run-target-dry tid)
    (define t (hash-ref (makegraph-targets graph) tid))
    (cond
     [(and (hash-ref already-run tid #f)
     	   (not (target-phony? t))
	   (not (equal? 'name (target-type t))))
      (printf "Not running target ~a again.\n" tid)]
     [(empty? (target-out-edges t))
      (run-leaf-dry t)
      (hash-set! already-run tid #t)]	
     [else

      (hash-set! already-run tid #t) ;; doing here in case infinite loop
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
  
  (run-target-dry (makegraph-root graph)))

;; graph -> error?
(define (run-graph-sequential bgraph)
  (for ([graph (buildgraph-makegraphs bgraph)])
    (run-makegraph-sequential graph)))

(define (run-makegraph-sequential graph)
  (define already-run (make-hash))

  (define (run-target-sequential tid)
    (define t (hash-ref (makegraph-targets graph) tid))
    (cond
      [(and (hash-ref already-run tid #f)
     	    (not (target-phony? t))
	    (not (equal? 'name (target-type t))))
      (printf "Not running target ~a again.\n" tid)]
      [(empty? (target-out-edges t))
       (run-leaf-sequential t)
       (hash-set! already-run tid #t)]
      [else
       (define-values (deps recipes)
         (separate-edges (target-out-edges t)))
     
       (printf "Running dependencies for target <~a,~a>\n" (target-name t) (target-mfile t))

       (for ([d (shuffle deps)]) ;; run these in strange order to stress test
         (run-target-sequential (edge-end d)))

       (printf "Running recipes for target <~a,~a>\n" (target-name t) (target-mfile t))
     
       (for ([r (reverse recipes)])
         (run-target-sequential (edge-end r)))
 
       (hash-set! already-run tid #t)]))

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

      (printf "Running CMD: ~a ; PID: ~a\n" (rusage-data-id (target-data t)) (rusage-data-pid (target-data t)))
      (cond
       [(system new-cmd #:set-pwd? #t)
        (printf "CMD: ~a succeeded.\n" new-cmd)]
       [else
        (error 'run-leaf-sequential "Command failed ~a" new-cmd)])]
     [else
      (printf "Did not run leaf target <~a,~a> because there was no command.\n" (target-name t) (target-mfile t))]))


  (run-target-sequential (makegraph-root graph)))