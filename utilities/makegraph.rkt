#lang errortrace racket

(require racket/struct
	 "flags.rkt")

(provide (struct-out target)
	 (struct-out targetid)
         (struct-out edge)
         (struct-out makegraph)
         (struct-out rusage-data)
	 (struct-out buildgraph)

	 create-buildgraph
         create-makegraph
         create-target
	 create-targetid
         create-rusage-data
	 add-makegraph
	 get-target
         add-recipe
         add-dependency
         add-edge
         remove-edge
         all-fields-set?
         add-target-to-makegraph
         target-in-graph?)

(define EDGE 0) ;; edge counter. to create ordering on edges
(define (get-edge-id)
  (begin0 EDGE
    (set! EDGE (- EDGE 1))))
(define TID 0)
(define (get-tid)
  (begin0 TID
    (set! TID (+ 1 TID))))

;; ----------- Node to represent makefile target ------------------
(define (target-print t port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))
  
  (write (target-id t) port)
  (write-string ", " port)
  (write (target-name t) port)
  (write-string ", " port)
  (write (target-mfile t) port)
  (write-string "\n" port)
  (write-string "target type: " port)
  (write (target-type t) port)
  (write-string "\nDependencies and recipes:\n" port)
  (for ([d (target-out-edges t)])
    (recur d port)
    (write-string "\n" port))
  (when mode (write-string ">" port)))

;; name is name of target
;; makefile is absolute path + name of makefile this target was executed from
;; need this info because different makefiles of different names in same directory 
;; can have targets with same name
(struct targetid (name mfile version) #:mutable #:transparent)

(define (create-targetid name mfile version)
  (targetid name mfile version))

;; out edges are recipes and deps
(struct target (id name mfile phony? type out-edges data)
  #:methods gen:custom-write
  [(define write-proc target-print)]
  #:mutable #:transparent)

(define (create-target name)
  (target (get-tid)
          name
          #f       ; mfile
	  #f       ; phony?
	  'unknown ; type 
          '()      ; out-edges
          '()))    ; data

(define (edge-print e port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))

  (write (edge-id e) port)
  (write-string "\n" port)
  (write-string (format "type ~a End: " (edge-type e)) port)
  (recur (edge-end e) port)
  (when mode (write-string ">" port)))

(struct edge (end type id) ;; end is targetid
  #:methods gen:custom-write
  [(define write-proc edge-print)]
  #:mutable #:transparent)

(define (insert-edge e ls)
  (cond
    [(empty? ls)
     (list e)]
    [(< (edge-id e) (edge-id (car ls)))
     (cons e ls)]
    [else
     (cons (car ls)
           (insert-edge e (cdr ls)))]))

(define (remove-edge t e)
  (define (helper ls)
    (cond
      [(empty? ls)
       ls]
      [(equal? e (car ls))
       (cdr ls)]
      [else
       (cons (car ls) (helper (cdr ls)))]))
  
  (set-target-out-edges! t (helper (target-out-edges t))))

(define (add-edge t e [t2 #f])
  (when t2
    (set-edge-type! e t2))

  (set-target-out-edges! t (insert-edge e (target-out-edges t))))

;; adds a recipe edge
(define (add-recipe add-to tid)
  (set-target-out-edges! add-to (cons (edge tid 'seq (get-edge-id)) 
  			 	      (target-out-edges add-to))))

;; adds a dependency edge
(define (add-dependency add-to tid)
  (set-target-out-edges! add-to (cons (edge tid 'dep (get-edge-id))
  			 	      (target-out-edges add-to))))

;; ----------------- End of target code ------------------------------

;; ----------------- graph to represent makefile graph ---------------
(define (makegraph-print mg port mode)
  (when mode (write-string "<" port))
  (define recur (case mode
                  [(#t) write]
                  [(#f) display]
                  [else (lambda (p port) (print p port mode))]))
  (write-string "Root: " port)
  (recur (makegraph-root mg) port)
  (write-string "\n" port)
  (write-string "Targets: \n" port)
  (for ([t (in-hash-values (makegraph-targets mg))])
    (recur t port)
    (write-string "\n"))
  (when mode (write-string ">" port)))

(struct makegraph (targets root)
  #:methods gen:custom-write
  [(define write-proc makegraph-print)]
  #:mutable #:transparent)

(define (create-makegraph [root #f])
  (makegraph (make-hash) root))

(define (fake-target? t)
  (and (string-prefix? (target-name t) "FAKE")
       (equal? (target-mfile t) "top")))

(define (same-shcall? t1 t2)
  (and (empty? (target-out-edges t1))
       (empty? (target-out-edges t2))
       (rusage-data? (target-data t1))
       (rusage-data? (target-data t2))
       (equal? (rusage-data-cmd (target-data t1))
	       (rusage-data-cmd (target-data t2)))))

(define (add-target-to-makegraph graph tid t cv)
  (define targets (makegraph-targets graph))
  
  (define (has-edge? t1 e)
    (ormap (lambda (e2)
	     (and (equal? (edge-type e) (edge-type e2))
		  (same-target? (get-target graph (edge-end e)) 
				(get-target graph (edge-end e2)))))
 	   (target-out-edges t1)))
  
  (define (has-edge-2? t e)
    (ormap (lambda (e2)
	     ;; both are different versions of the same target
	     ;; both are shcalls
	     ;; or regular case of same-target
	     (define tid1 (edge-end e))
	     (define tid2 (edge-end e2))
	     (or (and (equal? (targetid-name tid1) (targetid-name tid2))
		      (equal? (targetid-mfile tid1) (targetid-mfile tid2)))
		 (same-shcall? (get-target graph tid1)
			       (get-target graph tid2))
		 (same-target? (get-target graph tid1)
			       (get-target graph tid2))))
	   (target-out-edges t)))


  (define (compare-targets t1 t2)
    (for-each (lambda (e1)
		(unless (has-edge-2? t2 e1)
			(error 'compare-targets "Newer target <~a,~a> doesn't have edge ~a" (target-name t1) (target-mfile t1) e1)))
	      (target-out-edges t1)))

  ;; how do we define the idea of "same" target?
  ;; exactly the same?
  ;; subset the same?
  ;; 1. exactly the same
  ;; 2. newest target is a "subset" of the old target

  ;; t1 and t2 are target objects
  (define (same-target? t1 t2)
    (cond
     [(equal? t1 t2) ;; exactly the same
      #t]
     [(and (fake-target? t1) (fake-target? t2))
      (andmap (lambda (e)
		(cond
		 [(has-edge? t1 e)
		  #t]
		 [else
		  (when (debug?)
			(printf "Did not find edge ~a from target <~a,~a> in target <~a,~a>\n"
				e (target-name t2) (target-mfile t2) (target-name t1) (target-mfile t1)))
		  #f]))
	      (target-out-edges t2))]
     [else
      #f]))

  (define (same-target-driver? t1 t2)
    (cond
     [(equal? t1 t2)
      #t]
     [(or (and (equal? (target-name t1) (target-name t2))
	       (equal? (target-mfile t1) (target-mfile t2)))
	  (and (fake-target? t1) (fake-target? t2)))
      (andmap (lambda (e)
		(cond
		 [(has-edge? t1 e)
		  #t]
		 [else
		  (when (debug?)
			(printf "Did not find edge ~a from target <~a,~a> in target <~a,~a>\n"
				e (target-name t2) (target-mfile t2) (target-name t1) (target-mfile t1)))
		  #f]))
	      (target-out-edges t2))]
     [else
      #f]))

  (define tmptid (create-targetid (targetid-name tid) (targetid-mfile tid) cv))
  (cond
   [(hash-ref targets tmptid #f) =>
    (lambda (tmp)
      ;; there is already a target object with cv. So determine if these are the same version 
      ;; or if this is a newer version of the target.
      (cond
       [(or (empty? (target-out-edges t)) ;; same version
	    (same-target-driver? tmp t))
	cv]
       [else
	;; print differences if there are any in case important edges are missing from new target and we need to copy them.
	(compare-targets tmp t)
	(set-targetid-version! tmptid (+ 1 cv))
	(hash-set! targets tmptid t)
	(+ 1 cv)]))]
   [else
    (when (equal? "'cs.cp1250.po" (targetid-name tid))
	  (printf "adding target with value ~a\n" cv))
    (hash-set! targets tmptid t)
    cv]))

(define (get-target graph tid)
  (hash-ref (makegraph-targets graph) tid))

(define (target-in-graph? graph tid)
  (hash-ref (makegraph-targets graph) tid #f))
 
;; -------------- end make graph code --------------------------

;; -------------- start build graph code -----------------------

(struct buildgraph (makegraphs) #:mutable #:transparent)

(define (create-buildgraph)
  (buildgraph '()))

(define (add-makegraph bgraph mgraph)
  (set-buildgraph-makegraphs! bgraph (append (buildgraph-makegraphs bgraph) (list mgraph))))

;; -------------- end build graph code -------------------------

;; -------------- structure to represent rusage data -----------

(define (rusage-data-print rd port mode)
  (when mode (write-string "<" port))
  (write-string (format "cmd: ~a" (rusage-data-cmd rd))  port)
  (when mode (write-string ">" port)))

(struct rusage-data (id pid cmd dir rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata submake?)
  #:methods gen:custom-write
  [(define write-proc rusage-data-print)]
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data -1 #f cmd #f #f #f #f #f #f #f #f #f #f #f #f #f #f #f))

(define (all-fields-set? rds)
  (andmap identity
          (reverse (cdr (reverse (struct->list rds))))))

;; ---------------- end of rusage data code --------------------
