#lang racket

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
(struct targetid (name mfile) #:mutable #:transparent)

(define (create-targetid name mfile)
  (targetid name mfile))

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

(struct edge (end type id) ;; end is name?
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
  (for ([t (makegraph-targets mg)])
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

(define (shcall-target? t)
  (and (string-prefix? (target-name t) "SHCALL")
       (equal? (target-mfile t) "top"))) 

(define (add-target-to-makegraph graph tid t)
  (define targets (makegraph-targets graph))
  
  (define (has-edge? t1 e)	  
    (ormap (lambda (e2)
	     (and (equal? (edge-type e) (edge-type e2))
		  (same-target? (get-target graph (edge-end e)) 
				(get-target graph (edge-end e2)))))
 	   (target-out-edges t1)))

  ;; how do we define the idea of "same" target?
  ;; exactly the same?
  ;; subset the same?
  ;; 1. exactly the same
  ;; 2. newest target is a "subset" of the old target
  ;; 3. What about the case where we merge them?
  ;;    If we merge them it is because the NEWEST target has more dependnecies than 
  ;;    the old target because more dependencies were created while make was running

  ;; t1 and t2 are target objects
  (define (same-target? t1 t2)
    (cond
     [(equal? t1 t2) ;; exactly the same
      #t]
     [(and (shcall-target? t1) (shcall-target? t2)) ;; might be same shell-command
      (define d1 (target-data t1))
      (define d2 (target-data t2))
      (and (equal? (rusage-data-cmd d1) (rusage-data-cmd d2))
	   (equal? (rusage-data-dir d1) (rusage-data-dir d2)))]
     [(and (fake-target? t1) (fake-target? t2))  ;; might be same fake target
      ;; what makes a fake target the same?
      ;; same as a non fake target?
      #f
      ]
     [(and (equal? (target-name t1) (target-name t2))   ;; might be a subset; but then they need same name and file
	   (equal? (target-mfile t1) (target-mfile t2)))
      (andmap (lambda (e)
		(cond
		 [(has-edge? t1 e)
		  #t]
		 [else
		  (when (debug?)
			(printf "Did not find edge ~a from target <~a,~> in target <~a,~a>\n"
				e (target-name t2) (target-mfile t2) (target-name t1) (target-mfile t1)))
		  #f]))
	      (target-out-edges t2))]
     [else
      #f]))

  ;; t1 is exisiting target already in graph
  (define (maybe-merge t1 t2)
    (if (andmap (lambda (e)
    	          (or (has-edge? t1 e)
	              (equal? 'dep (edge-type e))))
	        (target-out-edges t2))
        (for-each (lambda (e)
		    (unless (has-edge? t1 e)
		      (add-dependency t1 (edge-end e))))
		  (reverse (target-out-edges t2)))
        #f))   

  (cond
    [(hash-ref targets tid #f) =>
     (lambda (tmp)
       (when  (and (not (empty? (target-out-edges t))) ;; if out edges are empty then this is a subset
       	           (not (same-target? tmp t)))
         (cond
	  [(maybe-merge tmp t)
	   (printf "Merged target <~a,~a>" (targetid-name tid) (targetid-mfile tid))]
	  [else
           (error 'add-target-to-makegraph "Target <~a,~a> already exists in makegraph. Edges ~a vs ~a" (targetid-name tid) (targetid-mfile tid) (target-out-edges tmp) (target-out-edges t))])))]
    [else
     (hash-set! targets tid t)]))

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
