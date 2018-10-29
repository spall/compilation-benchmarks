#lang errortrace racket/base

(require racket/list
         racket/sequence
	 "makegraph.rkt"
	 "makegraphfunctions.rkt"
	 "process-syscalls.rkt")

(provide build-new-graph)

(define RUNNING 0)
(define NOT-RUNNING 0)

(define (intersect? ls1 ls2)
  (cond
    [(empty? ls1)
     #f]
    [(member (car ls1) ls2)
     #t]
    [else
     (intersect? (cdr ls1) ls2)]))

#| leaf is a target that has no out edges

   1. consider each recipe.
   2.  build list of lists of recipes.  
       for example: '( (1 2 3) ) means that recipes 1 2 and 3 can run in parallel and there are 
       no other recipes
       '( (1 2) (3 4))  means that recipes 1 and 2 can run in parallel and 3 and 4 can run in
       parallel, after 1 and 2 finish.
   3. Using the above lists, for each sublist; create a fake target with dependency edges for
      each thing in the sublist;  so create a target for each thing in sublist....
   4. Create a 'seq edge from copy of current target to new fake targets;
   5. return this new target 
|#
(define (build-new-leaf t syscalls)  
  ;; 4. create copy of target
  (define nt (target (target-id t) (target-name t) (target-mfile t) '() '() (target-data t)))
  ;; throw away in edges  

  (define data (target-data t))
  (define-values (ins outs)
    (cond
      [(hash-ref syscalls (rusage-data-pid data) #f)
       (process-in-out-pid (rusage-data-pid data) (rusage-data-dir data) syscalls)]
      [else ;; no info
       (values #f #f)]))

  (values nt ins outs))
  
(define (build-new-non-leaf t syscalls)
  ;; 4. create copy of target
  (define nt (target (target-id t) (target-name t) (target-mfile t) '() '() #f)) ;; throw away in edges

  (define-values (deps reps)
    (let split ([es (target-out-edges t)])
      (cond
        [(empty? es)
         (values '() '())]
        [else
         (call-with-values (lambda () (split (cdr es)))
                           (lambda (ds rs)
                             (if (equal? 'dep (edge-type (car es)))
                                 (values (cons (car es) ds) rs)
                                 (values ds (cons (car es) rs)))))])))

  ;; collect all of the ins and outs of all of the dependencies
  ;; rebuild all of the dependencies and add to graph
  (define-values (dins douts)
    (let process-deps ([ds deps])
      (cond
        [(empty? ds)
         (values '() '())]
        [else
         (define d (car ds))
         (define-values (nd ins outs)
           (build-new-target (edge-end d) syscalls))
         
         (if nd
             (add-dependency nt nd)
             (printf "Dependency <~a,~a> did not do anything so deleting from graph\n" (target-name (edge-end d)) (target-mfile (edge-end d))))
         
         (call-with-values (lambda () (process-deps (cdr ds)))
                           (lambda (is os)
                             (values (and ins is (append ins is))
                                     (and outs os (append outs os)))))])))

  ;; Want to process recipes in reverse order of how they were run.
  ;; Example: run a, run b, run c;
  ;; Want to process c; then compare ins of c to outs o b;
  ;; if they do not share ins and outs;
  ;; then want to compare ins of b and c to outs of a.
  ;; etc.
  ;; then if c uses outs of a; but b does not
  ;; want to compare ins of a and b to outs of dependencies
  (define rep-ins (make-hash))
  (define rep-outs (make-hash))
  (define all-ins? #t)
  (define all-outs? #t)

  (define die? #f)
  
  (define-values (new-deps new-recipes)
    (let process-reps ([cr '()]
                       [rr '()]
                       [rs reps])
      (cond
        [(empty? rs) ;; compare cr to  deps
         (cond
           [douts
            (define-values (cans cants)
              (let loop ([crs cr])
                (cond
                  [(empty? crs)
                   (values '() '())]
                  [(hash-ref rep-ins (target-id (car crs)) #f) =>
                   (lambda (is)
                     (call-with-values
                      (lambda () (loop (cdr crs)))
                      (lambda (can cant)
                        (if (intersect? douts is) ;; can't move
                            (values can (cons (car crs) cant))
                            (values (cons (car crs) can) cant)))))]
                  [else
                   (call-with-values
                    (lambda () (loop (cdr crs)))
                    (lambda (can cant)
                      (values can (cons (car crs) cant))))])))
            
            (values cans (cons cants rr))]
           [else ;; can't move antyhing
	    (values '() (cons cr rr))])]
        [else ;; compare (car rs) to each thing in cr
         (define-values (nt ins outs)
           (build-new-target (edge-end (car rs)) syscalls))
         
	 
         (cond
           [(and nt ins outs) ;; enough information to move 
            (hash-set! rep-ins (target-id nt) ins)
            (hash-set! rep-outs (target-id nt) outs)
            
            ;; What does loop do?
            
            ;; compare outs to ins of stuff in cr
            (define-values (cans cants)
              (let loop ([crs cr])
                (cond
                  [(empty? crs)
                   (values '() '())] ;; cans and cants
                  [(hash-ref rep-ins (target-id (car crs)) #f) =>
                   (lambda (is)
                     (call-with-values
                      (lambda () (loop (cdr crs)))
                      (lambda (can cant)
		        (if (intersect? outs is) ;; can't move
                            (values can (cons (car crs) cant))
                            (values (cons (car crs) can) cant)))))]
                  [else ;; no info for this one
                   (call-with-values (lambda () (loop (cdr crs)))
                                     (lambda (can cant)
                                       (values can (cons (car crs) cant))))])))
            
            ;; have determined what can run in parallel with (car rs)
            ;; and have determined what cant run in parallel with (car rs)
            (process-reps (cons nt cans)
                          (cons cants rr)
                          (cdr rs))]
           [nt ;; not enough information to move
	    
            (set! all-ins? #f) (set! all-outs? #f)
            (process-reps (list nt)
                          (cons cr rr)
                          (cdr rs))]
           [else ;; target didn't do anything so ignore it
 	    (printf "Recipe <~a,~a> did not do anything so deleting from graph\n" (target-name (edge-end (car rs))) (target-mfile (edge-end (car rs))))
            (process-reps cr rr (cdr rs))])])))

  ;; have new deps if there are any and have new recipe ordering
  (for ([nd new-deps])
    (add-dependency nt nd))

  (for ([nrs new-recipes])
    (unless (empty? nrs)
      (cond
        [(= 1 (length nrs))
         (add-recipe nt (car nrs))]
        [else
         (define tmp (create-target (symbol->string (gensym "FAKE"))))
         (set-target-mfile! tmp (target-mfile nt))
         (for ([nr nrs])
           (add-dependency tmp nr))
         (add-recipe nt tmp)])))

  ;; done
  (if (empty? (target-out-edges nt))
      (begin (printf "this non leaf node <~a,~a> is now a leaf\n" (target-name nt) (target-mfile nt))
      (values #f (and all-ins? dins (append dins
                                       (sequence->list (in-hash-values rep-ins))))
          (and all-outs? douts (append douts
                                       (sequence->list (in-hash-values rep-outs))))))

  (values nt (and all-ins? dins (append dins
                                       (sequence->list (in-hash-values rep-ins))))
          (and all-outs? douts (append douts
                                       (sequence->list (in-hash-values rep-outs)))))))

(define (leaf? t)
  (empty? (target-out-edges t)))

(define (nothing-target? t)
  (and (leaf? t)
       (not (rusage-data? (target-data t)))))

(define (build-new-target t syscalls)
  (cond
    [(nothing-target? t)
     (set! NOT-RUNNING (+ 1 NOT-RUNNING))
     (values #f '() '())]  ;; target didn't do anything; so just delete it
    [(leaf? t)
     (set! RUNNING (+ 1 RUNNING))
     (build-new-leaf t syscalls)]
    [else
     (define-values (a b c)
       (build-new-non-leaf t syscalls))
     (if a
     	 (set! RUNNING (+ 1 RUNNING))
	 (set! NOT-RUNNING (+ 1 NOT-RUNNING)))
     (values a b c)]))

(define (build-new-graph graph syscalls)
  (define-values (nroot _ __)
    (build-new-target (makegraph-root graph) syscalls))

  (unless nroot
    (error 'build-new-graph "Did not create new root target!"))

  (printf "RUNNING is ~a\n" RUNNING)
  (printf "NOT-RUNNING is ~a\n" NOT-RUNNING)
  (create-makegraph nroot))
