#lang racket/base

(require racket/list
         racket/string
         "makegraph.rkt"
         "flags.rkt")

(provide get-targets
         create-dotfile-string
         print-all-targets-and-mfiles
         collapse-targets
         work
         span
         parallel-slackness
         predicted-speed

         print-graph)

;; return a list of possible targets
(define (get-targets graph tname)
  (define targets (makegraph-targets graph))
  (for/fold ([tgs '()])
            ([val (in-hash-values targets)])
    (if (equal? (target-name val) tname)
        (cons val tgs)
        tgs)))

;; work(root) = time to run sequentially
(define (work root_ graph)
  (define visited (make-hash))
  (define (driver root lbound ubound)
    ;; sum dep paths
    (define-values (workdeps leid_)
      (for/fold ([sum 0]
                 [leid ubound])
                ([e (reverse (target-deps root))])
        (cond
          [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
           (when (hash-ref visited e #f)
             (error 'work "Visited edge ~a a 2nd time!" e))
           (hash-set! visited e #t)
           (values (+ sum (sum-times (edge-data e))
                      (driver (get-target graph (edge-end e))
                              (edge-id e)
                              leid))
                   (edge-id e))]
          [else
           (values sum leid)])))
    
    (when DEBUG
      (when (> workdeps 0)
        (printf "For root <~a,~a>; sum of dependencies is ~a\n" (target-name root) (target-mfile root) workdeps)))
    
    (define-values (workchildren __)
      (for/fold ([sum 0]
                 [leid leid_])
                ([e (reverse (target-recipes root))])
        (cond
          [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
           (when (hash-ref visited e #f)
             (error 'work "Visited edge ~a a 2nd time!" e))
           (hash-set! visited e #t)
           (values (+ sum (sum-times (edge-data e))
                      (driver (get-target graph (edge-end e))
                              (edge-id e)
                              leid))
                   (edge-id e))]
          [else
           (values sum (edge-id e))])))
    
    (when DEBUG
      (when (> workchildren 0)
        (printf "For root <~a,~a>; sum of children's work is ~a\n" (target-name root) (target-mfile root) workchildren)))
    
    (+ workdeps workchildren))
  
  (define w (driver root_ (- (apply min (map edge-id (append (target-deps root_)
                                                             (target-recipes root_))))
                             1)
                    1))

  ;; Checking that we visited each edge once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  (for ([t (in-hash-values (makegraph-targets graph))])
    (for ([e (target-deps t)])
      (unless (hash-ref visited e #f)
        (error 'work "Never visited edge ~a during work calculation!" e)))
    (for ([e (target-recipes t)])
      (unless (hash-ref visited e #f)
        (error 'work "Never visited edge ~a during work calculation!" e))))
  w)
                                  
                              
(define (sum-times ttimes)
  (cond
    [(empty? ttimes)
     0]
    [(list? ttimes)
     (+ (rusage-data-elapsed (car ttimes))
        (sum-times (cdr ttimes)))]
    [else
     (unless (number? ttimes)
       (error 'sum-times "ttimes is neither a list nor a number ~a" ttimes))
     ttimes]))

;; Span(root) = longest time down deps path.
;;              + Sum of the times of the pink paths
(define (span root_ graph)
  (define visited (make-hash))
  (define (driver root lbound ubound) ;; for an edge to be part of current path. must be > lbound and < ubound
    (define-values (spandeps leid_)
      (for/fold ([max_ 0]
                 [leid ubound])
                ([e (reverse (target-deps root))])
        (cond
          [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
           (when (hash-ref visited e #f)
             (error 'span "Visited edge ~a a 2nd time!" e))
           (hash-set! visited e #t)
           (values (max max_
                        (+ (sum-times (edge-data e))
                           (driver (get-target graph (edge-end e))
                                   (edge-id e)
                                   leid)))
                   (edge-id e))]
          [else
           
           (values max_ leid)])))

    (when DEBUG
      (when (> spandeps 0)
        (printf "For root <~a,~a>; span of dependencies is ~a\n" (target-name root) (target-mfile root) spandeps)))

    (define-values (spanchildren __)
      (for/fold ([sum 0]
                 [leid leid_])
                ([e (reverse (target-recipes root))])
        (cond
          [(and (> (edge-id e) lbound) (< (edge-id e) ubound))
           (when (hash-ref visited e #f)
             (error 'span "Visited edge ~a a 2nd time!" e))
           (hash-set! visited e #t)
           (values (+ sum
                      (sum-times (edge-data e))
                      (driver (get-target graph (edge-end e))
                              (edge-id e)
                              leid))
                   (edge-id e))]
          [else
           (values sum (edge-id e))])))

    (when DEBUG
      (when (> spanchildren 0)
        (printf "For root <~a,~a>; sum of children's spans is ~a\n" (target-name root) (target-mfile root) spanchildren)))
    
    (+ spandeps spanchildren))
  
  (define s (driver root_ (- (apply min (map edge-id (append (target-deps root_)
                                                             (target-recipes root_))))
                             1)
                    1))

  ;; Checking that we visited each edge once. If we did not; there is a problem.
  ;; this only works if we are checking entire graph.
  #;(for ([t (in-hash-values (makegraph-targets graph))])
    (for ([e (target-deps t)])
      (unless (hash-ref visited e #f)
        (error 'span "Never visited edge ~a during span calculation!" e)))
    (for ([e (target-recipes t)])
      (unless (hash-ref visited e #f)
        (error 'span "Never visited edge ~a during span calculation!" e))))
  
  s)

;; factor by which the parallelism of the computation exceeds the number of processors
(define (parallel-slackness graph pcount)
  (define work_ (work (makegraph-root graph) graph))
  (define span_ (span (makegraph-root graph) graph))
  (exact->inexact (/ work_ (* pcount span_))))

#|
   TODO: rewrite this algorithm to work better with make...... 
   brent's law 
|#
(define (predicted-speed graph pcount)
  (define work_ (work (makegraph-root graph) graph))
  (define span_ (span (makegraph-root graph) graph))
  (define speed (exact->inexact (/ work_ pcount)))

  (+ span_ (/ (- work_ span_) pcount)))

(define (print-all-targets-and-mfiles graph name)
  (define targets (hash-keys (makegraph-targets graph)))
  (let loop ([ts targets])
    (unless (empty? ts)
      (when (equal? name (mcar (car ts)))
        (printf "target: ~a; mfile: ~a\n\n" (mcar (car ts)) (mcdr (car ts))))
      (loop (cdr ts)))))

(define (collapse-targets graph)
  (define targets (makegraph-targets graph))
  (for ([t (in-hash-values targets)])
    (let ([deps (target-deps t)]
          [children (target-recipes t)])
      ;; check deps and children for duplicates
      (set-target-deps! t (let loop ([dps deps])
                            (cond
                              [(empty? dps)
                               '()]
                              [(member (car dps) (cdr dps))
                               ;; occurs later so throw away
                               (loop (cdr dps))]
                              [else
                               (cons (car dps) (loop (cdr dps)))])))
      (set-target-recipes! t (let loop ([chlds children])
                                (cond
                                  [(empty? chlds)
                                   '()]
                                  [(member (car chlds) (cdr chlds))
                                   (loop (cdr chlds))]
                                  [else
                                   (cons (car chlds) (loop (cdr chlds)))]))))))
      
      

;; ------------------------ graphviz dot file --------------------------------

(define child-color "red") ;"blue")
(define dep-color "green") ;"red")

;; returns a string represting a grpah in the dot language
(define (create-dotfile-string g)
  (define targets (makegraph-targets g))
  (define (helper v targets color)
    (lambda (c)
      (define t (hash-ref targets c (lambda ()
                                      (error 'create-dotfile "Failed to find ~a among graph's targets" c))))
      (format "\"~a~a\" -> \"~a~a\" [color=~a];\n" (target-name v) (target-id v)
              (target-name t) (target-id t) color)))
      
  (define (create-dotfile-edges v)
    (append
     (map (lambda (c i)
            (define t (hash-ref targets c (lambda ()
                                            (error 'create-dotfile "Failed to find ~a among graph's targets" c))))
            (format "\"~a~a\" -> \"~a~a\" [label=~a, color=~a];\n" (target-name v) (target-id v)
                    (target-name t) (target-id t) i child-color))
          (reverse (target-recipes v))
          (range 1 (+ 1 (length (target-recipes v)))))
     
     (map (helper v targets dep-color)
          (target-deps v))))
  
  (apply string-append
         (cons "strict digraph {\n"
               (append (for/fold ([accu '()])
                                 ([val (in-hash-values targets)])
                         (append accu
                                 (create-dotfile-edges val)))
                       (list "}\n")))))
;; ----------------------------------------------------------------------
            
         

(define (print-graph graph)
  (for ([n (in-hash-values (makegraph-targets graph))])
    (printf "Processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))
    (printf "target-deps: ~a\n" (target-deps n))
    (for ([e (target-deps n)])
      (let ([tmp (get-target graph (edge-end e))])
        (printf "Dependency edge between <~a,~a,~a> and <~a,~a,~a> with ID ~a\n\n"
                (target-name n) (target-mfile n) (target-id n)
                (target-name tmp) (target-mfile tmp) (target-id tmp) (edge-id e))))
    (for ([e (target-recipes n)])
      (let ([tmp (get-target graph (edge-end e))])
        (printf "Child edge between <~a,~a,~a> and <~a,~a,~a> with ID ~a\n\n"
                (target-name n) (target-mfile n) (target-id n)
                (target-name tmp) (target-mfile tmp) (target-id tmp)
                (edge-id e))))

    (printf "Finished processing node <~a,~a,~a>\n" (target-name n) (target-mfile n) (target-id n))))
  
