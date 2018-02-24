#lang racket/base

(require racket/list
         racket/string
         "makegraph.rkt")

(provide calculate-target-time
         get-targets
         create-dotfile-string
         print-all-targets-and-mfiles
         collapse-targets
         span)


;; time in seconds or #f
(define (calculate-target-time t graph)
  (cond
    [(target-remake? t) 
     (define data (target-data t))
     (let sum-elapsed ([ls data])
       (cond
         [(empty? ls)
          0.0]
         [else
          (+ (rusage-data-elapsed (car ls))
             (sum-elapsed (cdr ls)))]))]
    [else
     (printf "Target ~a was not built.\n" (target-name t))
     #f]))

;; return a list of possible targets
(define (get-targets graph tname)
  (define targets (makegraph-targets graph))
  (for/fold ([tgs '()])
            ([val (in-hash-values targets)])
    (if (equal? (target-name val) tname)
        (cons val tgs)
        tgs)))

(define (span root graph)
  ;; Span(root) = longest time down deps path.
  ;;              + Sum of the times of the pink paths

  ;; loop through deps and calculate span for each one.
  (define maxdep (let loop1 ([deps (target-deps root)]
                             [max 0])
                   (cond
                     [(empty? deps)
                      max]
                     [else
                      (define t (get-target graph (car deps)))
                      (define tspan (span t graph))
                      (loop1 (cdr deps) (if (> tspan max)
                                            tspan
                                            max))])))

  (define sumchildren (let loop2 ([children (target-children root)]
                                  [sum 0])
                        (cond
                          [(empty? children)
                           sum]
                          [else
                           (define t (get-target graph (car children)))
                           (define tspan (span t graph))
                           (loop2 (cdr children) (+ tspan sum))])))

  (define ttime (calculate-target-time root graph))
  (+ ttime maxdep sumchildren))

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
          [children (target-children t)])
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
      (set-target-children! t (let loop ([chlds children])
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
          (reverse (target-children v))
          (range 1 (+ 1 (length (target-children v)))))
     
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
            
         
               
  
