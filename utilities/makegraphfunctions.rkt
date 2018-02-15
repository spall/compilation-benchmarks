#lang racket/base

(require racket/list
         "makegraph.rkt")

(provide longest-dep-path
         calculate-target-time
         get-target
         create-dotfile-string)


;; time in seconds or #f
(define (calculate-target-time t)
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

(define (get-target graph tname)
  (define targets (makegraph-targets graph))
  (let loop ([ts targets])
    (cond
      [(empty? ts)
       #f]
      [(equal? (target-name (car ts)) tname)
       (car ts)]
      [else
       (loop (cdr ts))])))

(define (longest-dep-path root)
  (let ([deps (target-deps root)])
    (cond
      [(empty? deps)
       (values 0 (list root))]
      [else
       (define-values (maxlen maxls) (longest-dep-path (car deps)))
       (let find-max ([mlen maxlen]
                      [mls maxls]
                      [rs (cdr deps)])
         (cond
           [(empty? rs)
            (values mlen mls)]
           [else
            (define-values (tmplen tmpls) (longest-dep-path (car rs)))
            (if (> tmplen mlen)
                (find-max tmplen tmpls (cdr rs))
                (find-max mlen mls (cdr rs)))]))])))

;; ------------------------ graphviz dot file --------------------------------

;; returns a string represting a grpah in the dot language
(define (create-dotfile-string g)
  (string-append
   (apply string-append
          (cons "strict digraph {\n" ;; strict means no more than 1 edge between the same 2 nodes
                (foldl (lambda (t ls)
                         (append (create-edges t)
                                 ls))
                       '()
                       (make2graph-targets g))))
   "}\n"))

(define child-color "blue")
(define dep-color "red")

;; returns a list of edges.
(define (create-dotfile-edges v)
  ;; edges from v to children
  (append
   (map (lambda (c)
          (format "~a -> ~a [color=~a];\n" (target-name v) (target-name c) child-color)) ;; edge from v to c because v depends on c
        (target-children v))
   
  ;; edges from dependences to v
   (map (lambda (d)
          (format "~a -> ~a [color=~a];\n" (target-name v) (target-name d) dep-color))
        (target-deps v))))

;; ----------------------------------------------------------------------
            
         
               
  
