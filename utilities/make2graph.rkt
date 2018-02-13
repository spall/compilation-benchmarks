#lang racket/base

(require racket/list
         racket/string
         "makegraph.rkt")

(provide parse-dry-run)

(define (get-target graph tname)
  (define targets (makegraph-targets graph))
  (let loop ([ts targets])
    (cond
      [(empty? ts)
       (let ([nt (create-target tname)])
         (set-makegraph-targets! graph (cons nt (makegraph-targets graph)))
         nt)]
      [(equal? (target-name (car ts)) tname)
       (car ts)]
      [else
       (loop (cdr ts))])))
    

(define (starts-with words looking-for) ;; looking-for is a list of words
  (cond
    [(empty? looking-for)
     words]
    [(empty? words)
     #f]
    [(equal? (car words) (car looking-for))
     (starts-with (cdr words) (cdr looking-for))]
    [else
     #f]))

(define (ends-with words looking-for)
  (let ([result (starts-with (reverse words) (reverse looking-for))])
    (if result
        (reverse result)
        result)))

(define FILENAME-SEP "'")

(define (get-target-name words)
  (if (empty? words)
      (error 'get-target-name "Words is empty")
      (values (string-trim (car words) FILENAME-SEP)
              (cdr words))))

(define (something fip graph root)
  (define makefile-name "") ;; set! this later

  (let outer-loop () ;; while we can read lines
    (define line (read-line fip))
    (unless (eof-object? line)
      (let ([words (string-split line)])
        (cond
          [(starts-with words (list "Considering" "target" "file")) =>
           (lambda (rest-line)
             (let-values ([(tname rest-line) (get-target-name rest-line)])
               ;; What do we do with this? Don't know yet.
               
               ;; skip all lines until we get to something about
               ;; finished prereqs or was already considered...
               (if (and (not (equal? makefile-name "")) (equal? tname makefile-name))
                   (let loop ()
                     (let ([words (string-split (read-line fip))])
                       (cond
                         [(or (starts-with words (list "Finished" "prerequisites" "of" "target" "file"))
                              (ends-with words (list "was" "considered" "already."))) =>
                          (lambda (rest-line)
                            (define-values (tname rest-line) (get-target-name rest-line))
                            (unless (equal? tname makefile-name) ;; stopping if equal.
                              (loop)))]
                         [else ;; correct i believe.
                          (loop)]))) 
                   ;; when do we want to do this?
                   (let ([child (get-target graph tname)])
                     ;; they check level here. not sure why
                     (add-child root child)
                     (something fip graph child)))
               (outer-loop)))]
          
          [(starts-with words (list "Must" "remake" "target")) =>
           (lambda (rest-line)
             (let-values ([(tname _) (get-target-name rest-line)])
               (define target (get-target graph tname))
               (set-target-remake?! target #t)
               (outer-loop)))]
          [(starts-with words (list "Pruning" "file")) =>
           (lambda (rest-line)
             (let-values ([(tname _) (get-target-name rest-line)])
               (add-child root (get-target graph tname))
               (outer-loop)))]
          [(or (starts-with words (list "Finished" "prerequisites" "of" "target" "file"))
               (ends-with words (list "was" "considered" "already."))) =>
           (lambda (rest-line)  ;; This one also check level; im not sure what this is used for.
             (let-values ([(tname _) (get-target-name rest-line)])
               (unless (equal? tname (target-name root))
                 (error 'something "expected ~a got ~a" (target-name root) tname))
               ;; break; just don't loop
               ))]
          [(starts-with words (list "Reading" "makefile")) =>
           (lambda (rest-line)
             (let-values ([(tname _) (get-target-name rest-line)])
               (set! makefile-name tname)
               (outer-loop)))]
          [else
           (outer-loop)]))))
  )  

(define (create-graph file-path)
  (define file (open-input-file file-path #:mode 'text))
  (define root (create-target "<ROOT>"))
  (define graph (makegraph '() root))

  (something file graph root)
 
  (close-input-port file)
  graph)

;; Code for a "queue"
(define head car)
(define tail cdr)
(define (enqueue v q)
  (reverse (cons v (reverse q))))

(define (bfs graph)
  (driver (makegraph-targets graph)
          (make-hash)))

(define (driver queue visited)
  (unless (empty? queue)
    (let ([h (head queue)])
      (cond
        [(not (hash-ref visited h #f))
         (printf "~a\n" (target-name h))
         (hash-set! visited h #t)
         (let ([nq (let foldl ([children (target-children h)]
                               [q (tail queue)])
                     (cond
                       [(empty? children) q]
                       [(hash-ref visited (car children) #f) q]
                       [else
                        (foldl (cdr children)
                               (enqueue (car children) q))]))])
           (driver nq visited))]
        [else
         (driver (tail queue) visited)]))))


(define (parse-dry-run file-path)
  (void))




