#lang racket/base

(require racket/string
         racket/list
         racket/match
         (only-in racket/function
                  identity)
         "makegraph.rkt")

(provide parse-rusage)

(define (starts-with? words to-match)
  (cond
    [(empty? to-match)
     #t]
    [(empty? words)
     #f]
    [(equal? (car words) (car to-match))
     (starts-with? (cdr words) (cdr to-match))]
    [else
     #f]))

(define (get-target-name str)
  (string-trim (string-trim (string-trim str "." #:left? #f) "'" #:left? #f) "`" #:right? #f))

(define (helper line to-match)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  (and (starts-with? words to-match)
       (get-target-name (last words))))

;; return #f or target
(define (no-need-to-remake? line)
  (helper line (list "No" "need" "to" "remake" "target")))

;; return #f or target
(define (must-remake? line)
  (helper line (list "Must" "remake" "target")))

;; return #f or target
(define (successfully-remade? line)
  (helper line (list "Successfully" "remade" "target" "file")))

;; return #f or target
(define (considering-target-file? line)
  (helper line (list "Considering" "target" "file")))

(define (invoking-recipe? line)
  (helper line (list "Invoking" "recipe" "from")))

;; return #f or path
(define (entering-directory? line)
  ;; make[n]:
  (define words (string-split (string-trim line " " #:repeat? #t)))
  ;; first word should be make something...
  (cond
    [(starts-with? (cdr words) (list "Entering" "directory"))
     ;; get make and number
     (cons (string-trim (cadr (string-split (string-trim (car words) ":" #:left? #f) "[")) "]" #:left? #f)
      (string-trim (string-trim (last words) "'" #:left? #f) "`" #:right? #f))]
    [else
     #f]))

;; return #f or path
(define (leaving-directory? line)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  ;; first word should be make something...
  (cond
    [(starts-with? (cdr words) (list "Leaving" "directory"))
     ;; get make and number
     (cons (string-trim (cadr (string-split (string-trim (car words) ":" #:left? #f) "[")) "]" #:left? #f)
             (string-trim (string-trim (last words) "'" #:left? #f) "`" #:right? #f))]
    [else
     #f]))

;; return #f or target
(define (finished-prerequisites? line)
  (helper line (list "Finished" "prerequisites" "of" "target" "file")))

;; return #f or command
(define (argv? line)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (cond
    [(equal? (car tmp) "argv")
     (unless (equal? (car words) "argv=sh")
       (printf "Expected ~a to start line but got ~a\n" "argv=sh" (car words)))
     (unless (equal? (car (cdr words)) "-c")
       (printf "Expected ~a but got ~a\n" "-c" (car (cdr words))))
     (string-join (cdr (cdr words)) " ")]
    [else
     #f]))

(define EXPECTED-NUM 13)

;; return #f or rusage-data structure
(define (rusage-info? line cmd)
  (define words (string-split (string-trim line " " #:repeat? #t)))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (cond
    [(equal? (car tmp) "rc")
     (unless (= EXPECTED-NUM (length words))
       (error 'parse-times-line "Expected ~a pieces of information; have ~a\n" EXPECTED-NUM line))
     (define rds (create-rusage-data cmd))
     (let parse ([info words])
       (unless (empty? info)
         (let* ([next (string-split (car info) "=" #:trim? #f)]
                [info_ (car next)]
                [val (car (cdr next))])
           (match info_
             ["rc" (set-rusage-data-rc! rds val)]
             ["elapsed" (set-rusage-data-elapsed! rds (string->number val))]
             ["user" (set-rusage-data-user! rds (string->number val))]
             ["system" (set-rusage-data-system! rds (string->number val))]
             ["maxrss" (set-rusage-data-maxrss! rds val)]
             ["avgrss" (set-rusage-data-avgrss! rds val)]
             ["ins" (set-rusage-data-ins! rds val)]
             ["outs" (set-rusage-data-outs! rds val)]
             ["minflt" (set-rusage-data-minflt! rds val)]
             ["majflt" (set-rusage-data-majflt! rds val)]
             ["swaps" (set-rusage-data-swaps! rds val)]
             ["avgmem" (set-rusage-data-avgmem! rds val)]
             ["avgdata" (set-rusage-data-avgdata! rds val)]
             [else
              (printf "Do not recognize ~a so ignoring\n" info)])
           (parse (cdr info)))))
     (unless (all-fields-set? rds)
       (error 'parse-times-line "Failed to populate entire structure: ~a" rds))
     rds]
    [else
     #f]))

(define (read-full-line fip)
  (let loop ()
    (define line (read-line fip))
    (cond
      [(or (equal? "" line)
           (eof-object? line))
       line]
      [(equal? (car (reverse (string-split line))) "\\")
       (string-append (string-trim line "\\" #:left? #f)
                      " "
                      (loop))]
      [else
       line])))

(define (parse-file fip)
  (define mgraph (create-makegraph))
  (define root (create-target "<ROOT>"))
  (set-makegraph-root! mgraph root)

  (define (parse-line line ts prqs? dirs)
    (define t (car ts))
    (cond
      [(equal? "" line)
       (read-file ts prqs? dirs)]
      [(eof-object? line)
       (unless (equal? root t)
         (error 'parse-file "Unexpected end of line before end of target ~a" (target-name t)))]
      
      ;; set field in target appropriately
      [(no-need-to-remake? line) =>
       (lambda (tname)
         (unless t
           (error 'parse-file "Expected t to be a target"))
         (unless (equal? tname (target-name t))
           (error 'parse-file "Expected no need to remake ~a got ~a" (target-name t) tname))
         (read-file (cdr ts) (cdr prqs?) dirs))]
      [(must-remake? line) =>
       (lambda (tname)
         (unless t
           (error 'parse-file "Expected t to be a target"))
         (unless (equal? tname (target-name t))
           (error 'parse-file "Expected must remake ~a got ~a" (target-name t) tname))
         (set-target-remake?! t #t)
         (read-file ts prqs? dirs))]
      
      [(invoking-recipe? line) =>
       (lambda (tname)
         (unless t
           (error 'parse-file "Expected t to be a target"))
         (unless (equal? tname (target-name t))
           (error 'parse-file "Expected to invoke recipe for ~a got ~a" (target-name t) tname))
         
         ;; next line should be entering directory?
         (define nline (read-full-line fip))
         (cond
           [(entering-directory? nline) =>
            (lambda (p)
              (define dir (cdr p))
              (set-target-mfile! t dir)
              (read-file ts prqs? (cons p dirs)))]
           [else
            (set-target-mfile! t (cdar dirs))
            (parse-line nline ts prqs? dirs)]))]
      
      [(entering-directory? line) =>
       (lambda (p)
         (define dir (cdr p))
         (read-file ts prqs? (cons p dirs)))]
      [(leaving-directory? line) =>
       (lambda (p) ;; should identify current dir on top of dirs stack
         (define n (car p))
         (define dir (cdr p))
         (cond
           [(and (equal? n (caar dirs)) (equal? dir (cdar dirs)))
            (read-file ts prqs? (cdr dirs))]
           [else
            (error 'parse-line "Leaving directory ~a with n ~a but top of stack is ~a\n\n" dir n (car dirs))]))]
      
      [(successfully-remade? line) => ;; finished this target
       (lambda (tname)
         (unless t
           (error 'parse-file "Target is #f\n"))
         (unless (equal? tname (target-name t))
           (error 'parse-file "Successfully remade target ~a expected to remake target ~a" tname (target-name t)))
         (read-file (cdr ts) (cdr prqs?) dirs))]
      
      [(finished-prerequisites? line) =>
       (lambda (tname)
         (unless t
           (error 'parse-file "Target is #f\n"))
         (unless (equal? tname (target-name t))
           (error 'parse-file "Expected to finish prereqs of ~a instead finished prereqs of ~a\n" (target-name t) tname))
         (read-file ts (cons #f (cdr prqs?)) dirs))]
      
      ;; starting a new target. 
      [(considering-target-file? line) =>
       (lambda (tname)
         ;; target could be a duplicate. wont know until later.
         (define ntarget
           (cond
             [#f ;(target-in-graph mgraph tname) =>
              identity]
             [else
              (let ([tmp (create-target tname)])
                (add-target-to-makegraph mgraph tmp)
                tmp)]))
         
         (when t
           (if (car prqs?) ;; prereq of t?
               (add-dep t ntarget)
               (add-child t ntarget)))
         (read-file (cons ntarget ts) (cons #t prqs?) dirs))]
      
      ;; run information
      [(argv? line) =>
       (lambda (cmd)
         (define nline (read-full-line fip))
         (cond
           [(rusage-info? nline cmd) =>
            (lambda (info)
              (when t
                (add-data-to-target! t info)))]
           [else
            (printf "Expected times line to follow argv line ~a got ~a instead\n" line nline)])
         (read-file ts prqs? dirs))]
      [else
       (read-file ts prqs? dirs)]))
  
  (define (read-file ts prqs? dirs)
    (define line (read-full-line fip))
    (parse-line line ts prqs? dirs))

  (read-file (list root) (list #f) (list (cons 0 "top")))
  mgraph)

(define (parse-rusage file-path)
  (define file (open-input-file file-path #:mode 'text))
  (define result (parse-file file))
  (close-input-port file)
  result)

  


































