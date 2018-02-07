#lang racket/base

(struct rusage-data (cmd rc elapsed user system maxrss
                         avgrss ins outs minflt majflt
                         swaps avgmem avgdata)
  #:mutable #:transparent)

(define (create-rusage-data cmd)
  (rusage-data cmd #f #f #f #f #f #f #f #f #f #f #f #f #f))

(struct makefile-data (datas)
  #:mutable #:transparent)

(define (all-fields-set? rds)
  (apply and (struct->list rds)))

(define (is-argv-line? line)
  (define words (string-split line))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (equal? (car tmp) "argv"))

(define (parse-argv-line line)
  (define words (string-split line))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (unless (is-argv-line? line)
    (error 'parse-argv-line "Line ~a does not begin with 'argv'" line))

  ;; argv=sh -c mkdir -p racket/src/build
  ;; so cdr cdr of words should begin line.

  ;; sanity check though
  (unless (equal? (car words) "argv=sh")
    (printf "Expected ~a to start line but got ~a\n" "argv=sh" (car words)))
  (unless (equal? (car (cdr words)) "-c")
    (printf "Expected ~a but got ~a\n" "-c" (car (cdr words))))

  (string-join (cdr (cdr words)) " ")) ;; return cmd.

(define (is-times-line? line)
  (define words (string-split line))
  (define tmp (string-split (car words) "=" #:trim? #f))
  (equal? (car tmp) "rc"))

(define EXPECTED-NUM 13)

;; returns a rusage-data structure
(define (parse-times-line line cmd)
  (define words (string-split line))
  (unless (is-times-line? line)
    (error 'parse-times-line "Line ~a does not begin with 'rc'" line))
  
  (unless (= EXPECTED-NUM (length words))
    (error 'parse-times-line "Expected ~a pieces of information; have ~a\n" EXPECTED-NUM line))

  (define rds (create-rusage-data cmd))
  (let parse ([info words])
    (unless (empty? info)
      (let* ([next (string-split (car info) "=" #:trim? #f)]
             [info (car next)]
             [val (car (cdr next))])
        (match info
          ["rc" (set-rusage-data-rc! rds val)]
          ["elapsed" (set-rusage-data-elapsed! rds val)]
          ["user" (set-rusage-data-user! rds val)]
          ["system" (set-rusage-data-system! rds val)]
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
  
  rds)

(define (parse fip)
  (define data (let read-file ()
                 (define line (read-line fip))
                 (cond
                   [(eof-object? line)
                    '()]
                   [(is-argv-line? line)
                    (let ([cmd (parse-argv-line line)]
                          [nline (read-line fip)])
                      (cond
                        [(is-times-line? nline)
                         (cons (parse-times-line nline cmd)
                               (read-file))]
                        [else
                         (printf "Expected times line to follow argv line got ~a instead\n" nline)
                         (read-file)]))]
                   [else
                    (read-file)])))
  (makefile-data data))

(define file-path
  (command-line
   #:args (path)
   path))

(define (driver file-path)
  (define file (open-input-file file-path #:mode 'text))
  (define result (parse file))
  (close-input-port file))
