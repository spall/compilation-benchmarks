#lang racket/base

(require "process-syscalls.rkt"
         parser-tools/lex
         parser-tools/yacc
         syntax/readerr
         racket/path
	 racket/string
	 racket/promise
	 racket/place
	 racket/match
         (prefix-in : parser-tools/lex-sre))

(provide parse-strace)

(define-tokens data (ARGS RETVAL SCALL SIGNAL FINAL))
(define-empty-tokens delim (EOF))

(define strace-lexer
  (lexer-src-pos
   ;; skip final line and signal lines
   [(eof) 'EOF]
   ["+++" (token-SIGNAL (string-append lexeme (signal-lexer input-port)))]
   ["---" (token-FINAL (string-append lexeme (final-lexer input-port)))]
   [whitespace (return-without-pos (strace-lexer input-port))]
   [#\( (token-ARGS (apply string-append ((args-lexer) input-port)))]
   [#\= (token-RETVAL (apply string-append (retval-lexer input-port)))]
   [(:~ #\() (token-SCALL (apply string-append (cons lexeme (scall-lexer input-port))))]))
   
(define signal-lexer
  (lexer	
   ["+++" lexeme]
   [any-char (string-append lexeme (signal-lexer input-port))]))

(define final-lexer
  (lexer
   ["---" lexeme]
   [any-char (string-append lexeme (final-lexer input-port))]))

(define scall-lexer
  (lexer
   [(:: (:* (:~ #\())) (list lexeme)]))

(define retval-lexer
  (lexer
   [#\newline null]
   [any-char (cons lexeme (retval-lexer input-port))]))

(define (args-lexer [pc 0])
  (lexer
   [#\" (append (cons lexeme (string-lexer input-port))
              ((args-lexer pc) input-port))]
   [#\( (cons lexeme ((args-lexer (+ pc 1)) input-port))]
   [#\) (if (= 0 pc)
            null
            (cons lexeme ((args-lexer (- pc 1)) input-port)))]
   [any-char (cons lexeme ((args-lexer pc) input-port))]))

(define string-lexer
  (lexer
   [#\" (list lexeme)]
   [(:: #\\ any-char) (cons lexeme (string-lexer input-port))]
   [any-char (cons lexeme (string-lexer input-port))]))

(define-lex-abbrevs
  [whitespace (:or #\newline #\space #\tab #\return #\vtab)])

;; parser

(define (strace-parser f)
  (parser
   (src-pos)

   (start s)
   (end EOF)
   
   (error(lambda (a name val start end)
           (printf "token is ~a ~a\n" name val)
              (raise-read-error 
               "read-error"
               f
               (position-line start)
               (position-col start)
               (position-offset start)
               (- (position-offset end)
                  (position-offset start)))))
   
   
   (tokens data delim)
   
   (grammar

    (s [(scall-list) (reverse $1)])
    
    (other
     [(SIGNAL) (void)]
     [(FINAL) (void)])

    (scall
     [(SCALL ARGS RETVAL) (syscall $1 $2 (string-trim $3))])
    
    (scall-list [() null]
    		[(scall-list other) $1]
                [(scall-list scall) (cons $2 $1)]))))
    

;; parses an strace file
 (define (parse-file f)
   (define file-port (open-input-file f #:mode 'text))

   (begin0 ((strace-parser f) (lambda () (strace-lexer file-port)))
     (close-input-port file-port)))


;; takes a directory of strace output files
(define (parse-strace dir)
  (unless (directory-exists? dir)
    (error 'parse-strace "~a is not a directory\n" dir))

  (define scalls (make-hash))

  
   (start-workers)
  
  (define promises (for/list ([f (in-directory dir)])
    (define ext (path-get-extension f))
    (unless ext
      (error 'parse-strace "~a does not have an extension" f))
    (define pid (string->number (bytes->string/locale ext #f 1)))

    (when (hash-has-key? scalls pid)
      (error 'parse-strace "Already have processed syscalls for pid ~a" pid))
    
    (delay/thread
      (hash-set! scalls pid (hash-ref (run-in-place f pid) pid)))))

  (for ([pr promises])
    (force pr))

  scalls)

(define WORKERS (make-parameter (and (place-enabled?) (min 16 (processor-count)))))

(define-values (enq-ch deq-ch) (place-channel))

(define (start-workers)
  (when (WORKERS)
    (for ([i (WORKERS)])
      (start-worker deq-ch i))))

(define (start-worker get-ch i)
  (place/context ch
    (let loop ()
      (match (place-channel-get get-ch)
       [(vector f pid res-ch)
        (define res (parse-file f))

        (place-channel-put res-ch (hash pid res))])
      (loop))))

(define (run-in-place f pid)
  (define-values (res-ch res-ch*) (place-channel))

  (place-channel-put enq-ch (vector f pid res-ch*))

  (place-channel-get res-ch))