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
	 racket/list
         (prefix-in : parser-tools/lex-sre))

(provide parse-strace)

(define-tokens data (ARGS UNFINISHED RETVAL SCALL SIGNAL FINAL))
(define-empty-tokens delim (EOF))

(define strace-lexer
  (lexer-src-pos
   ;; skip final line and signal lines
   [(eof) 'EOF]
   ["+++" (token-SIGNAL (string-append lexeme (signal-lexer input-port)))]
   ["---" (token-FINAL (string-append lexeme (final-lexer input-port)))]
   [whitespace (return-without-pos (strace-lexer input-port))]
   [#\( (call-with-values (lambda () ((args-lexer) input-port))
	  (lambda (ls val)
	    (if (equal? val 'finished)
		(token-ARGS (apply string-append ls))
		(token-UNFINISHED (apply string-append ls)))))]
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
   ["<unfinished ...>)" (values null 'unfinished)]
   ["<unfinished ...>" (values null 'unfinished)]
   [#\" (call-with-values (lambda () (string-lexer input-port))
	  (lambda (str) 
	    (call-with-values (lambda () ((args-lexer pc) input-port))
	      (lambda (rest val)
		(values (append (cons lexeme str)
				rest) val)))))]
   [#\( (call-with-values (lambda () ((args-lexer (+ pc 1)) input-port))
	  (lambda (rest val)
	    (values (cons lexeme rest) val)))]
   [#\) (if (= 0 pc)
            (values null 'finished)
            (call-with-values (lambda () ((args-lexer (- pc 1)) input-port))
	      (lambda (rest val)
		(values (cons lexeme rest) val))))]
   [any-char (call-with-values (lambda () ((args-lexer pc) input-port))
	       (lambda (rest val)
		 (values (cons lexeme rest) val)))]))

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
   
   (error (lambda (a name val start end)
	    (error 'parser "failed parsing file ~a on token ~a with value ~a." f name val)))
   
   (tokens data delim)
   
   (grammar
    
    (s [(scall-list) (reverse $1)])
    
    (other
     [(SIGNAL) (void)]
     [(FINAL) (void)])
    
    (scall
     [(SCALL UNFINISHED RETVAL) 'failed] ;; ok then
     [(SCALL UNFINISHED) 'failed]
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
  (define tstamp->pid (make-hash))
  
  
  (start-workers)
  
  (define (replace-comma str)
    (string-replace str "," "."))

  (define count_ 0)
  (define promises (for/list ([f (in-directory dir)])
			     (define ext (path-get-extension f))
			     (unless ext
				     (error 'parse-strace "~a does not have an extension" f))
			     (define strext (bytes->string/locale ext #f 1))
			     (define pid #f)
			     (define count #f)
			     (define tstamp #f)
			     (define tmp (string-split strext "_")) ;; should be either 2 or 3 things
			     (set! count_ (+ 1 count_))
			     (cond
			      [(= 2 (length tmp)) ;; pid_timestamp
			       (set! pid (string->number (car tmp)))
			       (set! tstamp (string->number (replace-comma (cadr tmp))))]
			      [(= 3 (length tmp))               ;; pid_n_timestamp
			       (set! pid (string->number (car tmp)))
			       (set! count (string->number (cadr tmp)))
			       (set! tstamp (string->number (replace-comma (caddr tmp))))]
			      [else
			       (error 'parse-strace "This data is not compatible with current version. ~a" strext)])
			     
			     (when (hash-ref tstamp->pid tstamp #f)
				   (error 'parse-strace "Have seen tstamp ~a before" tstamp))
			     (hash-set! tstamp->pid tstamp (cons pid (if count (+ 1 count) 0)))
			     
			     (delay/thread
			      (let ([res (run-in-place f pid)])
				(if (hash-has-key? scalls pid)
				    (hash-set! scalls pid (hash-set (hash-ref scalls pid) (+ 1 count) (if (contains-failure? res) #f res)))
				    (hash-set! scalls pid (hash 0 (if (contains-failure? res) #f res))))))
  
  (for ([pr promises])
       (force pr))
  
  ;; sort tstamp -> pid 
  (values (map cdr
	       (sort (hash->list tstamp->pid)
		     ;; want a list of pids. 
		     < #:key car))
	  scalls))

(define (contains-failure? ls)
  (cond
   [(empty? ls)
    #f]
   [(equal? 'failure (car ls))
    #t]
   [else
    (contains-failure? (cdr ls))]))

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
			   
			   (place-channel-put res-ch res)])
		   (loop))))

(define (run-in-place f pid)
  (define-values (res-ch res-ch*) (place-channel))
  
  (place-channel-put enq-ch (vector f pid res-ch*))
  
  (place-channel-get res-ch))
