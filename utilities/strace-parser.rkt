#lang errortrace racket

(require parsack
         "system_calls/process-syscalls.rkt"
	 "system_calls/system_calls.rkt")

(provide parse-strace)

(define $finalline (parser-compose
                    (skipMany $space)
                    (line <- (between (string "+++") (string "+++") (many (<!> (string "+++")))))
                    (<any> (try $eol) (lookAhead $eof))
                    (return line)))

(define $aparen (parser-seq
                 (many (<!> (<any> (char #\\) (char #\") (char #\() (char #\)))))
                 (<any> $escaped $quotes $openparen $closeparen)
                 #:combine-with append))

(define $escaped (parser-seq
                  (char #\\)
                  $anyChar))

(define $quotes (parser-compose
                 (q <- (char #\"))
                 (qc <- (getState 'qc))
                 (setState 'qc (+ 1 qc))
                 (return (list q))))

(define $openparen (parser-compose
                    (p <- (char #\())
                    (qc <- (getState 'qc))
                    (pc <- (getState 'pc))
                    (setState 'pc (if (odd? qc)
                                      pc
                                      (+ 1 pc)))
                    (return (list p))))

(define $closeparen (parser-compose
                     (p <- (char #\)))
                     (qc <- (getState 'qc))
                     (pc <- (getState 'pc))
                     (setState 'pc (if (odd? qc)
                                       pc
                                       (- pc 1)))
                     (return (list p))))

(define $done (parser-compose
               (qc <- (getState 'qc))
               (pc <- (getState 'pc))
               (if (= 0 pc)
                   (return null)
                   $err)))

(define $args (withState (['pc 0]
                          ['qc 0])
                         (parser-seq
                          $openparen
                          (manyUntil $aparen $done) ;; fix this because this is returning lists...
                          #:combine-with (lambda (a b)
                                           (list->string (flatten (append a b)))))))

(define $retval (parser-seq
                 (~ (skipMany $space))
                 (~ (char #\=))
                 (~ (skipMany $space))
                 (manyUntil $anyChar (<any> (try $eol) (lookAhead $eof)))
                 #:combine-with list->string))

(define $scallline (parser-compose
                    (skipMany $space)
                    (scall <- (many (<!> (<any> (char #\() $eol))))
                    (args <- $args)
                    (retval <- $retval)
                    (return (syscall (list->string scall) args retval))))
            
(define $nomatch (parser-compose
		  (manyUntil $anyChar (<any> (try $eol) (lookAhead $eof)))
		  (return null)))
       
(define $line
  (<or> (try $finalline)
        (try $scallline)
        $nomatch))

(define (parse-file f)
  (parse-result (manyUntil $line $eof) f))

(define (parse-strace dir)
  (unless (directory-exists? dir)
    (error 'parse-strace "~a is not a directory\n" dir))

  (define names (make-hash))

  (define scalls (make-hash))
  (for ([f (in-directory dir)])
    (define ext (path-get-extension f))
    (unless ext
      (error 'parse-strace "~a does not have an extension" f))
    (define pid (string->number (bytes->string/locale ext #f 1)))

    (define syscalls (filter syscall? (parse-file f)))

    (for ([sc syscalls])
     (hash-set! names (syscall-name sc) #t))      

    (when (hash-has-key? scalls pid)
      (error 'parse-strace "Already have processed syscalls for pid ~a" pid))
    
    (hash-set! scalls pid syscalls))

  #;(for ([k (in-hash-keys names)])
    (printf "~a\n" k))

  scalls)
    
    
    
