#lang racket/base

(provide debug?
	 SHELL)

(define debug? (make-parameter #f))
(define SHELL "/bin/sh -c")
