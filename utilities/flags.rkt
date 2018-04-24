#lang racket/base

(provide DEBUG
         set-DEBUG!)

(define DEBUG #f)

(define (set-DEBUG! b)
  (set! DEBUG b))
