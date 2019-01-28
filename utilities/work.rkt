#lang racket/base

(require "makegraph.rkt"
	 "common.rkt")

(provide work)
	 
#| returns the work of the build
|#
(define (work build)
  (for/sum ([graph (buildgraph-makegraphs build)])
	   (work-graph graph)))

#| returns the work of the makegraph
|#
(define (work-graph graph)
  (define visited (make-hash))
  
  (define (node-work node-id)
    (define node (get-target graph node-id))
    (cond
     [(hash-ref visited node #f)
      0]
     [(leaf? node)
      (hash-set! visited node #t)
      (leaf-node-work node)]
     [else
      (hash-set! visited node #t)
      (non-leaf-node-work node)]))

  (define (leaf-node-work node)
    (define data (target-data node))
    
    (cond
     [(rusage-data? data)
      (rusage-data-elapsed data)]
     [(number? data)
      data]
     [else
      (error 'leaf-node-work "Unrecognized data ~a ~a" data node)]))
  
  (define (non-leaf-node-work node)
    (for/sum ([e (reverse (target-out-edges node))])
	     (node-work (edge-end e))))
  
  (node-work (makegraph-root graph)))
