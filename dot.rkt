#lang racket
(require "auto.rkt" "inout.rkt")
(provide (all-defined-out))

(define (to-dot au name file-name)
  (match-define (automaton head body) au)
  (define init (hash-ref head 'INIT))
  (define s (hash-count body))
  (define (num->act x)
    (cond [(= x 0) "L"]
          [(= x 1) "M"]
          [(= x 2) "H"]))
  (define out (open-output-file file-name #:exists 'replace))
  (fprintf out
           "digraph ~a {
rankdir = LR
~a [peripheries = 2]

" name init)
  (for ([i (in-range s)])
    (match-define (state action dispatch) (hash-ref body i))
    (fprintf out
             "~a [label = \"~a\"]
~a -> ~a [label = \"L\"]
~a -> ~a [label = \"M\"]
~a -> ~a [label = \"H\"]

" i (num->act action)
i (hash-ref dispatch 0)
i (hash-ref dispatch 1)
i (hash-ref dispatch 2)))
  (fprintf out "}\n")
  (close-output-port out))

   
   



