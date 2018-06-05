#lang racket

(require "invest.rkt" "inout.rkt")

(provide (all-defined-out))

(define STEP 10000)

(define (report file-name)
  (define out (open-output-file file-name #:exists 'append))
  (fprintf out "REPORT\n")
  (for ([i (in-range 100)])
    (define cycle (* i STEP))
    (define da (population-at D cycle 1000000))
    (define l (length da))
    (fprintf out "Cycle: ~a, no: ~a\n" cycle l)
    (define res (invest cycle))
    (for ([r (in-list res)])
      (fprintf out r)
      (fprintf out "\n")))
  (close-output-port out))


