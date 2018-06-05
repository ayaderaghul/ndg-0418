#lang racket

(require "invest.rkt" "inout.rkt")

(provide (all-defined-out))

(define STEP 100000)

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

(define (report2 input file-name)
  (define out (open-output-file file-name #:exists 'append))
  (define d (flatten input))
  (fprintf out "REPORT\n\n")
  (define (read-input data)
    (cond [(null? data) result]
          [(define x (string->number (first data)))
           (define c (- 1000000 x))
           (define start (get-position (number->string x) data))
           (define x2 (- x 100))
           (define end (get-position (number->string x2) data))
           (define da (drop (take data end) (add1 start)))
           (define l (length da))
           (fprintf out "Cycle: ~a, no: ~a\n" cycle l)
           (define res (invest-1 da))
           (for ([r (in-list res)])
             (fprintf out r)
             (fprintf out "\n"))
           (read-input (drop data l))]))
  (read-input d)
  (close-output-port out))
         


