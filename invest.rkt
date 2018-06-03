#lang racket

(require "auto.rkt" "inout.rkt" "dot.rkt")
(provide (all-defined-out))

(define D (csvfile->list "/home/chi/Downloads/ndg-0418-3/991rank"))
(define AT 200000)

(define (invest at)
  (define da (population-at D at 1000000))
  (define l (length da))
  (define dat
    (if (= l 1)
        (resurrect da)
        (resurrect-n da)))
  (define mat
    (if (= l 1)
        (create-matrix (first dat))
        (create-matrix-m dat)))
  (print-matrix mat))

