#lang racket

(require "auto.rkt" "inout.rkt" "dot.rkt" "plot.rkt" "cons.rkt")
(provide (all-defined-out))

(define D (csvfile->list "/home/chi/Downloads/ndg-0418-3/901rank"))

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

(define Me (csvfile->list "/home/chi/Downloads/ndg-0418-3/901mean"))

(define (plot-interval from to pic tit)
  (define Mea (drop (take Me to) from))
  (define Mean (input->numbers Mea))
(plot-mean-i Mean from to DELTA ROUNDS pic tit))
 
