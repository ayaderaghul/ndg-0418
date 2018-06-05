#lang racket

(require "auto.rkt" "inout.rkt" "dot.rkt" "plot.rkt" "cons.rkt")
(provide (all-defined-out))

;(define D (csvfile->list "/home/chi/Downloads/ndg-0418-3/992rank"))

(define (invest at d)
  (define da (population-at d at 1000000))
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

(define (invest-1 da)
  (define l (length da))
  (define dat
    (if (= l 1)
        (resurrect da)
        (resurrect-n da)))
  (define res
    (if (= l 1)
        (benchmark (first dat))
        (benchmark-m dat)))
  (define r (map cdr res))
  (define m (apply max r))
  (define (cell pair m)
    (match-define (cons p1 p2) pair)
    (if (= p2 m)
        (format "~a ~a*" p1 p2)
        (format "~a ~a " p1 p2)))
  (define (cells pairs m)
    (for/list ([i (in-list pairs)])
      (~a (cell i m) #:min-width 13 #:align 'center)))
  (apply string-append (cells res m)))



(define (bench at d)
  (define da (population-at d at 1000000))
  (define l (length da))
  (define dat
    (if (= l 1)
        (resurrect da)
        (resurrect-n da)))
  (define res
    (if (= l 1)
        (benchmark (first dat))
        (benchmark-m dat)))
  (map car res))

#|
(define Me (csvfile->list "/home/chi/Downloads/ndg-0418-3/992mean"))

(define (plot-interval from to pic tit)
  (define Mea (drop (take Me to) from))
  (define Mean (input->numbers Mea))
(plot-mean-i Mean from to DELTA ROUNDS pic tit))
|#
