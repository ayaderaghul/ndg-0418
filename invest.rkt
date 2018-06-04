#lang racket

(require "auto.rkt" "inout.rkt" "dot.rkt" "plot.rkt" "cons.rkt")
(provide (all-defined-out))

(define D (csvfile->list "/home/chi/Downloads/ndg-0418-3/992rank"))

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

(define (bench at)
  (define da (population-at D at 1000000))
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

(define (per x y)
  (round (* 100 (/ x y))))
(define (app? x y)
  (match-define (list y1 y2) (list (- y 10) (+ y 10)))
  (if (and (< y1 x) (< x y2)) #t #f))

(define (test res)
  (match-define (list i l m h a s) res)
  (match-define (list i1 l1 m1 h1 a1 s1)
                (list
                 (per i 496.7) ; itself
                 (per l 794.7) ; with l
                 (per m 496.7) ; with m
                 (per h 198.7) ; with h
                 (per a 794.7) ; with a
                 (per s 794.7)
                 ))
  (cond 
   [(and (= i1 100) (app? l1 63) (app? m1 100) (app? h1 0) (app? a1 63)) 'fair]
   [(and (> i1 70) (> m1 80) (> h1 80)) 'traitor]
   [(and (> i1 70) (< m1 30) (< h1 30)) 'tough]
   [(and (> i1 80) (> l1 70) (> m1 70) (> h1 70)) 'acc]
   [(and (> i1 80) (> l1 80) (< m1 30) (< h1 30) (> a1 70)) 'doubletough]
   [else 'other]))
   
   
   
   
   
   
      
  
  
  

(define Me (csvfile->list "/home/chi/Downloads/ndg-0418-3/992mean"))

(define (plot-interval from to pic tit)
  (define Mea (drop (take Me to) from))
  (define Mean (input->numbers Mea))
(plot-mean-i Mean from to DELTA ROUNDS pic tit))
 
