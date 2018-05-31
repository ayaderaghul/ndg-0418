#lang racket
(require racket/hash plot)
(require "auto.rkt" "main.rkt" "inout.rkt")
(provide (all-defined-out))

(define (generate-population l m)
  (define ls (build-vector l (lambda (_) (L))))
  (define ms (build-vector m (lambda (_) (M))))
  (define hs (build-vector (- 1000 l m) (lambda (_) (H))))
  (vector-append ls ms hs))

(define (scan-for-LM pop)
  (define rank (scan pop))
  (list
   (hash-ref rank (L) 0)
   (hash-ref rank (M) 0)))

(define TESTS
  (list
   (list 50 50)
   (list 50 100)
   (list 50 150)
   
   (list 50 250)
   (list 50 350)
   (list 400 200)
   
   (list 900 50)
   (list 800 150)
   (list 700 250)
   
   (list 700 50)
   (list 600 200)
   (list 400 400)
   ))
(define FILES
  (list "0" "1" "2" "3" "4" "5" "6" "7" "8" "9" "10" "11"))

(define (evolve-t population cycles speed coor-file)
  (cond
   [(zero? cycles) (out-data coor-file (list (scan-for-LM population)))]
   [else
    (out-data coor-file (list (scan-for-LM population)))
    (define p2 (match-population population))
    (define pp (population-payoffs p2))
    (define p3 (regenerate p2 speed))
    (define p4 (vector-map reset p3))
    (evolve-t p4 (- cycles 1) speed coor-file)]))

(define (plot-rep)
  (define lines-list
    (for/list ([i (in-list FILES)])
      (lines (input->coors (csvfile->list i)))))
  (plot lines-list #:x-min 0 #:x-max 1000 #:y-min 0 #:y-max 1000 
        #:x-label "pL" #:y-label "pM" #:title "replicator dynamics: NDG"
        #:out-file "rep.png"
        ))

(define (main)
  (collect-garbage)
  (for (;[t (in-list TESTS)]
        [i (in-list (list 0 1 2 3 4 5 6 7 8 9 10 11))])
    (define pop (apply generate-population (list-ref TESTS i)))    
    (evolve-t pop 800 10 (number->string i)))
  (plot-rep))
