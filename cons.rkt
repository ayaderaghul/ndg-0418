#lang racket
(provide (all-defined-out))
;; CONFIGURATION
(define SIM-ID 1)
(define LOCATION 0) ;; 0 home 1 school lab

(define N 100)
(define CYCLES 500) ;; careful, you change the cycles here
(define SPEED 10)
(define ROUNDS 500) ;; not here
(define DELTA .99)
(define DELTAstr (string-trim (number->string (* DELTA 100)) ".0"))
(define MUTATION 2) ;; 3x3 game needs more mutation

(define DELTAS (build-list ROUNDS (lambda (x) (expt DELTA x))))

(define OUTLABstr "/Users/linhchi.nguyen/Dropbox/ndg-0418/")
