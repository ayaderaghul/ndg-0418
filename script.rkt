
(require "inout.rkt")
(require "auto.rkt")
(require "dot.rkt")

(define d (csvfile->list "/home/chi/Downloads/ndg-0418-3/991rank"))
(define da (population-at d 25000 1000000))
(define dat (resurrect da))
(define dat2 (resurrect-n da))

(define a (first dat))

(define aus (map car dat2))
(define num (map cdr dat2))


