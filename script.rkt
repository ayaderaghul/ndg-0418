
(require "inout.rkt")
(require "auto.rkt")

(define d (csvfile->list "/home/chi/Downloads/ndg-0418-3/991rank"))
(define da (population-at d 50000 500000))
(define dat (resurrect da))
(define a (first dat))

