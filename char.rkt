#lang racket
(require "inout.rkt" "cons.rkt" "plot.rkt")
(provide (all-defined-out))

(define (plot-pays-f id)
  (define INPAYS (gen-outs id))
  (define PAYS (csvfile->list INPAYS))
  (define PAYPIC (gen-name 1 id "pay.png"))
  (define wl (input->numbers (map first PAYS)))
  (define tm (input->numbers (map second PAYS)))
  (define th (input->numbers (map third PAYS)))
  (define wa (input->numbers (map fourth PAYS)))
  (define ta (input->numbers (map fifth PAYS)))
  (define ts (input->numbers (map last PAYS)))
  (plot-pays wl tm th wa ta ts DELTA ROUNDS PAYPIC "personaltiy test"))
		
(define (main)
  (for ([i (in-list (list 1 2))])
    (plot-pays-f i)))