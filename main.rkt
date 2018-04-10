#lang racket
(require "auto.rkt")
(require "inout.rkt")
(require plot)
(require racket/hash)
(plot-new-window? #t)
;;(require racket/future)
(provide (all-defined-out))
;;todo
;; markov automaton
;; CONFIGURATION
(define SIM-ID 3)

(define N 100)
(define CYCLES 100000)
(define SPEED 10)
(define ROUNDS 500)
(define DELTA .99)
(define MUTATION 2)

(define (build-random-population n)
  (build-vector n (lambda (_) (make-random-automaton 1))))
(define (population-payoffs population)
  (for/list
      ([auto population])
    (hash-ref (automaton-head auto) 'PAY)))
(define (match-population population rounds delta)
  ;(population-reset population)
  (for
      ([i (in-range 0 (- (vector-length population) 1) 2)])
    (define auto1 (vector-ref population i))
    (define auto2 (vector-ref population (+ i 1)))
    (define-values (a1 a2)
      (interact auto1 auto2 rounds delta))
    (vector-set! population i a1)
    (vector-set! population (+ i 1) a2))
  population)

(define (sum l)
  (apply + l))

(define (payoff->fitness population)
  (define payoffs (population-payoffs population))
  (define total (sum payoffs))
  (for/list ([p (in-list payoffs)])
    (/ p total)))

(define (shuffle-vector vec)
  (define lst (vector->list vec))
  (define l2 (shuffle lst))
  (list->vector l2))

(define (accumulate-fitness probabilities)
  (let relative->absolute
      ([payoffs probabilities] [so-far #i0.0])
    (cond
      [(empty? payoffs) '()]
      [else (define nxt (+ so-far (first payoffs)))
            (cons nxt (relative->absolute (rest payoffs) nxt))])))
(define (randomise-s probabilities speed)
  (define fitness (accumulate-fitness probabilities))
  (for/list ([n (in-range speed)])
    (define r (random))
    (for/last ([p (in-naturals)]
               [% (in-list fitness)]
               #:final (< r %)) p)))

(define (regenerate population rate)
  (define probabilities (payoff->fitness population))
  (define substitutes (randomise-s probabilities rate))
  (for ([i (in-range rate)]
        [auto (in-list substitutes)])
    (vector-set! population i
                 (vector-ref population auto)))
  (shuffle-vector population))


(define (population-reset population)
  (for ([auto population]
        [i (in-naturals)])
    (vector-set! population i (reset auto))))

(define (average lst)
  (exact->inexact (/ (sum lst) (length lst))))

(define (scan population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h au add1 0))
   (hash)
   p))
(define (scan-f population)
  (define p (vector->list population))
  (foldl
   (lambda (au h)
     (hash-update h (flatten-automaton au) add1 0))
   (hash)
   p))

(define (population-mean->lines data)
  (define coors
    (for/list ([d (in-list data)]
               [n (in-naturals)])
      (list n d)))
  (lines coors))

(define (compound d r)
  (foldl (lambda (n a) (+ a (expt d n))) 1 (build-list (- r 1) add1)))

(define (plot-mean-p data delta rounds)
  (define low (* 2 (compound delta rounds)))
  (define medium (* 5 (compound delta rounds)))
  (define high (* 8 (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800))

(define (plot-mean data delta rounds pic)
  (define low (* 2 (compound delta rounds)))
  (define medium (* 5 (compound delta rounds)))
  (define high (* 8 (compound delta rounds)))
  (define line1
    (function (lambda (x) low) #:color "blue"))
  (define line2
    (function (lambda (x) medium) #:color "green"))
  (define line3
    (function (lambda (x) high) #:color "red"))
  (plot (list line1 line2 line3
              (population-mean->lines data))
        #:y-min 0 #:y-max (+ 5 high) #:width 1200 #:height 800
        #:out-file pic))


(define (sort-population p)
 (sort (hash->list (scan (vector-map reset p)))
       > #:key cdr))

;; MUTATE
(define (mutate-population population rate)
  (for ([i (in-range rate)])

    (define auto (vector-ref population i))
    (vector-set! population i (mutate auto))))

(define (evolve population cycles speed mutation rounds delta mean-file rank-file p-file sim-id)
  (cond
    [(zero? cycles) (out-population sim-id (scan-f population) p-file)]
    [else
     (and (zero? (modulo cycles 100)) (print (number->string cycles)))
     (define p2 (match-population population rounds delta))
;;     (print "matched\n")
     (define pp (population-payoffs p2))
  ;;   (print "pp-ed\n")
     (define p3 (regenerate p2 speed))
    ;; (print "regenerated\n")
     (define p4 (vector-map reset p3))
     ;;(print "reset-ed\n")
     (and (zero? (modulo cycles 100)) (out-rank cycles (scan-f p4) rank-file))
     ;;(print "out-ranked\n")
     (mutate-population p4 mutation)
     ;;(print "mutated\n")
     (out-data mean-file (list (list (average pp))))
     ;;(print "out-meaned\n")
     (evolve p4 (- cycles 1)
             speed mutation rounds delta mean-file rank-file p-file sim-id)]))

(define (evolve-p population cycles speed mutation rounds delta)
  (cond
    [(zero? cycles) (list population)]
    [else
     (define p2 (match-population population rounds delta))
     (define pp (population-payoffs p2))
     (define p3 (regenerate p2 speed))
     (define auto (vector-ref p3 0))
     
;;     (and (zero? (modulo cycles 100)) (out-rank cycles (scan p3) rank-file))
     (mutate-population p3 mutation)    
;;     (out-data mean-file (list (list (average pp))))
     (cons (average pp)
           (evolve-p (vector-map reset p3) (- cycles 1)
                   speed mutation rounds delta))]))

(define (gen-name name id)
  (string-append (number->string id) name))

(define (main)
  (collect-garbage)
;;  (define A (build-random-population N))
  (define data (csvfile->list "p"))
  (define A (resurrect-p data))
  (define MEAN (gen-name "mean" SIM-ID))
  (define RANK (gen-name "rank" SIM-ID))
  (time (evolve A CYCLES SPEED MUTATION ROUNDS DELTA MEAN RANK "p" SIM-ID))
  (define DATA (csvfile->list MEAN))
  (define PIC (gen-name "pic.png" SIM-ID))
  (plot-mean (input->numbers DATA) DELTA ROUNDS PIC))
