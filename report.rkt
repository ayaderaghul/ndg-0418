#lang racket

(require "invest.rkt" "inout.rkt" "cons.rkt") 

(provide (all-defined-out))

(define STEP 10000)
(define (report d file-name)
  (define out (open-output-file file-name #:exists 'append))
  (fprintf out "REPORT\n")
  (for ([i (in-range 100)])
    (define cycle (* i STEP))
    (define da (population-at d cycle 1000000))
    (define l (length da))
    (fprintf out "Cycle: ~a, no: ~a\n" cycle l)
    (define res (invest cycle d))
    (for ([r (in-list res)])
      (fprintf out r)
      (fprintf out "\n")))
  (close-output-port out))

(define (report2 input file-name)
  (define out (open-output-file file-name #:exists 'append))
  (define d (flatten input))
  (fprintf out "REPORT\n\n")
  (define (read-input data)
    (cond [(null? data) '()]
          [else
           (define x (string->number (first data)))
           ;(print "after x")
           (define c (- 1000000 x))
         
           ;(print "after start")
           (define end (get-posn-next-number data))
          (define da
             (if (= end (sub1 (length data)))
                 (drop data 1)
                 (drop (take data end) 1)))
           (define l (length da))
           ;(print "after l")
           (fprintf out "Cycle: ~a, no: ~a\n" c l)
           (define res (invest-1 da))
           ;(print "after res")
           (fprintf out res)
           (fprintf out "\n")
           (read-input (drop data (+ 1 l)))]))
  (read-input d)
  (close-output-port out))

(define (report2-from from input file-name)
   (define out (open-output-file file-name #:exists 'append))
  (define d (flatten input))
  (define s (- 1000000 from))
  (define start (get-position (number->string s) d))
  (define d2 (drop d start))
  (define (read-input data)
    (cond [(null? data) '()]
          [else
            (define x (string->number (first data)))
            (print "after x")
            (define c (- 1000000 x))
            (print "after c")
            (define end (get-posn-next-number data))
            (define da
              (if (= end (sub1 (length data)))
                  (drop data 1)
                  (drop (take data end) 1)))
            (print "after da")
            (define l (length da))
            (fprintf out "Cycle: ~a, no: ~a\n" c l)
            (define res (invest-1 da))
            (print "after res")
            (fprintf out res)
            (fprintf out "\n")
           (read-input (drop data (+ 1 l)))]))
  (read-input d2)
  (close-output-port out))

(define (gen-out id)
  (string-append "/Users/linhchi.nguyen/Dropbox/ndg-0418-3/"
                 DELTAstr (number->string id) ".txt"))
;(define OUTFILE (gen-out 1))
(define (gen-in id)
  (string-append "/Users/linhchi.nguyen/Dropbox/ndg-0418-3/"
                 DELTAstr (number->string id) "rank"))
         
;(define INFILE (csvfile->list (gen-in 1)))

(define (main)
  (for ([i (in-list (list 1 2))])
    (define OUTFILE (gen-out i))
    (define INFILE (csvfile->list (gen-in i)))
    (report2 INFILE OUTFILE)))
