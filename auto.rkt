#lang racket
(require racket/hash "cons.rkt")
(provide (all-defined-out))

(define (randomise probability)
  (define r (random))
  (for/last
      ([s (in-naturals)]
       [p (in-list (list probability 1))]
       #:final (< r p)) s))

(define ACTIONS# 3)
(define ACTIONS (list 'L 'M 'H))
(define (random-action)
  (random ACTIONS#))

(struct automaton (head body) #:transparent)
(struct state (action dispatch) #:transparent)
(define (make-random-automaton states#)
  (define init (random states#))
  (define to-detach (random states#))
  (define (make-head) (hash 'INIT init
                            'PAY 0))
  (define ids (build-list states# values))
  (define (make-body) 
    (apply hash (flatten (map list ids (make-states)))))
  (define (make-states)
    (build-list states# make-state))
  (define (make-state _) (state (random-action) (make-transition)))
  (define (make-transition)
    (hash 0 (random states#)
          1 (random states#)
          2 (random states#)))
  (automaton (make-head) (make-body)))

(define (reset a)
  (match-define (automaton head body) a)
  (define new-head (hash-set head 'PAY 0))
  (automaton new-head body))

(define (flatten-automaton a)
  (match-define (automaton head body) a)
  (define flatten-head
    (list
     (hash-ref head 'PAY)
     (hash-ref head 'INIT)))
  (define l (hash-count body))
  (define (flatten-state s)
    (match-define (state action dispatch) s)
    (list 
     action
     0
     (hash-ref dispatch 0)
     1
     (hash-ref dispatch 1)
     2
     (hash-ref dispatch 2)))
  (define flatten-body
    (for/list ([i (in-range l)])
      (list
       i
       (flatten-state (hash-ref body i)))))
  (flatten (append flatten-head flatten-body)))

;; CLASICS AUTOMATA

(define (L)
  (automaton (hash 'INIT 0 'PAY 0)
             (hash 0 (state 0 (hash 0 0 1 0 2 0)))))
(define (M)
  (automaton (hash 'INIT 0 'PAY 0)
             (hash 0 (state 1 (hash 0 0 1 0 2 0)))))
(define (H)
  (automaton (hash 'INIT 0 'PAY 0)
             (hash 0 (state 2 (hash 0 0 1 0 2 0)))))

(define (A)
  (automaton (hash 'INIT 1 'PAY 0)
             (hash 0 (state 0 (hash 0 2 1 1 2 0))
                   1 (state 1 (hash 0 2 1 1 2 0))
                   2 (state 2 (hash 0 2 1 1 2 0)))))


;; IMMUTABLE MUTATION

(define (mutate-marginally a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define mutate-initial (random l))
  (define mutate-state (random l))
  (match-define (state action dispatch) 
                (hash-ref body mutate-state))
  (define r (random 3))
  (define new-head
    (cond [(zero? r) 
           (hash-set head 'INIT mutate-initial)]
          [else head])) ; leave head unchanged (change body)
  (define new-body
    (cond [(zero? r) body] ; leave body unchanged (change head)
          [(= r 1)
           (hash-set body mutate-state ; mutate the action in the state
                     (state (random-action) dispatch))]
          [(= r 2)
           (hash-set body mutate-state ; mutate the dispatching rule in the state
                     (state action
                            (hash-set dispatch (random-action) (random l))))]))
  (automaton new-head new-body))

(define (add-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (define (make-transition)
    (hash 0 (random (+ l 1))
          1 (random (+ l 1))
          2 (random (+ l 1))))
  (define (make-state) (state (random-action) (make-transition)))
  (define mutate-state (random l))
  (match-define (state action dispatch) (hash-ref body mutate-state))
  (define new-body                                                              
    (hash-union
     (hash-set body mutate-state
               (state action
                      (hash-set dispatch (random-action) l)))
     (hash l (make-state))))
  (automaton head new-body))

(define (random-mem l)
  (list-ref l (random (length l))))

(define (detach-state a)
  (match-define (automaton head body) a)
  (define l (hash-count body))
  (cond
   [(= l 1) (mutate-marginally a)]
   [else (begin
           (define (random-but n r)
             (random-mem (remq mutate-state (build-list n values))))
           (define mutate-state (random l))
           (define (check-rule rule)
             (match-define (cons opponent-action reaction) rule)
             (if (= mutate-state reaction)
                 (cons opponent-action (random-but l mutate-state))
                 rule))
           (define (check-dispatch rules)
             (apply hash
                    (flatten
                     (map check-rule (hash->list rules)))))
           (define (check-state a-state)
             (match-define (state action rules) a-state)
             (struct-copy state a-state [dispatch (check-dispatch rules)]))
           (define new-body                                                     
             (for/list([i (in-range l)])
               (list i
                     (check-state (hash-ref body i)))))
           (automaton head (apply hash (flatten new-body))))]))

(define (mutate a)
  (define r (random 3))
  (cond [(zero? r) (mutate-marginally a)]
        [(= r 1) (add-state a)]
        [(= r 2) (detach-state a)]))

(define (mutates au n)
  (cond [(zero? n) '()]
        [else
         (define new (mutate au))
         (cons au (mutates new (- n 1)))]))

(define PAYOFF-TABLE
  (list
   (list (cons 2 2) (cons 2 5) (cons 2 8))
   (list (cons 5 2) (cons 5 5) (cons 0 0))
   (list (cons 8 2) (cons 0 0) (cons 0 0))))


(define (payoff action1 action2)
  (list-ref
   (list-ref PAYOFF-TABLE action1)
   action2))

(define (interact au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (values 
   ;; (reverse results)
   (automaton (hash-set head1 'PAY (round5 pay1)) body1)
   (automaton (hash-set head2 'PAY (round5 pay2)) body2)))

(define (interact-r au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (cons
   ;; (reverse results)
(round1 pay1) (round1 pay2))) 

(define (interact-s au1 au2)
  (match-define (automaton head1 body1) au1)
  (match-define (automaton head2 body2) au2)
  (define-values (next1 next2 pay1 pay2 results)
    (for/fold ([current1 (hash-ref head1 'INIT)]
               [current2 (hash-ref head2 'INIT)]
               [payoff1 (hash-ref head1 'PAY)]
               [payoff2 (hash-ref head2 'PAY)]
               [results '()])
              ([_ (in-list DELTAS)])
      (match-define (state action1 dispatch1) (hash-ref body1 current1))
      (match-define (state action2 dispatch2) (hash-ref body2 current2))
      (match-define (cons pay1 pay2) (payoff action1 action2))
      (define n1 (hash-ref dispatch1 action2))
      (define n2 (hash-ref dispatch2 action1))
      (define result (list pay1 pay2))
      (values n1 n2
              (+ payoff1 (* _ pay1))
              (+ payoff2 (* _ pay2))
              (cons result results))))
  (values 
   (take (reverse results) 20)
   (automaton (hash-set head1 'PAY (round5 pay1)) body1)
   (automaton (hash-set head2 'PAY (round5 pay2)) body2)))
    
(define (round5 n)
  (/ (round (* 100000 n))
     100000))
(define (round1 n)
  (/ (round (* 10 n))
     10))
;;benchmark

(define BENCHMARKS (list (L) (M) (H) (A)))

(define (benchmark au)
  (cons (interact-r au au)
        (for/list ([i (in-list BENCHMARKS)])
          (interact-r au i))))

(define (create-matrix au)
  (define ls (cons au BENCHMARKS))
  (for/list ([i (in-list ls)])
    (benchmark i)))

(define (interact-g au aus num)
  (define res
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (cons
       (* (car (interact-r au i)) j)
       (* (cdr (interact-r au i)) j))))
  (cons
   (/ (apply + (map car res)) 100)
   (/ (apply + (map cdr res)) 100)))

(define (interact-g-r aus num au)
  (define res (interact-g-r au aus num))
  (reverse-p res))

(define (interact-g-itself aus num)
  (define res-m-h
    (for/list ([i (in-list aus)]
               [j (in-list num)])
      (* j (car (interact-g i aus num)))))
  (define res-m
    (round5 (/ (apply + res-m-h) 100)))
  (cons res-m res-m))

(define (benchmark-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (list
   (interact-g-itself aus num)
   (interact-g-r aus num (L))
   (interact-g-r aus num (M))
   (interact-g-r aus num (H))
   (interact-g-r aus num (A))))

(define (reverse-p pair)
  (match-define (cons a b) pair)
  (cons b a))
  
(define (create-matrix-m mix)
  (define aus (map car mix))
  (define num (map cdr mix))
  (cons
   (benchmark-m mix)
   (for/list ([i (in-list BENCHMARKS)])
     (cons (interact-g i aus num) (benchmark i)))))
  
  
  
