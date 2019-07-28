#lang racket/base
(require racket/match
         racket/promise)

#|

 abc
 def
 ghi

 state = (LSB) Xs Os
 marks = (LSB) abcd efgh i

|#

(define (bit m)
  (arithmetic-shift 1 m))
(define (bitwise-bit-set n m)
  (bitwise-ior n (bit m)))

(define STATES (make-hasheq))
(define SCOUNT 0)
(define-syntax-rule (state st n)
  (hash-ref! STATES st (λ () (set! SCOUNT (add1 SCOUNT)) n)))

(struct *end (scores))
(struct *middle (choices))
(define-syntax-rule (end st sc ...) (state st (*end (vector sc ...))))
(define-syntax-rule (middle st cs) (state st (delay (*middle cs))))

(define rows 3)
(define cols 3)
(define slots (* rows cols))

(define X-start 0)
(define O-start (+ X-start slots))
(define X-end O-start)
(define O-end (+ O-start slots))

(define (cell r c)
  (bit (+ c (* r 3))))

(define (seq r c dr dc)
  (for/sum ([i (in-range 3)])
    (cell (+ r (* i dr)) (+ c (* i dc)))))
(define (row x) (seq x 0 0 +1))
(define (col x) (seq 0 x +1 0))

(define winning
  (vector (row 0) (row 1) (row 2)
          (col 0) (col 1) (col 2)
          (seq 0 0 +1 +1) (seq 0 2 +1 -1)))
(define (winning? n)
  (for/or ([c (in-vector winning)])
    (= c (bitwise-and n c))))

(define complete
  (for*/sum ([r (in-range rows)]
             [c (in-range cols)])
    (cell r c)))
(define (complete? n)
  (= n complete))

(define COUNT 0)
(define (ttt me-start them-start n)
  (set! COUNT (add1 COUNT))
  (define Xs (bitwise-bit-field n X-start X-end))
  (define Os (bitwise-bit-field n O-start O-end))
  (define B (bitwise-ior Xs Os))
  (cond
    [(winning? Xs) (end n 1 0)]
    [(winning? Os) (end n 0 1)]
    [(complete? B) (end n 0 0)]
    [else
     (middle
      n
      (for/fold ([cs '()])
                ([i (in-range slots)])
        (if (bitwise-bit-set? B i)
          cs
          (cons (ttt them-start me-start
                     (bitwise-bit-set n (+ me-start i)))
                cs))))]))

(define game (time (ttt X-start O-start 0)))

(define (explore! g)
  (match g
    [(? *end?) 1]
    [(*middle cs) (+ 1 (explore! cs))]
    ['() 0]
    [(cons a d) (+ (explore! a) (explore! d))]
    [(? promise?) (explore! (force g))]))
(time (explore! game))
COUNT
SCOUNT
