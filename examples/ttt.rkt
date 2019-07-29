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

(define (show-mem)
  (printf "~a MB\n" (real->decimal-string (/ (current-memory-use) (* 1024 1024)))))
(define (bit m)
  (arithmetic-shift 1 m))
(define (bitwise-bit-set n m)
  (bitwise-ior n (bit m)))
(define (bitwise-bit-flip n m)
  (bitwise-xor n (bit m)))

(define STATES (make-hash))
(define-syntax-rule (state st n)
  (hash-ref! STATES st (λ () n)))

(struct *end (scores))
(struct *middle (choices))
(define-syntax-rule (end st sc ...) (state st (*end (vector sc ...))))
(define-syntax-rule (middle st cs) (state st (delay (*middle cs))))

(define rows 3)
(define cols 3)
(define slots (* rows cols))

(define player-idx 0)
(define X-start 1)
(define O-start (+ X-start slots))
(define X-end O-start)
(define O-end (+ O-start slots))

(define init-state 0)

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
(define (ttt n)
  (set! COUNT (add1 COUNT))
  (define x? (bitwise-bit-set? n player-idx))
  (define n-next (bitwise-bit-flip n player-idx))
  (define me-start (if x? X-start O-start))
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
      (for/list ([i (in-range slots)]
                 #:unless (bitwise-bit-set? B i))
        (ttt (bitwise-bit-set n-next (+ me-start i)))))]))

(show-mem)
(define game (time (ttt init-state)))

(define (explore! g)
  (match g
    [(? *end?) 1]
    [(*middle cs) (+ 1 (explore! cs))]
    ['() 0]
    [(cons a d) (+ (explore! a) (explore! d))]
    [(? promise?)
     (if (promise-forced? g)
       0
       (explore! (force g)))]))
(time (explore! game))
(show-mem)
COUNT
