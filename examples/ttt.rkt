#lang racket/base
(require (for-syntax racket/base
                     syntax/parse))

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
(define (bitwise-bit-flip n m)
  (bitwise-xor n (bit m)))

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

(define-syntax (condlet stx)
  (syntax-parse stx
    [(_ [#:when question answer ...+])
     #'(when question answer ...)]
    [(_ [#:cond question answer ...+] . more)
     #'(if question (let () answer ...) (condlet . more))]
    [(_ code)
     #'code]
    [(_ code . more)
     #'(let () code (condlet . more))]))

(define (ttt st)
  (condlet
   (define Xs (bitwise-bit-field st X-start X-end))
   [#:cond (winning? Xs) (vector 1 0)]
   (define Os (bitwise-bit-field st O-start O-end))
   [#:cond (winning? Os) (vector 0 1)]
   (define B (bitwise-ior Xs Os))
   [#:cond (complete? B) (vector 0 0)]
   (define x? (bitwise-bit-set? st player-idx))
   (define st-other (bitwise-bit-flip st player-idx))
   (define me-start (if x? X-start O-start))
   (for/list ([i (in-range slots)]
              #:unless (bitwise-bit-set? B i))
     (bitwise-bit-set st-other (+ me-start i)))))

;; Engine

(define (show-mem)
  (printf "~a MB\n" (real->decimal-string (/ (current-memory-use) (* 1024 1024)))))

(define (explore! choices st0)
  (define VISITED? (make-hash))
  (let loop ([st st0])
    (condlet
     [#:cond (hash-has-key? VISITED? st) 0]
     (hash-set! VISITED? st #t)
     (define opts (choices st))
     [#:cond (vector? opts) 1]
     (for/sum ([o (in-list opts)])
       (loop o)))))

(show-mem)
(time (explore! ttt init-state))
(show-mem)
