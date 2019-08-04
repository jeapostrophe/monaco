#lang racket/base
(require racket/match
         raart
         monaco/util
         monaco)

;; TTT

(define rows 3)
(define cols 3)
(define slots (* rows cols))

(define player-idx 0)
(define X-start (+ player-idx 1))
(define O-start (+ X-start slots))
(define X-end O-start)
(define O-end (+ O-start slots))

(define ttt-init 0)

(define (cell-idx r c)
  (+ c (* r cols)))
(define (cell r c)
  (bit (cell-idx r c)))

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

(define-functor (open-ttt st)
  (define Xs (bitwise-bit-field st X-start X-end))
  (define Os (bitwise-bit-field st O-start O-end))
  (define B (bitwise-ior Xs Os))
  (define x? (bitwise-bit-set? st player-idx)))

(define (ttt-who st)
  (open-ttt st)
  (if x? 1 0))

(define text:X (text "X"))
(define text:O (text "O"))
(define text:_ (text " "))
(define (ttt-render-st st)
  (open-ttt st)
  (vappend
   #:halign 'left
   (table
    (for/list ([r (in-range rows)])
      (for/list ([c (in-range cols)])
        (define b (cell-idx r c))
        (cond [(bitwise-bit-set? Xs b) text:X]
              [(bitwise-bit-set? Os b) text:O]
              [else text:_]))))
   (text (format "~a's turn" (if x? "X" "O")))))

(define (ttt-terminal? st)
  (open-ttt st)
  (or (winning? Xs)
      (winning? Os)
      (complete? B)))

(define (ttt-score st)
  (open-ttt st)
  (cond
    [(winning? Xs) (vector -1 +1)]
    [(winning? Os) (vector +1 -1)]
    [else (vector 0 0)]))

(define all-actions
  (for*/list ([r (in-range rows)]
              [c (in-range cols)])
    (cons r c)))

(define (ttt-render-a a)
  (match-define (cons r c) a)
  (action (string-ref "qweasdzxc" (cell-idx r c))
          (format "Select ~a,~a" (add1 r) (add1 c))
          a))

(define (ttt-legal st)
  (open-ttt st)
  (filter
   (match-lambda
     [(cons r c)
      (not (bitwise-bit-set? B (cell-idx r c)))])
   all-actions))

(define (ttt-aeval st a)
  (open-ttt st)
  (define st-other (bitwise-bit-flip st player-idx))
  (define me-start (if x? X-start O-start))
  (match-define (cons r c) a)
  (bitwise-bit-set st-other (+ (cell-idx r c) me-start)))

(module+ main
  (mcts-play! ttt-who ttt-terminal? ttt-score
              ttt-legal ttt-aeval ttt-render-st ttt-render-a
              ttt-init 0))
