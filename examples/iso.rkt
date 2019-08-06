#lang racket/base
(require racket/match
         racket/vector
         racket/list
         raart
         monaco/util
         monaco)

(define rows 6)
(define cols 8)
(define cells (* rows cols))
(define (cell-idx r c)
  (+ c (* r cols)))
(define (cell r c)
  (bit (cell-idx r c)))

(struct iso (who phase ps board))
(define iso-init
  (iso 0 'move (vector (cons 2 0) (cons 3 (sub1 cols))) 0))

(define (iso-terminal? st)
  (and (eq? 'move (iso-phase st))
       (empty? (iso-legal st))))

(define (iso-score st)
  (if (zero? (iso-who st))
    (vector  0.0 +1.0)
    (vector +1.0  0.0)))

(struct a:move (dr dc) #:prefab)
(struct a:select-row (r) #:prefab)
(struct a:select-col (c) #:prefab)
(define deltas '(-1 0 +1))
(define moves
  (for*/list ([dr (in-list deltas)]
              [dc (in-list deltas)]
              #:unless (= dr dc 0))
    (a:move dr dc)))
(define (iso-aeval st a)
  (match a    
    [(a:move dr dc)
     (define ps (iso-ps st))
     (define who (iso-who st))
     (match-define (cons r c) (vector-ref ps who))
     (define nps (vector-copy ps))
     (vector-set! nps who (cons (+ dr r) (+ dc c)))
     (struct-copy iso st
                  [phase 'select-row]
                  [ps nps])]
    [(a:select-row r)
     (struct-copy iso st
                  [phase (cons 'select-col r)])]
    [(a:select-col c)
     (match-define (cons 'select-col r) (iso-phase st))
     (struct-copy iso st
                  [who (modulo (add1 (iso-who st)) 2)]
                  [phase 'move]
                  [board (bitwise-bit-set (iso-board st) (cell-idx r c))])]))

(define-functor (open-iso st)
  (define B
    (for/fold ([B (iso-board st)]) ([p (in-vector (iso-ps st))])
      (match-define (cons r c) p)
      (bitwise-bit-set B (cell-idx r c)))))
;; XXX Some way to derive this
(define (iso-random-legal st)
  (open-iso st)
  (match (iso-phase st)
    ['move
     (match-define (cons r c) (vector-ref (iso-ps st) (iso-who st)))
     (let loop ()
       (define dr (- (random 3) 1))
       (define dc (- (random 3) 1))
       (define rp (+ dr r))
       (define cp (+ dc c))
       (if (and (not (= dr dc 0))
                (<= 0 rp) (<= 0 cp)
                (< rp rows) (< cp cols)
                (not (bitwise-bit-set? B (cell-idx rp cp))))
         (a:move dr dc)
         (loop)))]
    ['select-row
     (let loop ()
       (define r (random rows))
       (if (for/or ([c (in-range cols)])
             (not (bitwise-bit-set? B (cell-idx r c))))
         (a:select-row r)
         (loop)))]
    [(cons 'select-col r)
     (let loop ()
       (define c (random cols))
       (if (bitwise-bit-set? B (cell-idx r c))
         (loop)
         (a:select-col c)))]))
(define (iso-legal st)
  (open-iso st)
  (match (iso-phase st)
    ['move
     (match-define (cons r c) (vector-ref (iso-ps st) (iso-who st)))
     (filter
      (match-lambda
        [(a:move dr dc)
         (define rp (+ dr r))
         (define cp (+ dc c))
         (and (<= 0 rp) (<= 0 cp)
              (< rp rows) (< cp cols)
              (not (bitwise-bit-set? B (cell-idx rp cp))))])
      moves)]
    ['select-row
     (for/list ([r (in-range rows)]
                #:when (for/or ([c (in-range cols)])
                         (not (bitwise-bit-set? B (cell-idx r c)))))
       (a:select-row r))]
    [(cons 'select-col r)
     (for/list ([c (in-range cols)]
                #:unless (bitwise-bit-set? B (cell-idx r c)))
       (a:select-col c))]))

(define row-keys "qwerty")
(define col-keys "asdfghjk")
(define (keys->header ks)
  (for/list ([k (in-string ks)])
    (style 'bold (text (string k)))))
(define col-header (cons (blank) (keys->header col-keys)))
(define row-header (keys->header row-keys))
(define (iso-render-st st)
  (define board (iso-board st))
  (match-define (vector (cons p0r p0c) (cons p1r p1c)) (iso-ps st))
  (define (pd w) (style (if (= (iso-who st) w) 'inverse 'normal)
                        (text (format "~a" w))))
  (define p0d (pd 0))
  (define p1d (pd 1))
  (vappend
   #:halign 'left
   (table
    (list* col-header
           (for/list ([r (in-range rows)]
                      [rh (in-list row-header)])
             (cons rh
                   (for/list ([c (in-range cols)])
                     (define b (cell-idx r c))
                     (cond [(bitwise-bit-set? board b) (text "#")]
                           [(and (= r p0r) (= c p0c)) p0d]
                           [(and (= r p1r) (= c p1c)) p1d]
                           [else (text " ")]))))))
   (text (format "~a's turn: ~a" (iso-who st) (iso-phase st)))))

(define (iso-render-a a)
  (action
   (match a
     [(a:move dr dc) (string-ref "qweasdzxc" (+ (add1 dc) (* (add1 dr) 3)))]
     [(a:select-row r) (string-ref row-keys r)]
     [(a:select-col c) (string-ref col-keys c)])
   #f a))

(module+ main
  (mcts-play! iso-who iso-terminal? iso-score
              iso-legal iso-random-legal
              iso-aeval iso-render-st iso-render-a
              iso-init 0))
