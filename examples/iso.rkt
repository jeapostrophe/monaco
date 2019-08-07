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

(define iso-curr iso-who)
(define (iso-last st)
  (match (iso-phase st)
    ['move (if (zero? (iso-who st)) 1 0)]
    [_ (iso-who st)]))

(define (iso-score st)
  (if (zero? (iso-who st))
    (vector  0.0 +1.0)
    (vector +1.0  0.0)))

(define-functor (open-iso st)
  (define B
    (for/fold ([B (iso-board st)]) ([p (in-vector (iso-ps st))])
      (match-define (cons r c) p)
      (bitwise-bit-set B (cell-idx r c))))
  (define pp (vector-ref (iso-ps st) (iso-who st)))
  (define pr (car pp))
  (define pc (cdr pp)))

(define dirs 9)
(define iso-actions
  (max dirs rows cols))
(define (decode-move i)
  (define-values (q r) (quotient/remainder i 3))
  (values (sub1 q) (sub1 r)))

(define (iso-terminal? st)
  (open-iso st)
  (and (eq? 'move (iso-phase st))
       (not
        (for/or ([i (in-range dirs)])
          (iso-legal-move? B pr pc i)))))
(define (iso-legal-move? B pr pc i)
  (define-values (dr dc) (decode-move i))
  (define rp (+ dr pr))
  (define cp (+ dc pc))
  (and (<= 0 rp) (<= 0 cp)
       (< rp rows) (< cp cols)
       (not (bitwise-bit-set? B (cell-idx rp cp)))))
(define (iso-legal? st i)
  (open-iso st)
  (match (iso-phase st)
    ['move
     (and (between 0 i dirs)
          (iso-legal-move? B pr pc i))]
    ['select-row
     (and (between 0 i rows)
          (for/or ([c (in-range cols)])
            (not (bitwise-bit-set? B (cell-idx i c)))))]
    [(cons 'select-col r)
     (and (between 0 i cols)
          (not (bitwise-bit-set? B (cell-idx r i))))]))

(define (iso-aeval st i)
  (match (iso-phase st)
    ['move
     (define-values (dr dc) (decode-move i))
     (define ps (iso-ps st))
     (define who (iso-who st))
     (match-define (cons r c) (vector-ref ps who))
     (define nps (vector-copy ps))
     (vector-set! nps who (cons (+ dr r) (+ dc c)))
     (struct-copy iso st
                  [phase 'select-row]
                  [ps nps])]
    ['select-row
     (struct-copy iso st
                  [phase (cons 'select-col i)])]
    [(cons 'select-col r)
     (struct-copy iso st
                  [who (modulo (add1 (iso-who st)) 2)]
                  [phase 'move]
                  [board (bitwise-bit-set (iso-board st) (cell-idx r i))])]))

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

(define (iso-render-a st i)
  (action
   (match (iso-phase st)
     ['move (string-ref "qweasdzxc" i)]
     ['select-row (string-ref row-keys i)]
     [(cons 'select-col _) (string-ref col-keys i)])
   #f))

(module+ main
  (mcts-play! iso-actions iso-curr iso-last iso-terminal? iso-score
              iso-legal? iso-aeval
              iso-render-st iso-render-a
              iso-init 0))
