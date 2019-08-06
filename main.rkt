#lang racket/base
(require raart
         racket/math
         racket/list
         struct-define
         "util.rkt")
(provide mcts-play!
         (struct-out action))

(struct action (key desc))

;;     p = parent (could be stored in continuation)
;; 1  ia = initiating action
;; 4  cs = children
;; 4   w = reward
;; 4   v = visits
;; 1  ta = largest untried tried action
;; 1 who = player who makes decision / player who's reward counts
(struct mcts-node (p ia cs w v ta who) #:mutable)
(define-struct-define define-mcts mcts-node)

(define (mcts-node-w/v mn)
  (define-mcts mn)
  (if (zero? v) -inf.0 (/ w v)))
(define (mcts-node-score mn)
  (define-mcts mn)
  (+ (/ w v)
     ;; XXX No `c'
     (sqrt (* 2.0 (/ (log (mcts-node-v p)) v)))))

(define (real->decimal-string* r)
  (if (nan? r) "NAN" (real->decimal-string r)))

(define (mcts-play! how-many-actions who terminal? score legal? aeval render-st render-a
                    st0 human-id)
  (define (find-next-legal-action st prev)
    (do-until (set! prev (sub1 prev))
              (or (= -1 prev) (legal? st prev)))
    prev)
  (define (make-node p ia st)
    (mcts-node p ia '() 0.0 0.0 (add1 (find-next-legal-action st how-many-actions)) (who st)))
  (define (mcts-decide deadline mn st)
    (define i 0)
    (until (< deadline (current-inexact-milliseconds))
      (define sti st)
      (define mni mn)
      ;; Tree Policy
      (while (and (zero? (mcts-node-ta mni))
                  (cons? (mcts-node-cs mni)))
        (set! mni (argmax mcts-node-score (mcts-node-cs mni)))
        (set! sti (aeval sti (mcts-node-ia mni))))
      ;; Expand
      (unless (zero? (mcts-node-ta mni))
        (define m (sub1 (mcts-node-ta mni)))
        (set-mcts-node-ta! mni (add1 (find-next-legal-action sti m)))
        (set! sti (aeval sti m))
        (define new (make-node mni m sti))
        (set-mcts-node-cs! mni (cons new (mcts-node-cs mni)))
        (set! mni new))
      ;; Default Policy
      (until (terminal? sti)
        (define m #f)
        (do-until (set! m (random how-many-actions))
                  (legal? sti m))
        (set! sti (aeval sti m)))
      (define fsc (score sti))
      ;; Backup
      (while mni
        (set-mcts-node-w!
         mni
         (+ (vector-ref fsc (mcts-node-who mni))
            (mcts-node-w mni)))
        (set-mcts-node-v! mni (add1 (mcts-node-v mni)))
        (set! mni (mcts-node-p mni)))
      ;; Count
      (set! i (add1 i)))
    (eprintf "Took ~a steps\n" i)
    (mcts-node-ia
     (argmax #;mcts-node-w ;; Max Child
             #;mcts-node-v ;; Robust Child
             mcts-node-w/v ;; Average reward
             (mcts-node-cs mn))))
  (define (mcts-choose p ia st)
    (and p
         (for/or ([c (in-list (mcts-node-cs p))])
           (and (equal? ia (mcts-node-ia c))
                (set-mcts-node-p! c #f)
                c))))

  (let/ec esc
    (let loop ([st st0] [mgt #f])
      (define gt (or mgt (make-node #f #f st)))
      (printf "Expected computer value: ~a\n"
              (real->decimal-string*
               (/ (mcts-node-w gt) (mcts-node-v gt))))
      (draw-here (render-st st))
      (define a
        (cond
          [(terminal? st)
           (printf "Score is ~a\n" (score st))
           (esc)]
          [(= human-id (who st))
           (define k->val
             (for/hasheq ([o (in-range how-many-actions)]
                          #:when (legal? st o))
               (define k (action-key (render-a st o)))
               (values k o)))
           ;; XXX Add a ? command
           (define k
             (let read-loop ()
               (printf "> ") (flush-output)
               (define k (read-char))
               (cond
                 [(hash-has-key? k->val k) k]
                 [else (read-loop)])))
           (hash-ref k->val k)]
          [else
           (mcts-decide
            ;; XXX Base on opponent's time
            (+ (current-inexact-milliseconds) 100)
            gt st)]))
      (define stp (aeval st a))
      (loop stp (mcts-choose gt a stp)))))
