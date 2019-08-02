#lang racket/base
(require (for-syntax racket/base
                     syntax/parse)
         racket/match
         syntax/parse/define)

;; Library

(define-syntax (condlet stx)
  (syntax-parse stx
    [(_)
     #'(void)]
    [(_ [#:cond question answer ...+] . more)
     #'(if question (let () answer ...) (condlet . more))]
    [(_ code)
     #'code]
    [(_ code . more)
     #'(let () code (condlet . more))]))

(define (bit m)
  (arithmetic-shift 1 m))
(define (bitwise-bit-set n m)
  (bitwise-ior n (bit m)))
(define (bitwise-bit-flip n m)
  (bitwise-xor n (bit m)))

(define-simple-macro
  (define-functor (name:id input0 inputN:id ...)
    (define output:id e:expr) ...)
  #:with (this-output ...) (generate-temporaries #'(output ...))
  (define-simple-macro (name input0 inputN ...)
    (~@ #:with this-output (datum->syntax #'input0 'output))
    ...
    (begin
      (define output e) ...
      (define this-output output) ...)))

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
  (+ c (* r 3)))
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

(require raart
         racket/format)
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
    [(winning? Xs) (vector 0 1)]
    [(winning? Os) (vector 1 0)]
    [else (vector 0 0)]))

(define all-actions
  (for*/list ([r (in-range rows)]
              [c (in-range cols)])
    (cons r c)))

(define (ttt-render-a a)
  (match-define (cons r c) a)
  (format "Select ~a,~a" (add1 r) (add1 c)))

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

;; Engine

(define (show-mem)
  (printf "~a MB\n" (real->decimal-string (/ (current-memory-use) (* 1024 1024)))))

(define (play! who terminal? score legal aeval render-st render-a st0)
  (let loop ([st st0])
    (draw-here (render-st st))
    (cond
      [(terminal? st)
       (printf "Score is ~a\n" (score st))]
      [else
       (define opts (legal st))
       (for ([o (in-list opts)]
             [i (in-naturals)])
         (printf "~a. ~a\n" i (render-a o)))
       (printf "> ")
       (define stp (aeval st (list-ref opts (read))))
       (loop stp)])))

(play! ttt-who ttt-terminal? ttt-score
       ttt-legal ttt-aeval ttt-render-st ttt-render-a
       ttt-init)

;; XXX Use adqc to write the functions?

;; XXX MCTS node, v, data:
;; - s(v) --- Game State
;; - a(v) --- Inducing action
;; - Q(v) --- total reward
;; - N(v) --- number of payouts

;; XXX The discussion of MCTS in https://arxiv.org/pdf/1805.09218.pdf
;; says that there is a fixed set of actions. But sometimes, actions
;; are illegal (like in Go, you can't put your stone on top of
;; another.) Would you represent this by saying that you lose if you
;; do an illegal move? Or do you just vary the number of actions on
;; each node? Perhaps the MCTS-T algorithm is great for this
;; situation, because it is well-designed for "ending early"
;; situations. [This paper has a link to a git repo that implements
;; it.]

;; XXX Exploiting graph properties of game trees, by Plaat et al.,
;; 1996 talks about "transpositions" (the same state occurring in
;; different paths in the tree.)

;; XXX Any space paper says
;; - Left-Child Right-Sibling representation for the search tree
;; - This seems to imply that we need to generate all of the children
;;   at the same time.

;; XXX MCTS steps
;; - Selection: via tree policy
;; --- - UCB1: choose child v' maximizing Q(v')/N(v') + K \sqrt{ln N(v) / N(v')}
;; --- - K is often \sqrt(2) or \sqrt(2)/2
;; --- - This could be tracked easily with a priority queue per node.
;; --- - If v' is not in the tree, then it is "expanded"
;; - Simulation:
;; --- - Randomly playout of the game
;; - Back propagation:
;; --- - Update Q and N

;; XXX Single-Observer Information-Set MCTS

;; - The state is unknown, so we sample a concrete state from what is
;;   possible (based on the actions we've observed in the past)

;; - Still update the same tree

;; XXX Multi-Observer Information-Set MCTS

;; - One tree for each observer

;; - Choosing a move for player-i uses the i-tree

;; - Player i's tree has branches for actions but player j ≠ i's tree
;;   has merged the branches when it can't tell the difference

;; XXX AnyTime: MCTS is generic in the number of iterations it takes
;; before returning an answer.
;;
;; - Run MCTS when the user is idle and immediately make move when
;; they do. (Disadvantage: doesn't take advantage of what the user
;; did.)
;;
;; - Run MCTS for as long as the player took making their
;; move. (Disadvantage: Doesn't make use of idle time.)

;; XXX AnySpace: Node recycling

;; - http://mcts.ai/edpowley/papers/powley_aiide17.pdf

;; - Predetermine how many nodes you want to store

;; - Draw from the free list when you want one

;; - If the free list is empty, remove the one for which the Q/N
;;   values were least-recently read.

;; - It must be read, because if not, then you may kill a sibling that
;;   is useful.

;; - You maintain this by adjusting the position of elements on the
;;   way down the tree

;; - This means that the all of the siblings of a node need to be ;;
;;   touched on the way down, which is inefficient and doesn't make
;;   use of the heap effectively. Perhaps you could update them when
;;   readjusting the heap position of the sibling, but that would
;;   either be inefficient (look through all) or not exhaustive (only
;;   look at the ones on the path up the heap) and backwards (because
;;   the top of the queue would be the ones at the top of the tree,
;;   rather than the bottom)

;; - Paper says: We maintain a queue Θ of nodes. Non-leaf node are
;;   removed from Θ when visited during playout, and push to the back
;;   of Θ when they are updated. Leaf nodes are never present in
;;   Θ. Thus the node at the front of Θ is the least recently updated
;;   non-leaf node.

;; - If you remove a child, how do you get it back if you need it
;;   again? Generate all choices from the parent and add the ones that
;;   aren't there? How would you know? The paper doesn't say anything
;;   about it. One strategy would be to track only parents and delete
;;   ALL of the children.

;; XXX AlphaZero
;; - How is the neural network used?
;; --- - It is used to rank the children.
;; --- - It is used during simulation to select the move (like a "heavy" playout)
;; - Typical number of legal moves is used to scale exploration (the K constant)
;; - Games are terminated early after a certain number of steps and scored.
;; - Network input: N x N x (MT + L)
;; --- - N is the size of the board
;; --- - M is the number of planes (1 for each piece type)
;; --- - T is the number of time steps into the past
;; --- - L is game-specific variables, like legality of castling
;; - Network output "policy" is basically what piece to "pick up" and where to put it
;; --- - Probabilities for illegal moves are 0'd and then the rest re-normalized
;; - MCTS # of iterations was 800

;; XXX Ultimate Tic-Tac-Toe: A 3x3 grid of boards where if you play in
;; (2,1) then the opponent must use that board next.

;; XXX Connect-4: A 7-column x 6-row grid where you try to get 4 in a
;; row. Solved where first player wins if they play in the middle
;; column.

;; XXX Isolation: A 6x8 grid with two pawns where each turn you move
;; your pawn and then remove a square from the board. You lose if you
;; cannot move.
