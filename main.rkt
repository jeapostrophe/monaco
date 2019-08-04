#lang racket/base
(require raart
         racket/vector
         data/heap
         struct-define
         "util.rkt")
(provide mcts-play!
         (struct-out action))

(struct action (key desc val))

(define (show-mem)
  (printf "~a MB\n" (real->decimal-string (/ (current-memory-use) (* 1024 1024)))))

(struct mcts-node (p ia st t? q n cs) #:mutable)
(define (make-mcts-node terminal? p ia st)
  (mcts-node p ia st (terminal? st) 0.0 0.0 #f))

(define-struct-define define-mcts mcts-node)
(define K (sqrt 2))
(define (ln z) (log z 2))

(define (mcts-average-q mn)
  (define-mcts mn)
  (if (zero? n) -inf.0 (/ q n)))
(define (mcts-score mn)
  (define-mcts mn)
  (define pn (mcts-node-n p))
  (cond
    [(or (zero? pn) (zero? n)) +inf.0]
    [t? -inf.0]
    [else (+ (/ q n) (* K (sqrt (/ (ln pn) (ln n)))))]))
(define (mcts<=? x y)
  (>= (mcts-score x) (mcts-score y)))

(define (mcts-choose aeval mn a)
  (define-mcts mn)
  (and cs
       (for/or ([c (in-heap/consume! cs)])
         (and (equal? a (mcts-node-ia c))
              (set-mcts-node-p! c #f)
              c))))

(define (heap-min! h)
  (begin0 (heap-min h)
    (heap-remove-min! h)))

(define (random-list-ref l)
  (list-ref l (random (length l))))

(define (mcts-simulate terminal? score legal aeval st)
  (cond
    [(terminal? st)
     (vector-ref (score st) 1)]
    [else
     (mcts-simulate terminal? score legal aeval
                    (aeval st (random-list-ref (legal st))))]))

(define (mcts-step terminal? score legal aeval render-st
                   mn simulate?)
  (define-mcts mn)
  (cond
    ;; Has children, so select
    [(and cs (not t?))
     (define c (heap-min! cs))
     (define Δq (mcts-step terminal? score legal aeval render-st
                           c simulate?))
     (set! q (+ q Δq))
     (set! n (+ n 1))
     (heap-add! cs c)
     Δq]
    ;; We are the leaf to simulate
    [(or t? simulate?)
     (mcts-simulate terminal? score legal aeval st)]
    ;; No children, not simulation, so expand
    [else
     (set! cs (make-heap mcts<=?))
     (for ([a (in-list (legal st))])
       ;; XXX Maybe delay aeval until later
       (heap-add! cs (make-mcts-node terminal? mn a (aeval st a))))
     (mcts-step terminal? score legal aeval render-st
                mn #t)]))

(define (mcts-decide terminal? score legal aeval render-st
                     deadline mn k)
  (let loop ([i 0])
    (cond
      [(< deadline (current-inexact-milliseconds))
       (eprintf "Took ~a steps\n" i)
       mn]
      [else
       (mcts-step terminal? score legal aeval render-st mn #f)
       (loop (add1 i))]))
  (define mnp (heap-min (mcts-node-cs mn)))
  (define stp (mcts-node-st mnp))
  (k stp mnp))

(define (mcts-play! who terminal? score legal aeval render-st render-a
                    st0 human-id)
  (let loop ([st st0] [gt (make-mcts-node terminal? #f #f st0)])
    (draw-here (render-st st))
    (cond
      [(terminal? st)
       (printf "Score is ~a\n" (score st))]
      [(= human-id (who st))
       (define opts (legal st))
       (define k->val
         (for/hasheq ([o (in-list opts)])
           (values (action-key (render-a o)) o)))
       (define k
         (let read-loop ()
           (printf "> ")
           (define k (read-char))
           (if (hash-has-key? k->val k) k
               (read-loop))))
       (define a (hash-ref k->val k))
       (define stp (aeval st a))
       (loop stp
             (or (mcts-choose aeval gt a)
                 (make-mcts-node terminal? #f #f stp)))]
      [else
       (mcts-decide terminal? score legal aeval render-st
                    ;; XXX Base on opponent's time
                    (+ (current-inexact-milliseconds) 17)
                    gt loop)])))

;; XXX Use adqc to write the functions?

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

;; XXX Caylus: 35 spots to choose and 26 different buildings

;; XXX Hansa Teutonica (see other file) there are billions of options.

;; XXX Tigris & Euphrates: 11x16 spots, 4 different kinds of tiles,
;; plus 2 catastrophe tiles, 4 leaders with ability to remove one
;; also. Consider this as 11x16+1 spots with 9 things to place (tiles,
;; catastrophe, and leaders.) Plus, you get two actions, so that's 2.5
;; million possibilities.

;; XXX HT + T&E --- Abstract the actions into meta-actions like (HT)
;; "Open a new route", "Finish a route", and so on or (T&E) "Cause a
;; revolt", "Cause a war", "Establish kingdom", or so on. Then select
;; among possible ways to do that thing. I found some papers about
;; these ideas, but haven't read them yet:
;; https://project.dke.maastrichtuniversity.nl/games/files/msc/Roelofs_thesis.pdf
;; and https://arxiv.org/pdf/1805.09613.pdf

;; XXX HT (based on above): Another way is to think about a
;; "strategic" and a "tactical" AI. The strategic one chooses which
;; cities/offices it will target and the tactical one takes that goal
;; and determines the best way to get it. The hard part is
;; incorporating reward into things... what does the money-bag give
;; you? It really gives you more tactical options, but how do you
;; quantify that? It is clear that the key and medal give you points,
;; the liber sophia gives you a merchant, privilege gives you options,
;; but the money bag just gives you more moveable pieces. Perhaps I could quantify things by keeping track of how many turns it would take to claim a route/city. So, in the beginning you have 2 units per turn, but if you get displaced it is as if you have 3 units, because can put one and then move.

;; Roelofs ---
;; https://project.dke.maastrichtuniversity.nl/games/files/msc/Roelofs_thesis.pdf
;; --- has a big idea on page 24 and 26. There are multiple dimensions
;; of decisions (move, complete, and so on) and you first select a
;; dimension, then an action from that, and so on until you have come
;; to a complete action. Because of the asymmetric exploration of the
;; tree, you'll ignore unpromising sub-actions/dimension orderings. This requires "partial move completion" to take an incomplete action into a complete one. (In the experiment, the dimensions are the nodes to move soldiers from.)
