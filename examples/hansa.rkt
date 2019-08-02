#lang racket/base
(require racket/list)

(define desk
  `([keys
     1
     (t 2)
     (t 2)
     (t 3)
     (t 4)]
    [privilegium
     w
     (t o)
     (t p)
     (t b)
     4-points]
    [actiones
     2
     (t 3)
     (t 3)
     (t 4)
     (t 4)
     (t 5)
     4-points]
    [liber-sophiae
     2
     (m 3)
     (m 4)
     (m 5)
     4-points]
    [money-bag
     3
     (t 5)
     (t 7)
     (t C)
     4-points]))

(define board
  `((#:special-cities
     [A liber-sophiae]
     [C privilegium]
     [E money-bag]
     [W medals]
     [Y actiones]
     [AA keys])
    (#:special-routes
     [L P bonus-points]
     [G H taverna]
     [J K taverna]
     [T U taverna])
    (#:offices
     [A (t w 1) (m o)]
     [B (m w) (t p)]
     [C (m w)]
     [D (t w) (t o) (t b)]
     [E (t w 1) (t p)]
     [F (m o) (t b)]
     [G (t w) (t o) (t b)]
     [H (m w) (t p)]
     [I (t w) (t p)]
     [J (m o) (t b)]
     [K (t w) (t p) (m b)]
     [L (t w) (m w) (t o) (t p)]
     [M (m w) (t o)]
     [N (t w) (t o) (t p) (t b)]
     [O (t o)]
     [P (t w) (m w) (t o) (t p)]
     [Q (t w)]
     [R (m w) (t o) (t p)]
     [S (t w) (m b)]
     [T (t w) (t b)]
     [U (t w) (t b)]
     [V (m w) (t o)]
     [W (t w 1) (t p)]
     [X (t o 1) (t p)]
     [Y (t w) (m w) (t p)]
     [Z (m o) (m p)]
     [AA (t w 1) (t o)])
    (#:routes
     [A B 3]
     [B C 3]
     [B G 4]
     [C D 3]
     [D E 3]
     [D H 4]
     [D J 4]
     [F G 1]
     [F L 3]
     [G H 3]
     [H N 3]
     [H I 3]
     [I J 3]
     [I N 2]
     [J K 3]
     [K P 3]
     [L M 3]
     [L Q 3]
     [M N 3]
     [N S 3]
     [N O 4]
     [O P 4]
     [P V 3]
     [Q R 2]
     [R S 3]
     [S T 3]
     [S X 3]
     [T U 2]
     [U V 2]
     [U Z 4]
     [W X 4]
     [X Y 3]
     [Y Z 3]
     [Z AA 4])))

(module+ main
  (define total-cities
    (length (rest (list-ref board 2))))
  'total-cities total-cities
  (define total-offices
    (for/sum ([c (in-list (rest (list-ref board 2)))])
    (length (rest c))))
  'total-offices total-offices
  (define total-routes
    (length (rest (list-ref board 3))))
  'total-routes total-routes
  (define total-houses
    (for/sum ([r (in-list (rest (list-ref board 3)))])
      (third r)))
  'total-houses total-houses)

;; Per-player state
;; - Status of desk                             =  11 bits
;; - Supply/stock count (traders)   [0, 26] x 2 =  10 bits
;; - Supply/stock count (merchants) [0,  4] x 2 =   6 bits
;; - Used/free bonuses (ECO)        [0,  5] x 2 =   6 bits
;; - Used/free bonuses (SCO)        [0,  2] x 2 =   4 bits
;; - Used/free bonuses (3xA)        [0,  2] x 2 =   4 bits
;; - Used/free bonuses (4xA)        [0,  2] x 2 =   4 bits
;; - Used/free bonuses ( IS)        [0,  3] x 2 =   4 bits
;; - Used/free bonuses (R3T)        [0,  2] x 2 =   4 bits
;; - Held offices                               =  62 bits
;; - Held houses (traders)                      = 104 bits
;; - Held houses (merchants)                    = 104 bits
;; - Medals                                     =   4 bits
;; - Current score                  [0, 20]     =   5 bits
;; Total: 332 bits

;; Game state
;; - Bonus locations (route)    = 34 bits
;; - Bonus supply (ECO)  [0, 5] =  3 bits
;; - Bonus supply (SCO)  [0, 2] =  2 bits
;; - Bonus supply (3xA)  [0, 2] =  2 bits
;; - Bonus supply (4xA)  [0, 2] =  2 bits
;; - Bonus supply ( IS)  [0, 3] =  2 bits
;; - Bonus supply (R3T)  [0, 2] =  2 bits
;; Total: 47 bits

;; Control Flow
;; - Active player       [0, 4] =  3 bits
;; - Active player actions remaining 
;; - Displaced player
;; - Displaced player merchant count
;; - Displaced player trader count

;; Total state
;; - Players   332 x 5 = 1660
;; - Game              =   47
;; - Control Flow      =  ???
;; Total: 1707 = 214 bytes

;; 16 MB for 80k states

;; Alternate state plan

;; Per-player state
;; - 26 x Location of trader
;; --- In supply
;; --- In stock
;; --- Desk [15 places]
;; --- House [104 places] {replace with route? 34}
;; --- Office [46 places]
;; --- Extra office [27 places]
;; --- Total: 194 places =~ 8 bits {7 bits}
;; -  4 x Location of merchant
;; --- In supply
;; --- In stock
;; --- Desk [3 places]
;; --- House [104 places] {replace with route? 34}
;; --- Office [16 places]
;; --- Medals [4 places]
;; --- Extra office [27 places] {remove?}
;; --- Total: 156 places =~ 8 bits {6 bits}
;; - Score [0, 20] = 5 bits
;; - Total: 245 bits {211 bits}

;; Game state
;; - 16 x Location of bonus marker
;; --- In stock
;; --- On route [34 places]
;; --- Unused player [5 places]
;; --- Used player [5 places]
;; --- At city [27 places, only ECO]
;; --- Total: 72 places =~ 7 bits
;; - Total: 112 bits

;; Total state
;; - Players   245 x 5 = 1225
;; - Game              =  112
;; - Control Flow      =  ???
;; Total: 1337 = 168 bytes

;; 13 MB for 80k states

(module+ main
  'total-actions
  (+
   ;; Actions:
   ;; - (Take-Income t m) for t in [0,7] and m in [0, 4]
   ;;   t + m <= money-bag and t <= stock(t) and m <= stock(m)
   (* 8 5)
   ;; - (Take-Income-All)
   ;;   money-bag == C
   1
   ;; - (Place Which Route)
   ;;   supply is enough and route has empty spot
   ;;   used for displaced and displacing
   (* 2 total-routes)
   ;; - (Move-Piece Which-Piece Route)
   (* 30 total-routes)
   ;; - (Swap-Piece Which-Piece Which-Piece) ;; If one house is empty, then swap is empty
   ;;   From has me, To has me or empty
   (* 30 30)
   ;; - (Claim-Route Route Claim-Choice)
   ;;   Route is full of me
   (* total-routes
      ;; Claim-Choice:
      (+
       ;; - (Establish-Office)
       ;;   Office is available, merchant/trader in route, privilege is high enough
       1
       ;; - (Bonus:Establish-Extra-Office)
       ;;   Have token, trader in route
       1
       ;; - (Improve-Ability)
       ;;   City is ability and skill is not completed
       1
       ;; - (Claim-Medal Medal)
       ;;   City is medal, merchant in route, privilege is high enough, medal is free
       4
       ))
   ;; - (Bonus:Swap-Office City Office-Idx Office-Idx)
   ;;   Offices in same city, have token
   (* total-cities 4 4)
   ;; - (Bonus:+3-Actions)
   ;;   Have token
   1
   ;; - (Bonus:+4-Actions)
   ;;   Have token
   1
   ;; - (Bonus:Improve-Skill Skill)
   ;;   Have token, skill not completed
   5
   ;; - (Bonus:Remove-Three Route Route Route)
   ;;   House have traders/merchants
   #;(* (add1 total-routes) (add1 total-routes) (add1 total-routes))
   ))

;; XXX The action space is simply enormous for Hansa. Even with a lot
;; of reductions, there are still about ~3000 plies per action and a
;; player gets 2 to 5 actions, which means 9 million to 2.43 x 10^17
;; possible turns. That's 38 billion gigabytes of states for one move.

;; XXX I could make a set of goal-driven AIs.
;; - Medals --- Tries to get the medals
;; - Bonus --- Tries to end the game by getting bonus plates
;; - Network --- Tries to connect the red cities and get keys
;; - Blocker --- Gets the money bag and tries to block everyone else and get routes
