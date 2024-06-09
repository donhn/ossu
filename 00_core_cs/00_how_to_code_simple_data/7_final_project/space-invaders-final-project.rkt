;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-final-project) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders

;; =============================================================
;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 4)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 4)
(define TANK-SPEED 6)
(define MISSILE-SPEED 15)


(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (rectangle WIDTH HEIGHT "outline" "black"))

(define INVADER
  (overlay/xy (ellipse 10 15 "outline" "blue")              ;cockpit cover
              -5 6
              (ellipse 20 10 "solid"   "blue")))            ;saucer

(define TANK
  (overlay/xy (overlay (ellipse 28 8 "solid" "black")       ;tread center
                       (ellipse 30 10 "solid" "green"))     ;tread outline
              5 -14
              (above (rectangle 5 10 "solid" "black")       ;gun
                     (rectangle 20 10 "solid" "black"))))   ;main body

(define TANK-HEIGHT/2 (/ (image-height TANK) 2))

(define MISSILE (ellipse 5 15 "solid" "red"))

(define COLLISION-DIST 15)  ; pixels

;; =============================================================
;; Data Definitions:

(define-struct game (invader-struct missiles tank))
;; Game is (make-game InvaderStruct (ListofMissile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-i (game-invader-struct s))
       (fn-for-lom (game-missiles s))
       (fn-for-tank (game-tank s))))

(define-struct tank (x dir))
;; Tank is (make-tank Number Integer[-1, 1])
;; interp. the tank location is x, HEIGHT - TANK-HEIGHT/2 in screen coordinates
;;         the tank moves TANK-SPEED pixels per clock tick left if dir -1, right if dir 1

(define T0 (make-tank (/ WIDTH 2) 1))   ;center going right
(define T1 (make-tank 50 1))            ;going right
(define T2 (make-tank 50 -1))           ;going left

#;
(define (fn-for-tank t)
  (... (tank-x t) (tank-dir t)))

(define-struct invader (x y dx))
;; Invader is (make-invader Number Number Number)
;; interp. the invader is at (x, y) in screen coordinates
;;         the invader along x by dx pixels per clock tick

(define I1 (make-invader 150 100 12))                          ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))                      ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10))                ;> landed, moving right
(define I4 (make-invader (+ WIDTH 1) 100 INVADER-X-SPEED))     ; Beyond right side of screen
(define I5 (make-invader (- 0 1) 100 (* -1 INVADER-X-SPEED)))  ; Beyond left side of screen


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))

;; ListOfInvader is one of:
;;  - empty
;;  - (cons Invader ListOfInvader)
;; interp. a list of Invaders

(define LOI-1 empty)
(define LOI-2 (list I1))
(define LOI-3 (list I1 I2 I3))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]                    
        [else (... (fn-for-invader (first loi)) 
                   (fn-for-loi (rest loi)))]))  


(define-struct invader-struct (countdown loi))
;; InvaderStruct is (make-game Number[0,INVADE-RATE] ListOfInvader)
;; interp. The current invade countdown and list of invaders.

#;
(define (fn-for-invaders i)
  (... (fn-for-countdown (invaders-countdown i))
       (fn-for-loi (invaders-loi i))))

(define IS-1 (make-invader-struct 0 empty))
(define IS-2 (make-invader-struct INVADE-RATE LOI-2))


;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - reference: (first loi) is Invader
;;  - self-reference: (rest loi) is ListOfInvader

(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                               ;not hit I1
(define M2 (make-missile (invader-x I1) (invader-y I1)))         ;exactly hit I1
(define M3 (make-missile (+ COLLISION-DIST (invader-x I1))       ;edge hit I1
                         (invader-y I1)))        
(define M4 (make-missile 150 -1))                                ;off screen

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissile is one of:
;;  - empty
;;  - (cons Missile ListOfMissile)
;; interp. a list of Missiles

(define LOM-1 empty)
(define LOM-2 (list M1))
(define LOM-3 (list M1 M2 M3))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]                    
        [else (... (fn-for-missile (first lom)) 
                   (fn-for-lom (rest lom)))]))

;; Template rules used:
;;  - one of: 2 cases
;;  - atomic distinct: empty
;;  - reference: (first loi) is Missile
;;  - self-reference: (rest loi) is ListOfMissile

(define G0 (make-game IS-1 empty T0))
(define G1 (make-game IS-1 empty T1))
(define G2 (make-game IS-2 (list M1) T1))
(define G3 (make-game IS-2 (list M1 M2) T1))


;; =============================================================
;; Functions:

;; Game -> Game
;; Start the game with: (main G0)
(define (main s)
  (big-bang s                          ; Game
    (on-tick   process-game-state)     ; Game -> Game
    (to-draw   render)                 ; Game -> Image
    (stop-when game-over?)             ; Game -> Boolean
    (on-key    key-handler)))          ; Game KeyEvent -> Game

;; Game -> Game
;; Called each tick. Updates the game state by:
;;   1. Checking collisions between invaders and missiles.
;;   2. Updating invaders, missiles and tank states.

;(define (process-game-state s) s) ;stub

(define (process-game-state s)
  (advance-game-state (process-collisions s)))

;; Game -> Game
;; Updates invaders, missiles and tank states.
(define (advance-game-state s)
  (make-game (advance-invaders (game-invader-struct s))
             (advance-missiles (game-missiles s))
             (advance-tank (game-tank s))))

;; Game -> Game
;; Process collisions between and invaders and missiles.

;(define (process-collisions s) s) ;stub

(define (process-collisions s)
  (make-game (make-invader-struct
              (invader-struct-countdown (game-invader-struct s))
              (process-collisions-invaders
               (invader-struct-loi (game-invader-struct s))
               (game-missiles s)))
             (process-collisions-missiles
              (game-missiles s)
              (invader-struct-loi (game-invader-struct s)))
             (game-tank s)))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Removes any invader that collide with a missile.

;(define (process-collisions-invaders loi lom) loi) ;stub

(define (process-collisions-invaders loi lom)
  (cond [(empty? loi) empty]                    
        [else (remove-colliding-invader lom
                                        (first loi)
                                        (process-collisions-invaders (rest loi) lom))]))

;; ListOfMissiles Invader ListOfInvader -> ListOfInvader
;; Checks if given invader is colliding with any missile. If it is not colliding, add it to
;; the list of invaders.
(check-expect (remove-colliding-invader empty                         ; No missiles and empty list of invaders, add to list.
                                        (make-invader 150 150 10)
                                        empty)         
              (list (make-invader 150 150 10)))
(check-expect (remove-colliding-invader (list (make-missile 10 20))   ; One missile does not collide, and empty list of invaders, add to list.
                                        (make-invader 150 150 10)
                                        empty)
              (list (make-invader 150 150 10)))
(check-expect (remove-colliding-invader (list (make-missile 150 150)) ; One missile collides, and empty list of invaders, do not add to list. Return empty list.
                                        (make-invader 150 150 10)
                                        empty)
              empty)
(check-expect (remove-colliding-invader (list (make-missile 30 40))   ; One missile does not collide, add to non-empty list of invaders.
                                        (make-invader 150 150 10)
                                        (list I1)) 
              (list (make-invader 150 150 10) I1))
(check-expect (remove-colliding-invader (list (make-missile 150 150)) ; One missile collides, do not add to non-empty list of invaders.
                                        (make-invader 150 150 10)
                                        (list I1)) 
              (list I1))
(check-expect (remove-colliding-invader (list (make-missile 40 50)    ; Two missiles don't collide, add to non-empty list of invaders.
                                              (make-missile 20 30)) 
                                        (make-invader 150 150 10)
                                        (list I1 I2))
              (list (make-invader 150 150 10) I1 I2))
(check-expect (remove-colliding-invader (list (make-missile 40 50)    ; Two missiles, one collides, don't add to non-empty list of invaders.
                                              (make-missile 150 150)) 
                                        (make-invader 150 150 10)
                                        (list I1 I2))
              (list I1 I2))

;(define (remove-colliding-invader lom i loi) loi)
(define (remove-colliding-invader lom i loi)
  (cond [(empty? lom) (cons i loi)]
        [else (if (invader-collides? lom i)
                  (cond [(empty? loi) empty]
                        [else (cons (first loi) (rest loi))])
                  (cons i loi))]))


;; ListOfMissiles Invader -> Boolean
;; Returns true if the given invader is colliding with a missile.
(check-expect (invader-collides? empty (make-invader 30 100 10)) false)         ;No missiles                                   
(check-expect (invader-collides? (list (make-missile 100 100))                  ;One missile, not colliding
                                 (make-invader 90 30 10)) false)
(check-expect (invader-collides? (list (make-missile 150 150))                  ;One missile, exactly within colliding distance
                                 (make-invader (+ 150 COLLISION-DIST) 150 10))
              true)
(check-expect (invader-collides? (list (make-missile 150 150))                  ;One missile, one pixel beyond colliding distance
                                 (make-invader (+ 151 COLLISION-DIST) 150 10))
              false)
(check-expect (invader-collides? (list (make-missile 150 150))                  ;One missle, exactly on top of invader
                                 (make-invader 150 150 10))
              true)
(check-expect (invader-collides? (list (make-missile 100 100)                   ;Two missiles, both not colliding
                                       (make-missile 150 150))
                                 (make-invader 150 100 10))
              false)
(check-expect (invader-collides? (list (make-missile 10 10)                     ;Three missiles, one colliding
                                       (make-missile 50  50)
                                       (make-missile 150 150))
                                 (make-invader 150 150 10))
              true)

;(define (invader-collides? lom i) false)

(define (invader-collides? lom i)
  (cond [(empty? lom) false]                    
        [else (if (<= (dist (invader-x i) (invader-y i)
                            (missile-x (first lom)) (missile-y (first lom)))
                      COLLISION-DIST)
                  true
                  (invader-collides? (rest lom) i))]))

;; ListOfMissile ListOfInvader -> ListOfMissile
;; Removes any missiles that collide with an invader from given list of missiles.
;(define (process-collisions-missiles lom loi) lom) ;stub

(define (process-collisions-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (remove-colliding-missile loi
                                        (first lom)
                                        (process-collisions-missiles (rest lom) loi))]))

;; ListOfInvader Missile ListOfMissile -> ListOfMissile
;; Checks if missile is colliding with any invader and removes it from ListOfMissile.
(check-expect (remove-colliding-missile empty M1 empty) (list M1))        ; No invaders, empty list of missile, add to list
(check-expect (remove-colliding-missile empty M1 (list M2)) (list M1 M2)) ; No invaders, non-empty list of missile, add to list
(check-expect (remove-colliding-missile (list (make-invader 123 321 10))  ; One invader, not colliding, empty list of missile, add to list
                                        (make-missile 30 40)
                                        empty)
              (list (make-missile 30 40)))
(check-expect (remove-colliding-missile (list (make-invader 123 321 10))  ; One invader, colliding, empty list of missile, do not add.
                                        (make-missile 123 321)
                                        empty)
              empty)
(check-expect (remove-colliding-missile (list (make-invader 123 321 10)   ; Two invader, one colliding, empty list of missile, do not add.
                                              (make-invader 456 789 10)) 
                                        (make-missile 123 321)
                                        empty)
              empty)
(check-expect (remove-colliding-missile (list (make-invader 123 321 10)   ; Two invader, none colliding, empty list of missile, add to list.
                                              (make-invader 456 789 10)) 
                                        (make-missile 50 60)
                                        empty)
              (list (make-missile 50 60)))
(check-expect (remove-colliding-missile (list (make-invader 123 321 10)   ; Two invader, none colliding, non-empty list of missile, add to list.
                                              (make-invader 456 789 10)) 
                                        (make-missile 50 60)
                                        (list M1 M2))
              (list (make-missile 50 60) M1 M2))


;(define (remove-colliding-missile loi m lom) lom); stub

(define (remove-colliding-missile loi m lom)
  (cond [(empty? loi) (cons m lom)]
        [else (if (missile-collides? loi m)
                  (cond [(empty? lom) empty]
                        [else (cons (first lom) (rest lom))])
                  (cons m lom))]))

;; ListOfInvader Missile -> Boolean
;; Returns true if the missile is colliding with an invader.
(check-expect (missile-collides? empty M1) false)                 ; No invaders, no collision
(check-expect (missile-collides? (list (make-invader 10 50 10))   ; One invaders, no collision
                                 (make-missile 10 20))
              false)
(check-expect (missile-collides? (list (make-invader 10 20 10))   ; One invaders, collision
                                 (make-missile 10 20))
              true)
(check-expect (missile-collides? (list (make-invader 14 25 10)    ; Two invaders, no collision
                                       (make-invader 30 40 10))
                                 (make-missile 10 20))
              true)
(check-expect (missile-collides? (list (make-invader 10 20 10)    ; Two invaders, collision
                                       (make-invader 30 40 10))
                                 (make-missile 10 20))
              true)

;(define (missile-collides? loi m) false)
(define (missile-collides? loi m)
  (cond [(empty? loi) false]                    
        [else (if (<= (dist (missile-x m) (missile-y m)
                            (invader-x (first loi)) (invader-y (first loi)))
                      COLLISION-DIST)
                  true
                  (missile-collides? (rest loi) m))]))

;; Number Number Number Number -> Number
;; Returns the distance between two points.
;; Arguments are in order:
;;   - x1
;;   - y1
;;   - x2
;;   - y2

(check-within (dist 0 0 1 1) 1.4142136 0.1)
(check-within (dist 1 2 5 6) 5.6568542 0.1)

;(define (dist x1 y1 x2 y2) 0) ;stub
(define (dist x1 y1 x2 y2)
  (sqrt (+ (sqr (- x1 x2)) (sqr (- y1 y2)))))


;; Invaders -> Invaders
;; Update invade countdown and spawn a new invader when countdown reaches 0.
;; Update invader positions.
(check-expect (advance-invaders (make-invader-struct INVADE-RATE empty))  ; Decrease countdown timer by one. No new invader.
              (make-invader-struct (- INVADE-RATE 1) empty))
(check-random (advance-invaders (make-invader-struct 0 empty))            ; Countdown timer reached 0. Create new invader.
              (make-invader-struct (random INVADE-RATE)
                                   (add-new-invader empty)))

;(define (advance-invaders i) i) ;stub

(define (advance-invaders i)
  (make-invader-struct (process-countdown (invader-struct-countdown i))
                       (if (= 0 (invader-struct-countdown i))
                           (add-new-invader (invader-struct-loi i))
                           (process-loi (invader-struct-loi i)))))

;; ListOfInvader -> ListOfInvader
;; Creates a new invader at the top of the screen at a random x location
;; and moving in a random direction.

;(define (add-new-invader loi) loi)

(define (add-new-invader loi)
  (cons (make-invader (random WIDTH)
                      0
                      (random-invader-dir INVADER-X-SPEED))
        (process-loi loi)))

;; Number -> Number
;; Given the invader x speed, randomly inverts the sign.

;(define (random-invader-dir dx) dx)

(define (random-invader-dir dx)
  (cond [(> (random 10) 5) dx]
        [else (* -1 dx)]))

;; Number[0,INVADE-RATE] -> Number[0,INVADE-RATE]
;; Decrement invade countdown timer by one tick and randomly set it to a value between [0,INVADE-RATE] when
;; it reaches 0.
(check-expect (process-countdown 1) 0)                            ; Decrement 1 near lower bound
(check-expect (process-countdown INVADE-RATE) (- INVADE-RATE 1))  ; Decrement 1 at upper bound
(check-random (process-countdown 0) (random INVADE-RATE))         ; Reached 0, reset to random value between [0,INVADE-RATE]

(define (process-countdown cd)
  (if (= 0 cd)
      (random INVADE-RATE)
      (- cd 1)))

;; ListofInvader -> ListofInvader
;; Advance all invader positions in the game.
(check-expect (process-loi empty) empty)
(check-expect (process-loi (list I5)) (list (process-invader I5)))
(check-expect (process-loi (list I1 I4 I5)) (list (process-invader I1)
                                                  (process-invader I4)
                                                  (process-invader I5)))

;(define (advance-invaders loi) loi) ;stub

(define (process-loi loi)
  (cond [(empty? loi) empty]                    
        [else (cons (process-invader (first loi)) 
                    (process-loi (rest loi)))]))

;; Invader -> Invader
;; Update given invader's position.
;; If the invader has gone beyond the edge of the screen,
;; change dx direction and bound x to edge.
(check-expect (process-invader I1) (make-invader (+ (invader-x I1) (invader-dx I1))   ; Update position, not colliding with wall
                                                 (+ (invader-y I1) INVADER-Y-SPEED)
                                                 (invader-dx I1)))
(check-expect (process-invader I4) (make-invader WIDTH                                ; At right edge of screen, reverse direction
                                                 (+ (invader-y I4) INVADER-Y-SPEED)
                                                 (* -1 (invader-dx I4))))
(check-expect (process-invader I5) (make-invader 0                                    ; At left edge of screen, reverse direction
                                                 (+ (invader-y I5) INVADER-Y-SPEED)
                                                 (* -1 (invader-dx I5))))


;(define (process-invader i) i) ;stub

(define (process-invader i)
  (make-invader
   (cond [(> (invader-x i) WIDTH) WIDTH]
         [(< (invader-x i) 0) 0]
         [else (+ (invader-x i) (invader-dx i))])
   (+ (invader-y i) INVADER-Y-SPEED)
   (if (or (> (invader-x i) WIDTH)
           (< (invader-x i) 0))
       (* -1 (invader-dx i))
       (invader-dx i))))


;; ListofMissile -> ListofMissile
;; Advance all missile positions.
(check-expect (advance-missiles empty) empty)
(check-expect (advance-missiles (list M1)) (list (process-missile M1)))
(check-expect (advance-missiles (list M1 M2)) (list (process-missile M1)
                                                    (process-missile M2)))
(check-expect (advance-missiles (list M1 M4)) (list (process-missile M1)))

           
;(define (advance-missiles lom) lom) ;stub

(define (advance-missiles lom)
  (cond [(empty? lom) empty]                    
        [else (filter-missiles (process-missile (first lom)) 
                               (advance-missiles (rest lom)))]))


;; Missile ListOfMissile -> ListOfMissile
;; Inserts any missiles that have not gone off the screen.
(check-expect (filter-missiles M1 empty) (list M1))             ; On screen, add to empty list
(check-expect (filter-missiles M4 empty) empty)                 ; Off screen, do not add to empty list.
(check-expect (filter-missiles M2 (list M1)) (list M2 M1))      ; On screen, add to non-empty list
(check-expect (filter-missiles M4 (list M1 M2)) (list M1 M2))   ; Off screen, do not add to non-empty list

;(define (filter-missiles m lom) lom) ;stub

(define (filter-missiles m lom)
  (cond [(empty? lom)
         (if (> (missile-y m) 0)
             (list m)
             empty)]                    
        [else
         (if (> (missile-y m) 0)
             (cons m lom) 
             (cons (first lom) (rest lom)))]))

;; Missile -> Missile
;; Update position of missile.
(check-expect (process-missile M1) (make-missile (missile-x M1)
                                                 (- (missile-y M1) MISSILE-SPEED)))

;(define (process-missile m) m)

(define (process-missile m)
  (make-missile (missile-x m)
                (- (missile-y m) MISSILE-SPEED)))

;; Tank -> Tank
;; Advance tank position, tank bound to screen.
(check-expect (advance-tank T0) (make-tank (+ (tank-x T0) (* (tank-dir T0) TANK-SPEED))  ; Not at edge of screen, update position
                                           (tank-dir T0)))
(check-expect (advance-tank (make-tank 0 -1)) (make-tank 0 -1))                          ; At left edge of screen, bound to left edge
(check-expect (advance-tank (make-tank WIDTH 1)) (make-tank WIDTH 1))                    ; At right edge of screen, bound to right edge

;(define (advance-tank t) t) ;stub
(define (advance-tank t)
  (make-tank
   (cond [(> (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) WIDTH) WIDTH]
         [(< (+ (tank-x t) (* (tank-dir t) TANK-SPEED)) 0)     0]
         [else (+ (tank-x t) (* (tank-dir t) TANK-SPEED))])
   (tank-dir t)))


;; Game -> Image
;; Render game state.

;(define (render s) BACKGROUND) ;stub

(define (render s)
  (overlay (render-invaders (invader-struct-loi (game-invader-struct s)))
           (render-missiles (game-missiles s))
           (render-tank (game-tank s))))

;; ListofInvader -> Image
;; Render invaders.
(check-expect (render-invaders empty) BACKGROUND)
(check-expect (render-invaders (list I1)) (place-image INVADER (invader-x I1) (invader-y I1)
                                                       BACKGROUND))
(check-expect (render-invaders (list I1 I2)) (place-image INVADER (invader-x I1) (invader-y I1)
                                                          (place-image INVADER (invader-x I2) (invader-y I2)
                                                                       BACKGROUND)))

;(define (render-invaders loi) BACKGROUND) ;stub

(define (render-invaders loi)
  (cond [(empty? loi) BACKGROUND]                    
        [else (place-image INVADER (invader-x (first loi)) (invader-y (first loi)) 
                           (render-invaders (rest loi)))])) 


;; ListofMissile -> Image
;; Render missiles.
(check-expect (render-missiles empty) BACKGROUND)
(check-expect (render-missiles (list M1)) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                       BACKGROUND))
(check-expect (render-missiles (list M1 M2)) (place-image MISSILE (missile-x M1) (missile-y M1)
                                                          (place-image MISSILE (missile-x M2) (missile-y M2)
                                                                       BACKGROUND)))
;(define (render-missiles lom) BACKGROUND) ;stub

(define (render-missiles loi)
  (cond [(empty? loi) BACKGROUND]                    
        [else (place-image MISSILE (missile-x (first loi)) (missile-y (first loi)) 
                           (render-missiles (rest loi)))]))

;; Tank -> Image
;; Renders tank.

;(define (render-tank t) BACKGROUND) ;stub

(define (render-tank t)
  (place-image TANK (tank-x t) (- HEIGHT TANK-HEIGHT/2)
               BACKGROUND))


;; Game -> Boolean
;; Return true if any invader goes beyond the bottom of the play area

;(define (game-over? s) false) ;stub

(define (game-over? s)
  (landed? (invader-struct-loi (game-invader-struct s))))

;; ListOfInvader -> Boolean
;; Returns true if any invader goes beyond the bottom of the play area
(check-expect (landed? empty) false)           ;no invaders
(check-expect (landed? (list I1)) false)       ;not landed
(check-expect (landed? (list I2)) false)       ;exactly landed
(check-expect (landed? (list I3)) true)        ;beyond landed
(check-expect (landed? (list I1 I1 I1)) false) ;more than one invader not landed
(check-expect (landed? (list I1 I1 I3)) true)  ;more than one invader and one beyond landed

;(define (landed? loi) false) ;stub

(define (landed? loi)
  (cond [(empty? loi) false]
        [(> (invader-y (first loi)) HEIGHT) true]
        [else (landed? (rest loi))]))  


;; Game KeyEvent -> Game
;; Change direction of tank if left or right arrow key is pressed.
;; Fire a missile if space bar is pressed.
(check-expect (key-handler G2 "a") G2)  ; Not valid input, nothing changes
              
;(define (key-handler s ke) s) ;stub

(define (key-handler s ke)
  (make-game
   (game-invader-struct s)
   (if (key=? ke " ")
       (spawn-missile (game-missiles s) (tank-x (game-tank s)))
       (game-missiles s))
   (if (or (key=? ke "left") (key=? ke "right"))
       (change-dir (game-tank s) ke)
       (game-tank s))))

;; ListOfMissile Number -> ListOfMissile
;; Spawns a new missile in front of the tank..
;;   - Number is the x position of the tank
(check-expect (spawn-missile empty 20) (list (make-missile 20 (- HEIGHT (image-height TANK)))))
(check-expect (spawn-missile (list M1) 20) (list (make-missile 20 (- HEIGHT (image-height TANK)))
                                                 M1))

;(define (spawn-missile lom x) lom) ;stub

(define (spawn-missile lom x)
  (cons (make-missile x (- HEIGHT (image-height TANK)))
        lom))


;; Tank KeyEvent -> Tank
;; Sets the tank direction depending on input.
(check-expect (change-dir T0 "left") (make-tank (tank-x T0) -1))
(check-expect (change-dir T0 "right") (make-tank (tank-x T0) 1))

;(define (change-dir t ke) t) ;stub

(define (change-dir t ke)
  (make-tank (tank-x t)
             (cond [(key=? ke "left") -1]
                   [else 1])))