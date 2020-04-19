;; The first three lines of this file were inserted by DrRacket. They record metadata
;; about the language level of this file in a form that our tools can easily process.
#reader(lib "htdp-beginner-abbr-reader.ss" "lang")((modname space-invaders-starter) (read-case-sensitive #t) (teachpacks ()) (htdp-settings #(#t constructor repeating-decimal #f #t none #f () #f)))
(require 2htdp/universe)
(require 2htdp/image)

;; Space Invaders


;; Constants:

(define WIDTH  300)
(define HEIGHT 500)

(define INVADER-X-SPEED 1.5)  ;speeds (not velocities) in pixels per tick
(define INVADER-Y-SPEED 1.5)
(define TANK-SPEED 2)
(define MISSILE-SPEED 10)

(define HIT-RANGE 10)

(define INVADE-RATE 100)

(define BACKGROUND (empty-scene WIDTH HEIGHT))

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
(define TANK-CENTER (- HEIGHT (/ (image-height TANK) 2)))


;; Data Definitions:

(define-struct game (invaders missiles tank))
;; Game is (make-game  (listof Invader) (listof Missile) Tank)
;; interp. the current state of a space invaders game
;;         with the current invaders, missiles and tank position

;; Game constants defined below Missile data definition

#;
(define (fn-for-game s)
  (... (fn-for-loinvader (game-invaders s))
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

(define I1 (make-invader 150 100 12))           ;not landed, moving right
(define I2 (make-invader 150 HEIGHT -10))       ;exactly landed, moving left
(define I3 (make-invader 150 (+ HEIGHT 10) 10)) ;> landed, moving right


#;
(define (fn-for-invader invader)
  (... (invader-x invader) (invader-y invader) (invader-dx invader)))


(define-struct missile (x y))
;; Missile is (make-missile Number Number)
;; interp. the missile's location is x y in screen coordinates

(define M1 (make-missile 150 300))                       ;not hit U1
(define M2 (make-missile (invader-x I1) (+ (invader-y I1) 10)))  ;exactly hit U1
(define M3 (make-missile (invader-x I1) (+ (invader-y I1)  5)))  ;> hit U1

#;
(define (fn-for-missile m)
  (... (missile-x m) (missile-y m)))

;; ListOfMissiles is one of:
;; - empty
;; - (cons missle ListOfMissiles)
;; interp. a list of missiles

(define LOM1 empty)
(define LOM2 (cons (make-missile 150 300) (cons (make-missile 200 330) empty)))

#;
(define (fn-for-lom lom)
  (cond [(empty? lom) (...)]
        [else
         (... (fn-for-drop (first lom))
              (fn-for-lom (rest lom)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons missile ListOfMissiles)
;; - reference: (first lom) is Missiles
;; - self reference: (rest lom) is ListOfMissile

;; ListOfInvaders is one of:
;; - empty
;; - (cons invader ListOfInvaders)
;; interp. a list of invaders

(define LOI1 empty)
(define LOI2 (cons (make-invader 150 100 12) (cons (make-invader 250 200 25) empty)))

#;
(define (fn-for-loi loi)
  (cond [(empty? loi) (...)]
        [else
         (... (fn-for-drop (first loi))
              (fn-for-loi (rest loi)))]))

;; Template Rules used:
;; - one-of: 2 cases
;; - atomic distinct: empty
;; - compound: (cons invader ListOfInvaders)
;; - reference: (first loi) is Invader
;; - self reference: (rest loi) is ListOfInvaders

(define G0 (make-game empty empty T0))
(define G1 (make-game empty empty T1))
(define G2 (make-game (list I1) (list M1) T1))
(define G3 (make-game (list I1 I2) (list M1 M2) T1))


;; Functions:

;; Game -> Game
;; start the world with (main (make-game empty empty (make-tank T0)))
;; 
#;(define (main s)
  (big-bang s
            (on-tick   tock)     ; Game -> Game
            (to-draw   render)   ; Game -> Image
            (stop-when ...)      ; Game -> Boolean
            (on-key    ...)))    ; Game KeyEvent -> Game

(define (main s)
  (big-bang s
    (on-tick advance-game)
    (to-draw render-game)
    (on-key handle-key)
    (stop-when game-over?)))

;; advance-game
;; Game -> Game
;; Produces a new game state with updated object
;; change position of missiles, invaders, tank
;; remove missiles if off screen
;; remove missiles, invader if hit
;; produce new invader

;(define (advance-game g) G0) ; stub
; <template from data definition>


(check-expect (advance-game G0)
              (make-game empty empty (make-tank (+ (/ WIDTH 2) TANK-SPEED) 1)))
(check-expect (advance-game G1)
              (make-game empty empty (make-tank (+ 50 TANK-SPEED) 1)))
(check-expect (advance-game (make-game (cons (make-invader 150 100 12) (cons (make-invader 250 200 25) empty))
                         (cons (make-missile 150 300) (cons (make-missile 200 330) empty))
                         (make-tank 50 -1)))
              (make-game (cons (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12)
                               (cons (make-invader (+ 250 (* 25 INVADER-X-SPEED)) (+ 200 INVADER-Y-SPEED) 25) empty))
                         (cons (make-missile 150 (- 300 10))
                               (cons (make-missile 200 (- 330 10)) empty))
                         (make-tank (- 50 TANK-SPEED) -1)))

(define (advance-game g)
  (make-game (add-invader (update-invaders (remove-invaders (game-invaders g) (game-missiles g))))
             (update-missiles (filter-missiles (game-missiles g) (game-invaders g)))
             (move-tank (game-tank g))))

;; ListOfInvaders -> ListOfInvaders
;; add 1 invader at random x coordinate
(check-random (add-invader empty)
              (cond [(> INVADE-RATE (random 1000)) (cons (make-invader (random WIDTH) 10 1) empty)]
                    [else empty]))
(check-random (add-invader LOI2)
              (cond [(> INVADE-RATE (random 1000)) (cons (make-invader (random WIDTH) 10 1) LOI2)]
                    [else LOI2]))
                                                   

;(define (add-invader loi) loi) ;stub
(define (add-invader loi)
  (cond [(> INVADE-RATE (random 1000))
         (cons (make-invader (random WIDTH) 10 1) loi)]
        [else loi]))

;; ListOfInvaders -> ListOfInvaders
;; updates the list of invaders
(check-expect (update-invaders LOI1) LOI1)
(check-expect (update-invaders LOI2) (cons (make-invader (+ 150 (* 12 INVADER-X-SPEED))
                                                         (+ 100 INVADER-Y-SPEED) 12)
                                           (cons (make-invader (+ 250 (* 25 INVADER-X-SPEED))
                                                               (+ 200 INVADER-Y-SPEED) 25)
                                                               empty)))

;(define (update-invaders loi) loi) ;stub
(define (update-invaders loi)
  (cond [(empty? loi) empty]
        [else
         (cons (update-invader (first loi))
              (update-invaders (rest loi)))]))

;; Invader -> Invader
;; updates the position of an invader and reverses direction if wall is hit
(check-expect (update-invader I1) (make-invader (+ 150 (* 12 INVADER-X-SPEED)) (+ 100 INVADER-Y-SPEED) 12))
(check-expect (update-invader (make-invader WIDTH 120 1)) (make-invader WIDTH (+ 120 INVADER-Y-SPEED) -1))
(check-expect (update-invader (make-invader WIDTH 200 -1)) (make-invader (- WIDTH INVADER-X-SPEED) (+ 200 INVADER-Y-SPEED) -1))
(check-expect (update-invader (make-invader 0 200 -1)) (make-invader 0 (+ 200 INVADER-Y-SPEED) 1))
(check-expect (update-invader (make-invader (+ 0 (- INVADER-X-SPEED 1)) 200 -1)) (make-invader 0 (+ INVADER-Y-SPEED 200) 1))

;(define (update-invader i) i) ;stub
(define (update-invader invader)
  (cond[(< (+ (invader-x invader) (* (invader-dx invader) INVADER-X-SPEED)) 0)
        (make-invader 0 (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
       [(> (+ (invader-x invader) (* (invader-dx invader) INVADER-X-SPEED)) WIDTH)
        (make-invader WIDTH (+ (invader-y invader) INVADER-Y-SPEED) (- (invader-dx invader)))]
       [else
        (make-invader (+ (invader-x invader) (* (invader-dx invader) INVADER-X-SPEED))
                      (+ (invader-y invader) INVADER-Y-SPEED)
                      (invader-dx invader))]))

;; ListOfInvaders ListOfMissiles -> ListOfInvaders
;; Removes Invaders from the ListOfInvaders that have been hit by a missile
(check-expect (remove-invaders LOI1 LOM1) empty)
(check-expect (remove-invaders (cons (make-invader 100 100 1) empty)
                               (cons (make-missile 50 50) empty))
              (cons (make-invader 100 100 1) empty))
(check-expect (remove-invaders (cons (make-invader 100 100 1) (cons (make-invader 50 50 1) empty))
                              (cons (make-missile 50 50) (cons (make-missile 20 20) empty)))
              (cons (make-invader 100 100 1) empty))
(check-expect (remove-invaders (cons (make-invader 100 100 1) (cons (make-invader 125 125 2) (cons (make-invader 200 200 4) empty)))
                              (cons (make-missile 125 125) (cons (make-missile 75 75) (cons (make-missile 200 200) empty))))
              (cons (make-invader 100 100 1) empty))

;(define (remove-invaders loi lom) loi) ; stub
(define (remove-invaders loi lom)
  (cond [(empty? loi) empty]
        [else (if (invader-hit? (first loi) lom)
                  (remove-invaders (rest loi) lom)
                  (cons (first loi) (remove-invaders (rest loi) lom)))]))

;; Invader ListOfMissiles -> Boolean
;; produces True if invader's x,y coordinate is the same as the missile's x,y
;; coordinate. Returns false otherwise

(check-expect (invader-hit? (make-invader 100 100 1) (cons (make-missile 200 200)
                                                           (cons (make-missile 50 50) empty))) false)
(check-expect (invader-hit? (make-invader 200 200 1) (cons (make-missile 200 200)
                                                           (cons (make-missile 50 50) empty))) true)
;(define (hit-invader? invader lom) false) ;stub
(define (invader-hit? invader lom)
  (cond [(empty? lom) false]
        [else (or (hit? invader (first lom))
              (invader-hit? invader (rest lom)))]))

;; Invader Missile -> Boolean
;; Returns true if the invader and missile are in the same x, y coordinate.
;; Returns false otherwise
(check-expect (hit? (make-invader 100 100 1) (make-missile 50 50)) false)
(check-expect (hit? (make-invader 75 75 2) (make-missile 75 75)) true)

;(define (hit? invader missile) false) ; stub
(define (hit? invader missile)
  (and (<= (abs (- (invader-x invader) (missile-x missile))) HIT-RANGE)
       (<= (abs (- (invader-y invader) (missile-y missile))) HIT-RANGE)))


;; ListOfMissile -> ListOfMissiles
;; Update the location of the missile by decreasing its location by its speed.
;; Missiles beyond the screen will be removed from the list
(check-expect (update-missiles LOM1) LOM1)
(check-expect (update-missiles (cons (make-missile 100 100) empty)) (cons (make-missile 100 (- 100 MISSILE-SPEED)) empty))
(check-expect (update-missiles LOM2) (cons (make-missile 150 (- 300 MISSILE-SPEED))
                                           (cons (make-missile 200 (- 330 MISSILE-SPEED))
                                                 empty)))
(check-expect (update-missiles (cons (make-missile 200 -20)
                                     (cons (make-missile 220 15)
                                           empty)))
              (cons (make-missile 220 (- 15 MISSILE-SPEED)) empty))

;(define (update-missiles lom) lom) ;stub

(define (update-missiles lom)
  (cond [(empty? lom) empty]
        [else
         (if (off-screen? (first lom))
             (update-missiles (rest lom))
             (cons (update-missile (first lom))
                   (update-missiles (rest lom))))]))

;; Missile -> Boolean
;; produces true if Missile is off the screen, otherwise produces false
(check-expect (off-screen? M1) false)
(check-expect (off-screen? (make-missile 150 0)) false)
(check-expect (off-screen? (make-missile 150 (- (/ (image-height MISSILE) 2)))) true)
(check-expect (off-screen? (make-missile 150 -40)) true)
    
;(define (off-screen? missile) false) ;stub
(define (off-screen? m)
  (<= (missile-y m) (- (/ (image-height MISSILE) 2))))


;; Missile -> Missile
;; produces new location of missile
(check-expect (update-missile (make-missile 150 300))
              (make-missile 150 (- 300 MISSILE-SPEED)))
(check-expect (update-missile (make-missile 200 200))
              (make-missile 200 (- 200 MISSILE-SPEED)))

; (define (update-missile missile) missile) ;stub

(define (update-missile m)
  (make-missile (missile-x m) (- (missile-y m) MISSILE-SPEED)))

;; ListOfMissiles ListOfInvaders -> ListOfMissiles
;; produce list of missiles by removing missiles that hit invaders
(check-expect (filter-missiles (cons (make-missile 50 50) empty)
                               (cons (make-invader 200 200 10) empty))
              (cons (make-missile 50 50) empty))
(check-expect (filter-missiles (cons (make-missile 50 50) (cons (make-missile 200 200) empty))
                              (cons (make-invader 100 100 5) (cons (make-invader 50 50 3) empty)))
              (cons (make-missile 200 200) empty))
(check-expect (filter-missiles (cons (make-missile 50 50) (cons (make-missile 200 200) empty))
                              (cons (make-invader 200 100 5) (cons (make-invader 50 50 3) empty)))
              (cons (make-missile 200 200) empty))

; (define (filter-missiles lom loi) lom)

(define (filter-missiles lom loi)
  (cond [(empty? lom) empty]
        [else (if (missile-hit? (first lom) loi)
                  (filter-missiles (rest lom) loi)
                  (cons (first lom) (filter-missiles (rest lom) loi)))]))

;; Missile ListOfInvaders -> Boolean
;; produces true if missile hits an invader, otherwise produces false
(check-expect (missile-hit? (make-missile 200 200) (cons (make-invader 100 100 10) empty)) false)
(check-expect (missile-hit? (make-missile 200 200) (cons (make-invader 100 100 10)
                                                         (cons (make-invader 200 200 10) empty))) true)
;(define (missile-hit? m loi) false) ; stub
(define (missile-hit? m loi)
  (cond [(empty? loi) false]
        [else (or (hit? (first loi) m)
                  (missile-hit? m (rest loi)))]))

;; Tank -> Tank
;; Move the tank along the screen by TANK-SPEED
(check-expect (move-tank T0) (make-tank (+ (* 1 TANK-SPEED) (/ WIDTH 2)) 1))
(check-expect (move-tank T1) (make-tank (+ (* 1 TANK-SPEED) 50) 1))
(check-expect (move-tank T2) (make-tank (+ (* -1 TANK-SPEED) 50) -1))

;(define (move-tank t) t) ;stub

(define (move-tank t)
  (cond [(> (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (- WIDTH (/ (image-width TANK) 2)))
         (make-tank (- WIDTH (/ (image-width TANK) 2)) (tank-dir t))]
         [(< (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (/ (image-width TANK) 2))
          (make-tank (/ (image-width TANK) 2) (tank-dir t))]
         [else (make-tank (+ (tank-x t) (* TANK-SPEED (tank-dir t))) (tank-dir t))]))

;; Game -> Image
;; render tank, missiles, and invaders on the screen.
(check-expect (render-game G1) (place-image TANK 50 TANK-CENTER BACKGROUND))
(check-expect (render-game G2) (place-image TANK 50 TANK-CENTER
                                       (place-image MISSILE 150 300
                                                    (place-image INVADER 150 100 BACKGROUND))))

;(define (render-game g) BACKGROUND) ; stub

(define (render-game g)
  (render-invaders (game-invaders g)
                   (render-missiles (game-missiles g)
                                    (render-tank (game-tank g)))))

;; Tank Image -> Image
;; produces and image with tank
(check-expect (render-tank T0) (place-image TANK (/ WIDTH 2) TANK-CENTER BACKGROUND))
(check-expect (render-tank T1)
              (place-image TANK 50 TANK-CENTER BACKGROUND))

;(define (render-tank t) BACKGROUND) ; stub
(define (render-tank t)
  (place-image TANK (tank-x t) TANK-CENTER BACKGROUND))

;; ListOfInvaders Image -> Image
;; produces an image with the invaders added to the existing image
(check-expect (render-invaders LOI1 BACKGROUND) BACKGROUND)
(check-expect (render-invaders LOI2 (place-image TANK 50 TANK-CENTER BACKGROUND))
              (place-image INVADER 150 100
                           (place-image INVADER 250 200
                                        (place-image TANK 50 TANK-CENTER BACKGROUND))))

;(define (render-invaders loi img) BACKGROUND) ; stub
(define (render-invaders loi img)
  (cond [(empty? loi) img]
        [else
         (render-invader (first loi)
                      (render-invaders (rest loi) img))]))
  
;; Invader Image -> Image
;; produces image with invader added
(check-expect (render-invader (make-invader 100 100 1) BACKGROUND)
              (place-image INVADER 100 100 BACKGROUND))
(check-expect (render-invader (make-invader 100 100 1)
                           (place-image INVADER 150 100 (place-image INVADER 250 200 (place-image TANK 50 TANK-CENTER BACKGROUND))))
              (place-image INVADER 100 100 (place-image INVADER 150 100 (place-image INVADER 250 200 (place-image TANK 50 TANK-CENTER BACKGROUND)))))
;(define (render-invader i img) BACKGROUND) ; stub
(define (render-invader i img)
  (place-image INVADER (invader-x i) (invader-y i) img))

;; ListOfMissiles Image -> Image
;; produces image with the missiles added to the existing image
(check-expect (render-missiles LOM1 BACKGROUND) BACKGROUND)
(check-expect (render-missiles LOM2 (place-image INVADER 100 100
                                                 (place-image INVADER 150 100
                                                              (place-image INVADER 250 200
                                                                           (place-image TANK 50 TANK-CENTER BACKGROUND)))))
              (place-image MISSILE 150 300
                           (place-image MISSILE 200 330
                                        (place-image INVADER 100 100
                                                 (place-image INVADER 150 100
                                                              (place-image INVADER 250 200
                                                                           (place-image TANK 50 TANK-CENTER BACKGROUND)))))))
                                        
;(define (render-missiles lom img) BACKGROUND) ; stub
(define (render-missiles lom img)
  (cond [(empty? lom) img]
        [else
         (render-missile (first lom)
                         (render-missiles (rest lom) img))]))

;; Missile Image -> Image
;; produces image with missile added
(check-expect (render-missile (make-missile 100 100) BACKGROUND)
              (place-image MISSILE 100 100 BACKGROUND))
(check-expect (render-missile (make-missile 100 100)
                              (place-image INVADER 150 100 (place-image INVADER 250 200 (place-image TANK 50 TANK-CENTER BACKGROUND))))
              (place-image MISSILE 100 100 (place-image INVADER 150 100 (place-image INVADER 250 200 (place-image TANK 50 TANK-CENTER BACKGROUND)))))
;(define (render-missile m img) BACKGROUND) ;stub
(define (render-missile m img)
  (place-image MISSILE (missile-x m) (missile-y m) img))

;; Game KeyEvent -> Game
;; handles key presses
;; left arrow key causes the tank to move left
;; right arrow key causes the tank to move right
;; space key fires a missile
(check-expect (handle-key G0 "a") G0)
(check-expect (handle-key G0 "left") (make-game empty empty (make-tank (/ WIDTH 2) -1)))
(check-expect (handle-key G0 "right") (make-game empty empty (make-tank (/ WIDTH 2) 1)))
(check-expect (handle-key G3 "l") G3)
(check-expect (handle-key G3 "left") (make-game (list I1 I2) (list M1 M2) (make-tank 50 -1)))
(check-expect (handle-key G3 "right") (make-game (list I1 I2) (list M1 M2) (make-tank 50 1)))
(check-expect (handle-key G0 " ") (make-game empty (cons (make-missile (/ WIDTH 2) HEIGHT) empty) T0))
(check-expect (handle-key G3 " ") (make-game (list I1 I2)
                                             (list (make-missile 50 HEIGHT) M1 M2)
                                             T1))

;(define (handle-key g ke) g) ; stub
(define (handle-key g ke)
  (cond [(key=? ke " ")
         (make-game (game-invaders g) (cons (make-missile (tank-x (game-tank g)) HEIGHT) (game-missiles g)) (game-tank g))]
        [(key=? ke "left")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) -1))]
        [(key=? ke "right")
         (make-game (game-invaders g) (game-missiles g) (make-tank (tank-x (game-tank g)) 1))]
        [else 
         g]))

;; Game -> Boolean
;; produces true when the game is over
(check-expect (game-over? G0) false)
(check-expect (game-over? (make-game (list (make-invader 100 HEIGHT 10) (make-invader 200 200 2))
                                     (list (make-missile 20 30) (make-missile 40 40))
                                     (make-tank 50 1))) true)

;(define (game-over? g) false) ;stub
(define (game-over? g)
  (invader-landed? (game-invaders g)))

;; ListOfInvaders -> Boolean
;; produces true if any invader has touched the bottom of the game screen
(check-expect (invader-landed? empty) false)
(check-expect (invader-landed? LOI2) false)
(check-expect (invader-landed? (list (make-invader 100 HEIGHT 2) (make-invader 100 200 3))) true)

;(define (invader-landed? loi) false) ;stub
(define (invader-landed? loi)
  (cond [(empty? loi) false]
        [else (or (landed? (first loi))
                  (invader-landed? (rest loi)))]))

;; Invader -> Boolean
;; produces true if Invader has reached the bottom of the game screen
(check-expect (landed? I1) false)
(check-expect (landed? (make-invader 100 HEIGHT 1)) true)

;(define (landed? i) false) ;stub
(define (landed? i)
  (>= (invader-y i) HEIGHT))