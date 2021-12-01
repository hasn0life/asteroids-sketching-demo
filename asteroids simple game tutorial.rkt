#lang sketching

;; port of https://simplegametutorials.github.io/love/asteroids/
;; in sketching to try it out

;; global constants
(define WIDTH 800)
(define HEIGHT 600)
(define SHIP-RADIUS 30)
(define BULLET-SPEED 500)
(define BULLET-RADIUS 5)
(define BULLET-TIMER-LIMIT 0.5)

;; using a vector of lists but it can be a list of structs or other data structures
;; variables are '(speed radius)
;; while this lang does offer a notation for vector access, it can't nest accesses as far as I can tell?
(define ASTEROID-STAGES (vector '(120 15) '(70 30) '(50 50) '(20 80)))
(define prev-millis 0)

(define bullet-timer BULLET-TIMER-LIMIT)

;; unlike the original tutorial we're gonna use structs to hold these variables
;; which I think is neater
(struct ship (x y speed-x speed-y angle))
(struct bullet (x y angle timeleft))
(struct asteroid (x y angle stage))

;; keep these empty so we can fill them with the reset function
(define myShip '()) 
(define bullets '())
(define asteroids '())

(define (reset)
  (set! myShip (ship (/ WIDTH 2) (/ HEIGHT 2) 0 0 0))
  (set! bullets '())
  (set! asteroids (list
                  (asteroid 100 100 (* (random 1) 2 pi) (sub1 (vector-length ASTEROID-STAGES)))
                  (asteroid (- WIDTH 100) 100 (* (random 1) 2 pi) (sub1 (vector-length ASTEROID-STAGES)))
                  (asteroid (/ WIDTH 2) (- HEIGHT 100) (* (random 1) 2 pi) (sub1 (vector-length ASTEROID-STAGES))))))

;; keeps track of whether we're holding down the space bar
;; to stop it from getting invalidated when we press other keys
(define space-key-pressed #f)

(define (on-key-pressed)
  (when (or (equal? key #\space) (equal? key #\s) (equal? key #\S))
    (set! space-key-pressed #t))) 

(define (on-key-released)
  (when (or (equal? key #\space) (equal? key #\s) (equal? key #\S))
    (set! space-key-pressed #f)))
     
(define (are-circles-intersecting aX aY aRad bX bY bRad)
  (<= (+ (sq (- aX bX)) (sqr (- aY bY)))
      (sq (+ aRad bRad))))

;; racket's modulo only works on numbers so we make our own for floats
(define (flmod x m)
    (- x (* (floor (/ x m)) m)))

;; we use setup instead of load
;; for whatever reason the game doesnt start focused in dr racket
(define (setup)
  (size 800 600)
  (background 0)
  (set-frame-rate! 30) ;; we get smooth performance at 30 with occasional stutters
  (no-stroke)
  (reset))

;; we don't have seperate update and draw steps since this is
;; processing inspired
(define (draw)

  ;; love's update function provides the time difference between
  ;; the frames, but we have to calculate this ourselves
  (define current-millis (millis))
  (define dt (/ (- current-millis prev-millis) 1000.0))
  (set! prev-millis current-millis)
  
  (define turn-speed 10)

  ;; the key-pressed and key function work together to approximate 
  ;; love's keyboard.isDown() function
  (when (and key-pressed (equal? key 'right))
    (+= myShip.angle (* dt turn-speed)))
  
  (when (and key-pressed (equal? key 'left))
    (-= myShip.angle (* dt turn-speed)))

  
  (:= myShip.angle (flmod myShip.angle (* 2 pi)))
  
  (define ship-speed 100)
  
  (when (and key-pressed (equal? key 'up))
    (+= myShip.speed-x  (* (cos myShip.angle) ship-speed dt))
    (+= myShip.speed-y  (* (sin myShip.angle) ship-speed dt)))

  ;; however, we need a different approach for the space bar since we're usually
  ;; holding it down while moving the ship
  (when space-key-pressed 
    (+= bullet-timer dt)
    (when (> bullet-timer BULLET-TIMER-LIMIT)
      (set! bullet-timer 0)
      (set! bullets (cons
                     (bullet (+ myShip.x (* (cos myShip.angle) SHIP-RADIUS))
                             (+ myShip.y (* (sin myShip.angle) SHIP-RADIUS))
                             myShip.angle
                             3)
                     bullets))))
    
  
  ;; physics
  
  (:= myShip.x (flmod (+ myShip.x (* myShip.speed-x dt)) WIDTH))
  (:= myShip.y (flmod (+ myShip.y (* myShip.speed-y dt)) HEIGHT))

  (define temp-list bullets)
  
  (for ([b (in-list bullets)])
    (-= b.timeleft dt)
    (when (<= b.timeleft 0)
      (set! temp-list (remove b temp-list))))

  (set! bullets temp-list)
        
  (for ([b (in-list bullets)])
    (:= b.x (flmod  (+ b.x (* (cos b.angle) BULLET-SPEED dt)) WIDTH))
    (:= b.y (flmod  (+ b.y (* (sin b.angle) BULLET-SPEED dt)) HEIGHT))
    
    (for ([a (in-list asteroids)])
      ;; using vector ref and cadr to access the asteroid stage variables
      (when (are-circles-intersecting b.x b.y BULLET-RADIUS
                                      a.x a.y (cadr (vector-ref ASTEROID-STAGES
                                                                a.stage)))
        (set! bullets (remove b bullets))
        (define angle (random (* 2 pi)))
        (if (> a.stage 0)
            (set! asteroids
                  (append (list (asteroid a.x a.y angle (sub1 a.stage))
                                (asteroid a.x a.y (- angle pi ) (sub1 a.stage)))
                          (remove a asteroids)))
            (set! asteroids (remove a asteroids)))
        )))
        

  (for ([a (in-list asteroids)] )
    (:= a.x (flmod (+ a.x (* (cos a.angle)
                             (car (vector-ref ASTEROID-STAGES a.stage))
                             dt)) WIDTH))
    (:= a.y (flmod (+ a.y (* (sin a.angle)
                             (car (vector-ref ASTEROID-STAGES a.stage))
                             dt)) HEIGHT))
    (when (are-circles-intersecting
           myShip.x myShip.y SHIP-RADIUS
           a.x a.y
           (cadr (vector-ref ASTEROID-STAGES a.stage)))
      (reset) )) ;;lose the game

  (when (= (length asteroids) 0)
    (reset)) ;; win the game
    
  ;; draw here

  (background 0)
  
  (for* ([y (in-range -1 1)]
         [x (in-range -1 1)])
    (push-matrix) ;; push matrix is basically the same as origin here? 
    (translate (* x WIDTH) (* y HEIGHT))
        
    (no-stroke)
    (fill "blue")
    (circle myShip.x myShip.y (* 2 SHIP-RADIUS))

    (for ([b (in-list bullets)])
      (fill "green")
      (circle b.x b.y (* 2 BULLET-RADIUS)))
    
    (define inner-rad 20)
    (fill "white")
    (circle (+ myShip.x (* (cos myShip.angle) inner-rad))
            (+ myShip.y (* (sin myShip.angle) inner-rad))
            5)

    (for ([a (in-list asteroids)])
      (fill "yellow")
      (circle a.x a.y (* 2 (cadr (vector-ref ASTEROID-STAGES a.stage)))))

    (pop-matrix) )

  ;; we can print some debug parameters
   ; (fill 0 255 0)
   ; (text (~a "Angle: " myShip.angle
   ;           " speed: " myShip.speed-x " " myShip.speed-y) 40 50)
  )
  