(import * from gui)
(import * from graphics)

(defobject shape (x y angle xv yv anglev geometry))
(defobject missile (x y angle xv yv anglev geometry count))
(defobject asteroid (x y angle xv yv anglev geometry area r2))

(defvar *screen*)
(defvar *canvas*)

(defvar *ship-geometry* '((-5 5) (0 2) (5 5) (0 -10)))
(defvar *ship*)
(defvar *ship-tsv* 0)
(defvar *ship-tav* 0)
(defvar *dead* false)

(defvar *missile-geometry* '((0 -2) (0 2)))
(defvar *missiles* (list))

(defvar *asteroids* (list))
(defconstant *asteroid-speed* 5)
(defconstant *asteroid-rspeed* (* pi 0.01))

(defun asteroid (x y area)
  (let ((geometry (list))
        (r0 (/ (sqrt area) pi)))
    (dotimes (i 20)
      (let ((r (* r0 (+ 1 (* (random) 0.2) -0.1)))
            (a (* i 2 pi (/ 20))))
        (push (list (* r (cos a)) (* r (sin a)))
              geometry)))
    (make-asteroid x: x
                   y: y
                   angle: (* (random) 2 pi)
                   xv: (* (- (random) 0.5) *asteroid-speed*)
                   yv: (* (- (random) 0.5) *asteroid-speed*)
                   anglev: (* (- (random) 0.5) *asteroid-rspeed*)
                   geometry: geometry
                   area: area
                   r2: (* r0 r0 1.1 1.1))))

(defun draw (e canvas)
  (with-canvas canvas
    (stroke-style (if (and (= e *ship*) *dead*)
                      "#FF0000"
                      "#FFFFFF"))
    (line-width 2)
    (let ((first true)
          (cos (cos e.angle))
          (sin (sin e.angle)))
      (begin-path)
      (dolist ((px py) e.geometry)
        (let* ((tx (+ e.x (* px cos) (* py sin)))
               (ty (+ e.y (* py cos) (* px (- sin)))))
          (if first
              (move-to tx ty)
              (line-to tx ty))
          (setf first false)))
      (close-path)
      (stroke))))

(defun update (e)
  (setf e.x (% (+ (% (+ e.x e.xv) *canvas*.offsetWidth)
                  *canvas*.offsetWidth)
               *canvas*.offsetWidth))
  (setf e.y (% (+ (% (+ e.y e.yv) *canvas*.offsetHeight)
                  *canvas*.offsetHeight)
               *canvas*.offsetHeight))
  (incf e.angle e.anglev))

(defun inside (x y geometry)
  (do ((inside false)
       (j (1- (length geometry)) i)
       (i 0 (1+ i)))
      ((>= i (length geometry))
       inside)
    (let (((x0 y0) (aref geometry i))
          ((x1 y1) (aref geometry j)))
      (when (or (and (<= y0 y) (< y y1))
                (and (<= y1 y) (< y y0)))
        (when (<= x (+ x0 (/ (* (- y y0) (- x1 x0)) (- y1 y0))))
          (setf inside (not inside)))))))

(defun dist2 (x0 y0 x1 y1)
  (let ((dx (- x1 x0))
        (dy (- y1 y0)))
    (+ (* dx dx) (* dy dy))))

(defun hits (x y a)
  (when (< (dist2 x y a.x a.y) a.r2)
    (let* ((cos (cos (- a.angle)))
           (sin (sin (- a.angle)))
           (dx (- x a.x))
           (dy (- y a.y))
           (rx (+ (* cos dx) (* sin dy)))
           (ry (- (* cos dy) (* sin dx))))
      (inside rx ry a.geometry))))

(defun check-hits ()
  (labels ((on-target (x y)
             (let ((new-asteroids (list))
                   (exploded false))
               (dolist (a *asteroids*)
                 (if (and (not exploded) (hits x y a))
                     (progn
                       (setf exploded true)
                       (when (> a.area (* 40 40))
                         (repeat 2
                           (push (asteroid a.x a.y (/ a.area 3))
                                 new-asteroids))))
                     (push a new-asteroids)))
               (setf *asteroids* new-asteroids)
               exploded)))
    (setf *missiles* (filter (lambda (m)
                               (not (on-target m.x m.y)))
                             *missiles*))))

(defun check-destruction ()
  (if *dead*
      (when (= 0 (length *missiles*))
        (setf *dead* false))
      (when (setf *dead* (any (a *asteroids*)
                              (hits *ship*.x *ship*.y a)))
        (repeat 100
          (let ((a (* (random) pi 2))
                (s (* (random) 5)))
            (push (make-missile x: *ship*.x
                                y: *ship*.y
                                angle: a
                                xv: (* s (sin a))
                                yv: (* s (cos a))
                                anglev: 0
                                geometry: *missile-geometry*
                                count: (/ *canvas*.offsetWidth 4))
                  *missiles*))))))

(defun main ()
  (setf *screen* (set-style (create-element "div")
                            position "absolute"
                            px/left 0
                            px/right 0
                            px/top 0
                            px/bottom 0))
  (append-child document.body *screen*)
  (setf *canvas* (set-style (create-element "canvas")
                            position "absolute"
                            px/left 0
                            px/top 0
                            px/width (1- *screen*.offsetWidth)
                            px/height (1- *screen*.offsetHeight)))
  (append-child *screen* *canvas*)

  (repeat 10
    (push (asteroid (* (random) *canvas*.offsetWidth)
                    (* (random) *canvas*.offsetHeight)
                    (* 200 200))
          *asteroids*))

  (setf *ship* (make-shape x: (/ *canvas*.offsetWidth 2)
                           y: (/ *canvas*.offsetHeight 2)
                           angle: 0
                           xv: 0
                           yv: 0
                           anglev: 0
                           geometry: *ship-geometry*))

  (set-interval (lambda ()
                  (setf *canvas*.width *canvas*.offsetWidth)
                  (setf *canvas*.height *canvas*.offsetHeight)
                  (with-canvas *canvas*
                    (fill-style "#000000")
                    (fill-rect 0 0 *canvas*.width *canvas*.height))
                  (dolist (e (append *asteroids* *missiles* (list *ship*)))
                    (update e)
                    (draw e *canvas*))
                  (check-destruction)
                  (unless *dead*
                    (check-hits))
                  (setf *missiles* (filter (lambda (m)
                                             (>= (decf m.count) 0))
                                           *missiles*))
                  (let ((sx (* *ship-tsv* (- (sin *ship*.angle))))
                        (sy (* *ship-tsv* (- (cos *ship*.angle)))))
                    (setf *ship*.xv (+ (* 0.95 *ship*.xv) (* 0.05 sx)))
                    (setf *ship*.yv (+ (* 0.95 *ship*.yv) (* 0.05 sy)))
                    (setf *ship*.anglev (+ (* 0.95 *ship*.anglev) (* 0.05 *ship-tav*)))))
                20)

  (set-handler (js-code "window") onkeydown
               (case event.which
                 (37 (setf *ship-tav* 0.1))
                 (39 (setf *ship-tav* -0.1))
                 (38 (setf *ship-tsv* 4))
                 (40 (setf *ship-tsv* -4))
                 (32 (push (make-missile x: *ship*.x
                                         y: *ship*.y
                                         angle: *ship*.angle
                                         xv: (* -5 (sin *ship*.angle))
                                         yv: (* -5 (cos *ship*.angle))
                                         anglev: 0
                                         geometry: *missile-geometry*
                                         count: (/ *canvas*.offsetWidth 5 2))
                           *missiles*))
                 (13 (dotimes (i 40)
                       (let ((a (/ (* i 2 pi) 40)))
                         (push (make-missile x: *ship*.x
                                             y: *ship*.y
                                             angle: a
                                             xv: (* -5 (sin a))
                                             yv: (* -5 (cos a))
                                             anglev: 0
                                             geometry: *missile-geometry*
                                             count: (/ *canvas*.offsetWidth 5 4))
                               *missiles*)))))
               (event.stopPropagation)
               (event.preventDefault))

  (set-handler (js-code "window") onkeyup
               (case event.which
                 (37 (setf *ship-tav* 0))
                 (39 (setf *ship-tav* 0))
                 (38 (setf *ship-tsv* 0))
                 (40 (setf *ship-tsv* 0))
                 (otherwise (display event.which)))
               (event.stopPropagation)
               (event.preventDefault)))
(main)