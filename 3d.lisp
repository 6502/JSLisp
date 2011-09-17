(defun v (&rest coords) coords)

(defun x (p) (first p))
(defun y (p) (second p))
(defun z (p) (third p))

(defun v+ (&rest pts)
  (reduce (lambda (a b) (mapn #'+ a b)) pts))

(defun v- (&rest pts)
  (reduce (lambda (a b) (mapn #'- a b)) pts))

(defun v* (v k)
  (map (lambda (x) (* x k)) v))

(defun v/ (v k)
  (map (lambda (x) (/ x k)) v))

(defun v. (a b)
  (reduce #'+ (mapn #'* a b)))

(defun vlen (x)
  (sqrt (v. x x)))

(defun vdir (x)
  (v/ x (vlen x)))

(defun v^ (a b)
  (v (- (* (y a) (z b)) (* (z a) (y b)))
     (- (* (z a) (x b)) (* (x a) (z b)))
     (- (* (x a) (y b)) (* (y a) (x b)))))

(defstruct camera
  o u v n)

(defun camera (from to up dist)
  (let* ((n (vdir (v- to from)))
         (u (v* (vdir (v^ up n)) dist))
         (v (v^ n u)))
    (make-camera :o from
                 :n n
                 :u u
                 :v v)))

(defun camera-map (camera p)
  (let* ((x (v- p (camera-o camera)))
         (z (v. x (camera-n camera)))
         (zs (/ z))
         (xs (* (v. x (camera-u camera)) zs))
         (ys (* (v. x (camera-v camera)) zs)))
    (v xs ys zs)))

(defun camera-invmap (camera xs ys)
  (let ((dist (vlen (camera-u camera))))
    (v+ (camera-o camera)
        (v* (camera-u camera) (/ xs dist))
        (v* (camera-v camera) (/ ys dist))
        (v* (camera-n camera) dist))))

(defun camera-normalize (camera)
  (let* ((n (camera-n camera))
         (u (camera-u camera))
         (v (camera-v camera))
         (dist (vlen u)))
    (setf (camera-n camera) (vdir n))
    (setf u (v- u (v* n (v. u n))))
    (setf (camera-u camera) (v* (vdir u) dist))
    (setf (camera-v camera)
          (v^ (camera-n camera)
              (camera-u camera)))))

(load (http-get "gui.lisp"))

(defvar *faces* (list))

(dolist (i '(-1 0 1))
  (dolist (j '(-1 0 1))
    (push (list "#0000FF"
                (v (- i 0.5) (- j 0.5) -1.5)
                (v (+ i 0.5) (- j 0.5) -1.5)
                (v (+ i 0.5) (+ j 0.5) -1.5)
                (v (- i 0.5) (+ j 0.5) -1.5)) *faces*)

    (push (list "#FFFF00"
                (v (+ i 0.5) (- j 0.5) 1.5)
                (v (- i 0.5) (- j 0.5) 1.5)
                (v (- i 0.5) (+ j 0.5) 1.5)
                (v (+ i 0.5) (+ j 0.5) 1.5)) *faces*)

    (push (list "#00FF00"
                (v (+ i 0.5) -1.5 (- j 0.5))
                (v (- i 0.5) -1.5 (- j 0.5))
                (v (- i 0.5) -1.5 (+ j 0.5))
                (v (+ i 0.5) -1.5 (+ j 0.5))) *faces*)

    (push (list "#FF00FF"
                (v (- i 0.5) 1.5 (- j 0.5))
                (v (+ i 0.5) 1.5 (- j 0.5))
                (v (+ i 0.5) 1.5 (+ j 0.5))
                (v (- i 0.5) 1.5 (+ j 0.5))) *faces*)

    (push (list "#FF0000"
                (v -1.5 (- i 0.5) (- j 0.5))
                (v -1.5 (+ i 0.5) (- j 0.5))
                (v -1.5 (+ i 0.5) (+ j 0.5))
                (v -1.5 (- i 0.5) (+ j 0.5))) *faces*)

    (push (list "#00FFFF"
                (v 1.5 (+ i 0.5) (- j 0.5))
                (v 1.5 (- i 0.5) (- j 0.5))
                (v 1.5 (- i 0.5) (+ j 0.5))
                (v 1.5 (+ i 0.5) (+ j 0.5))) *faces*)))

(dolist (f *faces*)
  (dotimes (i (1- (length f)))
    (setf (aref f (1+ i))
          (v* (aref f (1+ i)) 100))))

(defstruct section n k0 k1)

(defvar *sections* (list (make-section :n (v 1 0 0) :k0 -160 :k1 -40)
                         (make-section :n (v 1 0 0) :k0 -60 :k1 60)
                         (make-section :n (v 1 0 0) :k0 40 :k1 160)
                         (make-section :n (v 0 1 0) :k0 -160 :k1 -40)
                         (make-section :n (v 0 1 0) :k0 -60 :k1 60)
                         (make-section :n (v 0 1 0) :k0 40 :k1 160)
                         (make-section :n (v 0 0 1) :k0 -160 :k1 -40)
                         (make-section :n (v 0 0 1) :k0 -60 :k1 60)
                         (make-section :n (v 0 0 1) :k0 40 :k1 160)))

(defun face-in-section (face section)
  (let ((m (v/ (reduce #'v+ (rest face)) (1- (length face))))
        (n (section-n section)))
    (<= (section-k0 section) (v. m n) (section-k1 section))))

(defun xrot (angle)
  (let ((c (cos angle))
        (s (sin angle))
        (n (- (sin angle))))
    (list  1  0  0
           0  c  s
           0  n  c
           0  0  0)))

(defun yrot (angle)
  (let ((c (cos angle))
        (s (sin angle))
        (n (- (sin angle))))
    (list  c  0  s
           0  1  0
           n  0  c
           0  0  0)))

(defun zrot (angle)
  (let ((c (cos angle))
        (s (sin angle))
        (n (- (sin angle))))
    (list  c  s  0
           n  c  0
           0  0  1
           0  0  0)))

(defun xform (m p)
  (let ((x (x p))
        (y (y p))
        (z (z p)))
    (v (+ (* x (aref m 0))
          (* y (aref m 3))
          (* z (aref m 6))
          (aref m 9))
       (+ (* x (aref m 1))
          (* y (aref m 4))
          (* z (aref m 7))
          (aref m 10))
       (+ (* x (aref m 2))
          (* y (aref m 5))
          (* z (aref m 8))
          (aref m 11)))))

(defun animate (section angle duration redraw)
  (let ((faces (filter (lambda (f) (face-in-section f section))
                       *faces*))
        (xf (cond
              ((= (x (section-n section)) 1) #'xrot)
              ((= (y (section-n section)) 1) #'yrot)
              (true #'zrot)))
        (f null)
        (start (clock))
        (i 0))
    (setf f (lambda ()
              (let ((ci (/ (- (clock) start) duration)))
                (when (> ci 1)
                  (setf ci 1))
                (let ((s (- (* 3 ci ci) (* 2 ci ci ci))))
                  (let ((x (funcall xf (* angle (- s i)))))
                    (dolist (f faces)
                      (dotimes (j (1- (length f)))
                        (setf (aref f (1+ j)) (xform x (aref f (1+ j)))))))
                  (funcall redraw)
                  (setf i s))
                (when (< ci 1)
                  (set-timeout f 10)))))
    (set-timeout f 10)))

(defun inside (p pts)
  (let ((n (length pts))
        (inside false)
        (x (x p))
        (y (y p)))
    (do ((j (1- n) i)
         (i 0 (1+ i)))
        ((>= i n) inside)
      (let* ((p0 (aref pts j))
             (p1 (aref pts i))
             (x0 (x p0)) (y0 (y p0))
             (x1 (x p1)) (y1 (y p1)))
        (when (or (and (<= y0 y) (< y y1))
                  (and (<= y1 y) (< y y0)))
          (when (>= (+ x0 (/ (* (- y y0) (- x1 x0)) (- y1 y0))) x)
            (setf inside (not inside))))))))

(defun hit3d (p0 p1 face)
  ; <p0 + t(p1 - p0), n> = k
  ; <p0, n> + t<p1 - p0, n> = k
  ; t = (k - <p0, n>) / <p1 - p0, n>
  (let* ((n (v^ (v- (third face) (second face))
                (v- (fourth face) (third face))))
         (k (v. (third face) n))
         (t (/ (- k (v. p0 n)) (v. (v- p1 p0) n))))
    (v+ p0 (v* (v- p1 p0) t))))

(let* ((canvas (create-element "canvas"))
       (layout (:Hdiv canvas))
       (cb null)
       (frame (window 100 100 350 450
                      :title "3d view"
                      :close (lambda () (clear-interval cb))
                      :layout layout))
       (cam (camera (v -400 -600 -1000) (v 0 0 0) (v 0 1 0) 800))
       (marks (list)))
  (labels ((visible-faces ()
             (let ((xfaces (map (lambda (f)
                                  (let ((xp (map (lambda (p) (camera-map cam p))
                                                 (slice f 1))))
                                    (list (max (map #'z xp))
                                          (first f)
                                          xp
                                          f)))
                                (filter (lambda (f)
                                          (> 0
                                             (v. (v- (camera-o cam) (third f))
                                                 (v^ (v- (third f) (second f))
                                                     (v- (fourth f) (third f))))))
                                        *faces*))))
               (nsort xfaces (lambda (a b) (< (first a) (first b))))
               xfaces))
           (redraw ()
             (let* ((ctx (funcall (. canvas getContext) "2d"))
                    (w (. canvas width))
                    (h (. canvas height))
                    (zx (/ w 2))
                    (zy (/ h 2)))
               (setf (. ctx fillStyle) "#808080")
               (funcall (. ctx fillRect) 0 0 w h)
               (setf (. ctx strokeStyle) "#000000")
               (setf (. ctx lineWidth) 1)
               (dolist (xf (visible-faces))
                 (setf (. ctx fillStyle) (second xf))
                 (funcall (. ctx beginPath))
                 (let ((pts (third xf)))
                   (funcall (. ctx moveTo)
                            (+ zx (x (first pts)))
                            (+ zy (y (first pts))))
                   (dolist (p (slice pts 1))
                     (funcall (. ctx lineTo)
                              (+ zx (x p))
                              (+ zy (y p))))
                   (funcall (. ctx closePath))
                   (funcall (. ctx fill))
                   (funcall (. ctx stroke))))
               (setf (. ctx fillStyle) "#000000")
               (dolist (p marks)
                 (let ((pp (camera-map cam p)))
                   (funcall (. ctx beginPath))
                   (funcall (. ctx arc)
                            (+ zx (x pp))
                            (+ zy (y pp))
                            2
                            0 (* 2 pi) true)
                   (funcall (. ctx fill)))))))

    (append-child frame canvas)

    (setf cb (set-interval (lambda ()
                             (let ((w (. canvas offsetWidth))
                                   (h (. canvas offsetHeight)))
                               (when (or (/= w (. canvas width))
                                         (/= h (. canvas height)))
                                 (setf (. canvas width) w)
                                 (setf (. canvas height) h)
                                 (redraw))))
                           100))

    (set-handler canvas onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (let ((x0 (first (event-pos event)))
                       (y0 (second (event-pos event)))
                       (cx (first (element-pos canvas)))
                       (cy (second (element-pos canvas)))
                       (w (. canvas width))
                       (h (. canvas height)))
                   (dolist (xf (visible-faces))
                     (when (inside (v (- x0 cx (/ w 2))
                                      (- y0 cy (/ h 2)))
                                   (third xf))
                       (push (hit3d (camera-o cam)
                                    (camera-invmap cam
                                                   (- x0 cx (/ w 2))
                                                   (- y0 cy (/ h 2)))
                                    (fourth xf))
                             marks)
                       (when false
                         (let* ((f (fourth xf))
                                (sections (filter (lambda (s)
                                                    (and (< (abs (v. (section-n s)
                                                                     (v^ (v- (third f) (second f))
                                                                         (v- (fourth f) (third f)))))
                                                            0.5)
                                                         (face-in-section f s)))
                                                  *sections*)))
                           (when (> (length sections) 0)
                             (animate (first sections)
                                      (/ pi 2)
                                      1000
                                      #'redraw))))))
                   (redraw)
                   (tracking (lambda (x y)
                               (let* ((dx (- x x0))
                                      (dy (- y y0))
                                      (p1 (camera-invmap cam 0 0))
                                      (p2 (camera-invmap cam dx dy)))
                                 (setf (camera-o cam)
                                       (v* (vdir (v+ (camera-o cam) (v* (v- p1 p2) 4)))
                                           (vlen (camera-o cam))))
                                 (setf (camera-n cam)
                                       (vdir (v- (v 0 0 0) (camera-o cam))))
                                 (camera-normalize cam)
                                 (redraw)
                                 (setf x0 x)
                                 (setf y0 y)))))))

  (set-coords layout 0 20 350 450)

  (show frame))