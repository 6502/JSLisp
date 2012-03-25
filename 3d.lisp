(import gui)
(import geo3d)

(defun build-faces ()
  (let ((faces (list)))
    (dolist (i '(-1 0 1))
      (dolist (j '(-1 0 1))
        (push (list (v 1 1 1)
                    (v (- i 0.5) (- j 0.5) -1.5)
                    (v (+ i 0.5) (- j 0.5) -1.5)
                    (v (+ i 0.5) (+ j 0.5) -1.5)
                    (v (- i 0.5) (+ j 0.5) -1.5)) faces)

        (push (list (v 0 0 1)
                    (v (+ i 0.5) (- j 0.5) 1.5)
                    (v (- i 0.5) (- j 0.5) 1.5)
                    (v (- i 0.5) (+ j 0.5) 1.5)
                    (v (+ i 0.5) (+ j 0.5) 1.5)) faces)

        (push (list (v 1 0 0)
                    (v (+ i 0.5) -1.5 (- j 0.5))
                    (v (- i 0.5) -1.5 (- j 0.5))
                    (v (- i 0.5) -1.5 (+ j 0.5))
                    (v (+ i 0.5) -1.5 (+ j 0.5))) faces)

        (push (list (v 0 0.7 0)
                    (v (- i 0.5) 1.5 (- j 0.5))
                    (v (+ i 0.5) 1.5 (- j 0.5))
                    (v (+ i 0.5) 1.5 (+ j 0.5))
                    (v (- i 0.5) 1.5 (+ j 0.5))) faces)

        (push (list (v 1 1 0)
                    (v -1.5 (- i 0.5) (- j 0.5))
                    (v -1.5 (+ i 0.5) (- j 0.5))
                    (v -1.5 (+ i 0.5) (+ j 0.5))
                    (v -1.5 (- i 0.5) (+ j 0.5))) faces)

        (push (list (v 0.9 0.65 0)
                    (v 1.5 (+ i 0.5) (- j 0.5))
                    (v 1.5 (- i 0.5) (- j 0.5))
                    (v 1.5 (- i 0.5) (+ j 0.5))
                    (v 1.5 (+ i 0.5) (+ j 0.5))) faces)))

    (dolist (f faces)
      (dotimes (i (1- (length f)))
        (setf (aref f (1+ i))
              (v* (aref f (1+ i)) 100))))

    faces))

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

(defvar *running-animation* false)

(defun animate (section angle duration redraw faces)
  (unless *running-animation*
    (let ((faces (filter (lambda (f) (face-in-section f section))
                         faces))
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
                  (if (< ci 1)
                      (set-timeout f 10)
                      (setf *running-animation* false)))))
      (set-timeout f 10)
      (setf *running-animation* true))))

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
       (faces (build-faces))
       (frame (window 100 100 350 450
                      :title "3d view"
                      :close true
                      :resize true
                      :client canvas))
       (cam (camera (v -400 -600 -1000) (v 0 0 0) (v 0 1 0) 800))
       (marks (list)))
  (labels ((visible-faces ()
             (let ((xfaces (map (lambda (f)
                                  (let ((xp (map (lambda (p) (camera-map cam p))
                                                 (slice f 1))))
                                    (list (apply #'max (map #'z xp))
                                          (first f)
                                          xp
                                          f)))
                                (filter (lambda (f)
                                          (> 0
                                             (v. (v- (camera-o cam) (third f))
                                                 (v^ (v- (third f) (second f))
                                                     (v- (fourth f) (third f))))))
                                        faces))))
               (nsort xfaces (lambda (a b) (< (first a) (first b))))
               xfaces))
           (redraw ()
             (let* ((ctx (funcall (. canvas getContext) "2d"))
                    (w (. canvas width))
                    (h (. canvas height))
                    (zx (/ w 2))
                    (zy (/ h 2)))
               (setf (. ctx fillStyle) "#000000")
               (funcall (. ctx fillRect) 0 0 w h)
               (setf (. ctx strokeStyle) "#000000")
               (setf (. ctx lineWidth) 1)
               (dolist (xf (visible-faces))
                 (let ((r (first (second xf)))
                       (g (second (second xf)))
                       (b (third (second xf)))
                       (k (v. (vdir (v^ (v- (third (fourth xf)) (second (fourth xf)))
                                        (v- (fourth (fourth xf)) (third (fourth xf)))))
                              (vdir (camera-n cam)))))
                   (setf k (+ 0.5 (* 0.5 k k)))
                   (setf (. ctx fillStyle)
                         ~"rgb({(floor (* 255 r k))},{(floor (* 255 g k))},{(floor (* 255 b k))})"))
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
    (set-style (window-frame frame)
               backgroundColor "#000000")
    (setf (window-resize-cback frame)
          (lambda (x0 y0 x1 y1)
            (setf (. canvas width) (- x1 x0))
            (setf (. canvas height) (- y1 y0))
            (set-style canvas
                       px/left x0
                       px/top y0
                       px/width (- x1 x0)
                       px/height (- y1 y0))
            (redraw)))
    (set-handler canvas onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (let ((x0 (first (event-pos event)))
                       (y0 (second (event-pos event)))
                       (cx (first (element-pos canvas)))
                       (cy (second (element-pos canvas)))
                       (w (. canvas width))
                       (h (. canvas height))
                       (found false))
                   (dolist (xf (visible-faces))
                     (when (and (not found)
                                (inside (v (- x0 cx (/ w 2))
                                           (- y0 cy (/ h 2)))
                                        (third xf)))
                       (setf found true)
                       (let ((hp (hit3d (camera-o cam)
                                        (camera-invmap cam
                                                       (- x0 cx (/ w 2))
                                                       (- y0 cy (/ h 2)))
                                        (fourth xf)))
                             (sections (filter (lambda (s) (face-in-section (fourth xf) s))
                                               *sections*))
                             (fired false))
                         (tracking (lambda (x y)
                                     (let ((dx (- x x0))
                                           (dy (- y y0)))
                                       (when (and (not fired)
                                                  (> (+ (* dx dx) (* dy dy)) (* 8 8)))
                                         (setf fired true)
                                         (let ((best null))
                                           (dolist (s sections)
                                             (let ((xr (cond
                                                         ((= (x (section-n s)) 1) #'xrot)
                                                         ((= (y (section-n s)) 1) #'yrot)
                                                         (true #'zrot))))
                                               (dolist (aa '(-1 1))
                                                 (let* ((xhp (camera-map cam (xform (funcall xr (/ aa 100)) hp)))
                                                        (ddx (- (x xhp) (- x0 cx (/ w 2))))
                                                        (ddy (- (y xhp) (- y0 cy (/ h 2))))
                                                        (m (+ (* dx ddx) (* dy ddy))))
                                                   (when (or (null? best)
                                                             (> m (first best)))
                                                     (setf best (list m s aa)))))))
                                           (animate (second best)
                                                    (* (/ pi 2) (third best))
                                                    500
                                                    #'redraw
                                                    faces)))))))))
                   (unless found
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
                                   (setf y0 y))))))))
  (show-window frame))
