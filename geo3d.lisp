(defun v (&rest coords) coords)

(defun x (p) (first p))
(defun y (p) (second p))
(defun z (p) (third p))

(defun v+ (&rest pts)
  (reduce (lambda (a b) (map #'+ a b)) pts))

(defun v- (&rest pts)
  (reduce (lambda (a b) (map #'- a b)) pts))

(defun v* (v k)
  (map (lambda (x) (* x k)) v))

(defun v/ (v k)
  (map (lambda (x) (/ x k)) v))

(defun vdot (a b)
  (reduce #'+ (map #'* a b)))

(defun vlen (x)
  (sqrt (vdot x x)))

(defun vdir (x)
  (v/ x (vlen x)))

(defun v^ (a b)
  (v (- (* (y a) (z b)) (* (z a) (y b)))
     (- (* (z a) (x b)) (* (x a) (z b)))
     (- (* (x a) (y b)) (* (y a) (x b)))))

(defobject camera (o u v n))

(defun camera (from to up dist)
  (let* ((n (vdir (v- to from)))
         (u (v* (vdir (v^ up n)) dist))
         (v (v^ n u)))
    (make-camera o: from
                 n: n
                 u: u
                 v: v)))

(defun camera-map (camera p)
  (let* ((x (v- p camera.o))
         (z (vdot x camera.n))
         (zs (/ z))
         (xs (* (vdot x camera.u) zs))
         (ys (* (vdot x camera.v) zs)))
    (v xs ys zs)))

(defun camera-invmap (camera xs ys)
  (let ((dist (vlen camera.u)))
    (v+ camera.o
        (v* camera.u (/ xs dist))
        (v* camera.v (/ ys dist))
        (v* camera.n dist))))

(defun camera-normalize (camera &optional (dist (vlen camera.u)))
  (let ((n camera.n)
        (u camera.u))
    (setf camera.n (vdir n))
    (setf u (v- u (v* n (vdot u n))))
    (setf camera.u (v* (vdir u) dist))
    (setf camera.v
          (v^ camera.n
              camera.u))))

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

(export v x y z
        v+ v- v* v/ vdot vlen vdir v^
        camera "camera-" "set-camera-"
        camera-map camera-invmap camera-normalize
        xrot yrot zrot
        xform)