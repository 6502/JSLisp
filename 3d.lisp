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

(defun camera (from to up dist scale)
  (let* ((n (vdir (v- to from)))
         (u (* scale (vdir (v^ up n))))
         (v (v^ n u)))
    (lambda (p)
      (let* ((x (v- p from))
             (z (v. x n))
             (zs (/ z))
             (xs (* (v. x u) zs))
             (ys (* (v. x v) zs)))
        (v xs ys zs)))))

(load (http-get "gui.lisp"))

(let* ((canvas (create-element "canvas"))
       (layout (:Hdiv canvas))
       (frame (window 100 100 200 300
                      :title "3d view"
                      :close (lambda ())
                      :layout layout))
       (from (v -100 -100 -1000)))
  (append-child frame canvas)

  (set-interval (lambda ()
                  (let ((w (. canvas offsetWidth))
                        (h (. canvas offsetHeight))
                        (ctx (funcall (. canvas getContext) "2d"))
                        (cam (camera from (v 0 0 0) (v 0 1 0) 100 1))
                        (zx (/ (. canvas width) 2))
                        (zy (/ (. canvas height) 2)))
                    (setf (. canvas width) w)
                    (setf (. canvas height) h)
                    (setf (. ctx fillStyle) "#000000")
                    (funcall (. ctx fillRect) 0 0 w h)
                    (setf (. ctx strokeStyle) "#FFFFFF")
                    (setf (. ctx lineWidth) 2)
                    (labels ((moveTo (p)
                               (let ((sp (funcall cam p)))
                                 (funcall (. ctx moveTo) (+ zx (x sp)) (+ zy (y sp)))))
                             (lineTo (p)
                               (let ((sp (funcall cam p)))
                                 (funcall (. ctx lineTo) (+ zx (x sp)) (+ zy (y sp))))))
                      (dolist (i (range -10 11))
                        (moveTo (v (* i 10) 0 -100))
                        (lineTo (v (* i 10) 0 100))
                        (funcall (. ctx stroke))))))
                100)

  (set-coords layout 100 100 200 300)

  (show frame))