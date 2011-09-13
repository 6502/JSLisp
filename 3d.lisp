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

(defun camera (from to up dist)
  (let* ((n (vdir (v- to from)))
         (u (v* (vdir (v^ up n)) dist))
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
       (cb null)
       (frame (window 100 100 200 300
                      :title "3d view"
                      :close (lambda () (clear-interval cb))
                      :layout layout))
       (from (v -200 -500 -1000)))
  (append-child frame canvas)

  (setf cb
        (set-interval (lambda ()
                        (let ((w (. canvas offsetWidth))
                              (h (. canvas offsetHeight))
                              (ctx (funcall (. canvas getContext) "2d"))
                              (cam (camera from (v 0 0 0) (v 0 1 0) 800))
                              (zx (/ (. canvas width) 2))
                              (zy (/ (. canvas height) 2)))
                          (setf (. canvas width) w)
                          (setf (. canvas height) h)
                          (setf (. ctx fillStyle) "#000000")
                          (funcall (. ctx fillRect) 0 0 w h)
                          (setf (. ctx strokeStyle) "#00FF00")
                          (setf (. ctx lineWidth) 2)
                          (labels ((moveTo (p)
                                     (let ((sp (funcall cam p)))
                                       (funcall (. ctx moveTo) (+ zx (x sp)) (+ zy (y sp)))))
                                   (lineTo (p)
                                     (let ((sp (funcall cam p)))
                                       (funcall (. ctx lineTo) (+ zx (x sp)) (+ zy (y sp))))))
                            (funcall (. ctx beginPath))
                            (dolist (i (range -10 11))
                              (moveTo (v (* i 50) 0 -500))
                              (lineTo (v (* i 50) 0 500))
                              (moveTo (v -500 0 (* i 50)))
                              (lineTo (v 500 0 (* i 50))))
                            (funcall (. ctx stroke)))))
                      100))

  (set-coords layout 100 100 200 300)

  (show frame))