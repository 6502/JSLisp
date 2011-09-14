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

(defvar *faces* (list))

(dolist (i '(-1 0 1))
  (dolist (j '(-1 0 1))
    (push (list "#0000FF"
                (v (- i 0.5) (- j 0.5) -1.5)
                (v (+ i 0.5) (- j 0.5) -1.5)
                (v (+ i 0.5) (+ j 0.5) -1.5)
                (v (- i 0.5) (+ j 0.5) -1.5)) *faces*)

    (push (list "#FFFF00"
                (v (- i 0.5) (- j 0.5) 1.5)
                (v (+ i 0.5) (- j 0.5) 1.5)
                (v (+ i 0.5) (+ j 0.5) 1.5)
                (v (- i 0.5) (+ j 0.5) 1.5)) *faces*)

    (push (list "#00FF00"
                (v (- i 0.5) -1.5 (- j 0.5))
                (v (+ i 0.5) -1.5 (- j 0.5))
                (v (+ i 0.5) -1.5 (+ j 0.5))
                (v (- i 0.5) -1.5 (+ j 0.5))) *faces*)

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
                (v 1.5 (- i 0.5) (- j 0.5))
                (v 1.5 (+ i 0.5) (- j 0.5))
                (v 1.5 (+ i 0.5) (+ j 0.5))
                (v 1.5 (- i 0.5) (+ j 0.5))) *faces*)))

(let* ((canvas (create-element "canvas"))
       (layout (:Hdiv canvas))
       (cb null)
       (frame (window 100 100 200 300
                      :title "3d view"
                      :close (lambda () (clear-interval cb))
                      :layout layout))
       (from (v -400 -600 -1000)))
  (append-child frame canvas)

  (setf cb (set-interval (lambda ()
                           (let ((w (. canvas offsetWidth))
                                 (h (. canvas offsetHeight)))
                             (when (or (/= w (. canvas width))
                                       (/= h (. canvas height)))
                               (let ((ctx (funcall (. canvas getContext) "2d"))
                                     (cam (camera from (v 0 0 0) (v 0 1 0) 800))
                                     (zx (/ (. canvas width) 2))
                                     (zy (/ (. canvas height) 2)))
                                 (setf (. canvas width) w)
                                 (setf (. canvas height) h)
                                 (setf (. ctx fillStyle) "#808080")
                                 (funcall (. ctx fillRect) 0 0 w h)
                                 (setf (. ctx strokeStyle) "#000000")
                                 (setf (. ctx lineWidth) 1)
                                 (labels ((moveTo (p)
                                            (let ((sp (funcall cam p)))
                                              (funcall (. ctx moveTo) (+ zx (x sp)) (+ zy (y sp)))))
                                          (lineTo (p)
                                            (let ((sp (funcall cam p)))
                                              (funcall (. ctx lineTo) (+ zx (x sp)) (+ zy (y sp)))))
                                          (zdist (f)
                                            (max (map #'z (map cam f)))))
                                   (dolist (f (sort *faces*
                                                    (lambda (a b) (< (zdist a) (zdist b)))))
                                     (setf (. ctx fillStyle) (first f))
                                     (funcall (. ctx beginPath))
                                     (moveTo (v* (second f) 100))
                                     (dolist (p (slice f 2))
                                       (lineTo (v* p 100)))
                                     (funcall (. ctx closePath))
                                     (funcall (. ctx fill))
                                     (funcall (. ctx stroke))))))))
                         100))

  (set-coords layout 100 100 200 300)

  (show frame))