(defobject xy (x y))

;; Allows an xy instance to be considered as a list of two elements for apply
(defgetter xy \0 this.x)
(defsetter xy \0 (setf this.x value))
(defgetter xy \1 this.y)
(defsetter xy \1 (setf this.y value))
(defgetter xy length 2)

(defmacro/f xy (x y)
  "A shortcut for (new-xy x y)"
  `(new-xy ,x ,y))

(defun xy+ (&rest args)
  "Addition of xy vectors"
  (let ((xx 0)
        (yy 0))
    (dolist (p args)
      (incf xx p.x)
      (incf yy p.y))
    (xy xx yy)))

(defun xy- (a &optional b)
  "Difference or negation of xy vectors"
  (if b
      (xy (- a.x b.x) (- a.y b.y))
      (xy (- a.x) (- a.y))))

(defun xy* (a k)
  "Scaling of an xy vector [a] by a scalar [k]"
  (xy (* a.x k)
      (* a.y k)))

(defun xy-cross (a b)
  "Cross-product of two xy vectors [a] and [b] as a scalar"
  (- (* a.x b.y)
     (* a.y b.x)))

(defun xy-dot (a b)
  "Dot-product of two xy vectors [a] and [b]"
  (+ (* a.x b.x)
     (* a.y b.y)))

(defun xy-len2 (p)
  "Squared length of an xy vector [p]"
  (xy-dot p p))

(defun xy-len (p)
  "Lenght of an xy vector [p]"
  (sqrt (xy-dot p p)))

(defun xy-dist2 (a b)
  "Squared distance between two xy vectors [a] and [b]"
  (xy-len2 (xy- a b)))

(defun xy-dist (a b)
  "Distance between two xy vectors [a] and [b]"
  (xy-len (xy- a b)))

(defun xy-avg (a b)
  "Average of two xy vectors [a] and [b]"
  (xy* (xy+ a b) 0.5))

(defun xy-ortho (a)
  "Left orthogonal of an xy vector [a]"
  (xy (- a.y) a.x))

(defun xy-norm (a)
  "Normalized xy vector [a]"
  (xy* a (/ (xy-len a))))

(defun xy-arg (p)
  "Angle between xy vector [p] and +X"
  (atan2 p.y p.x))

(defun xy-from-polar (rho theta)
  "Build an xy vector from polar coordinates [rho] and [theta]"
  (xy (* rho (cos theta))
      (* rho (sin theta))))

(export xy xy+ xy- xy*
        xy-cross xy-dot
        xy-len2 xy-len xy-dist2 xy-dist
        xy-arg xy-from-polar
        xy-avg xy-ortho xy-norm)