(deftuple xy (x y))

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

(defun xy/ (a k)
  "Inverse scaling of an xy vector [a] by a scalar [k]"
  (xy (/ a.x k)
      (/ a.y k)))

(defun xy-cross (a b)
  "Cross-product of two xy vectors [a] and [b] as a scalar"
  (- (* a.x b.y)
     (* a.y b.x)))

(defun xy-dot (a b)
  "Dot-product of two xy vectors [a] and [b]"
  (+ (* a.x b.x)
     (* a.y b.y)))

(defun xy-abs2 (p)
  "Squared length of an xy vector [p]"
  (xy-dot p p))

(defun xy-abs (p)
  "Lenght of an xy vector [p]"
  (sqrt (xy-dot p p)))

(defun xy-dist2 (a b)
  "Squared distance between two xy vectors [a] and [b]"
  (xy-abs2 (xy- a b)))

(defun xy-dist (a b)
  "Distance between two xy vectors [a] and [b]"
  (xy-abs (xy- a b)))

(defun xy-avg (&rest pts)
  "Average of vectors [pts]"
  (xy/ (apply #'xy+ pts) (length pts)))

(defun xy-ortho (a)
  "Left orthogonal of an xy vector [a]"
  (xy (- a.y) a.x))

(defun xy-norm (a)
  "Normalized xy vector [a]"
  (xy* a (/ (xy-abs a))))

(defun xy-arg (p)
  "Angle between xy vector [p] and +X"
  (atan2 p.y p.x))

(defun xy-from-polar (rho theta)
  "Build an xy vector from polar coordinates [rho] and [theta]"
  (xy (* rho (cos theta))
      (* rho (sin theta))))

(export xy xy+ xy- xy* xy/
        xy-cross xy-dot
        xy-abs2 xy-abs xy-dist2 xy-dist
        xy-arg xy-from-polar
        xy-avg xy-ortho xy-norm)