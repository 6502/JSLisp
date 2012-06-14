(defconstant +BASE+ 10000000)

(defun L+ (a b)
  "Sum of two large integers [a] and [b]"
  (do ((result (list))
       (carry 0)
       (na (length a))
       (nb (length b))
       (i 0 (1+ i)))
      ((and (>= i na) (>= i nb) (= carry 0))
       result)
    (let ((x (+ (or (aref a i) 0)
                (or (aref b i) 0)
                carry)))
      (if (>= x +BASE+)
          (progn
            (setf carry 1)
            (setf (aref result i) (- x +BASE+)))
          (progn
            (setf carry 0)
            (setf (aref result i) x))))))

(defun Lk* (a k)
  "Product of a large integer [a] for a small integer [k]"
  (let ((result (list))
        (carry 0))
    (dolist (v a)
      (let ((x (+ (* v k) carry)))
        (push (% x +BASE+) result)
        (setf carry (floor (/ x +BASE+)))))
    (when carry
      (push carry result))
    result))

(defun L* (a b)
  "Product of two large integers [a] and [b]"
  (let ((result (list)))
    (dolist (k b)
      (setf result (L+ result (Lk* a k)))
      (setf a (append '(0) a)))
    result))

(defun Lmat2* (m1 m2)
  "Product of two 2x2 matrices"
  ;; a b    e f
  ;; c d    g h
  (let ((a (first m1))
        (b (second m1))
        (c (third m1))
        (d (fourth m1))
        (e (first m2))
        (f (second m2))
        (g (third m2))
        (h (fourth m2)))
    (list (L+ (L* a e) (L* b g))
          (L+ (L* a f) (L* b h))
          (L+ (L* c e) (L* d g))
          (L+ (L* c f) (L* d h)))))

(defun Lmat2exp (m exp)
  "Raises 2x2 matrix [m] to the [exp] power"
  (cond
    ((= exp 0) (list 1 0 0 1))
    ((= exp 1) m)
    ((% exp 2) (Lmat2* m (Lmat2exp m (1- exp))))
    (true (let ((h (Lmat2exp m (ash exp -1))))
            (Lmat2* h h)))))

(defun Lstr (n)
  "Converts a large integer to a string"
  (let ((x (apply #'+ (map (lambda (x)
                             (let ((s ~"00000000{x}"))
                               (slice s (- (length s) 7))))
                           (reverse n)))))
    (replace x "^0*(.)" "$1")))

(defun Lfibo (n)
  "Computes the [n]-th fibonacci number (result as a string)"
  (Lstr (first (Lmat2exp '((1)(1)
                           (1)(0))
                         n))))
