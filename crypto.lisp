(defun hash (s)
  "32-bit hash of a string"
  (let ((hash 5381))
    (dotimes (n 2)
      (dolist (x (map #'char-code s))
        (setf hash (logior (+ (* hash 33) x) 0))))
    hash))

(export hash)