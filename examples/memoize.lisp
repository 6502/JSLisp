(defun memoize (f)
  "Returns a memoized version of function [f]"
  (let ((cache #()))
    (lambda (&rest args)
      (let ((result (aref cache args)))
        (first (or result
                   (setf (aref cache args)
                         (list (apply f args)))))))))

(defun fibo (n)
  "[n]-th Fibonacci number"
  (if (< n 2)
      1
      (+ (fibo (- n 1))
         (fibo (- n 2)))))

(defmacro show (x)
  `(display ~"{(str-value ',x)} --> {,x}"))

(display "--- before memoization ---")
(show (time (fibo 34)))
(show (fibo 34))
(setf #'fibo (memoize #'fibo))
(display "--- after memoization ---")
(show (time (fibo 34)))
(show (fibo 34))
(show (time (fibo 100)))
(show (fibo 100))
