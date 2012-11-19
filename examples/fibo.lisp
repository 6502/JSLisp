(defun fibo (x)
  "Returns [n]-th Fibonacci number"
  (if (< x 2)
      1
      (+ (fibo (- x 1))
         (fibo (- x 2)))))

(defun test-fibo (n)
  "Displays first [n] Fibonacci numbers"
  (dotimes (i n)
    (display ~"(fibo {i}) --> {(fibo i)}")))

(display (time (test-fibo 20)))
