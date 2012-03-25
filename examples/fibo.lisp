(defun fibo (x)
  (if (< x 2)
      1
      (+ (fibo (- x 1))
         (fibo (- x 2)))))

(defun test-fibo (n)
  (dotimes (i n)
    (display ~"(fibo {i}) --> {(fibo i)}")))

(display (time (test-fibo 20)))
