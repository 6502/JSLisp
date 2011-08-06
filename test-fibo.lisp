(defun fibo (x)
    (if (< x 2)
        1
        (+ (fibo (- x 1))
           (fibo (- x 2)))))
(defun test-fibo (n)
    (dotimes (i n)
        (display (+ "(fibo " i ") --> " (fibo i)))))
(defun clock () (js-code "(new Date).getTime()"))
(defmacro time (&rest body)
    (let ((start (gensym)))
        `(let ((,start (clock)))
            ,@body
            (- (clock) ,start))))
(time (test-fibo 30))

(defun fibo (n)
    ; f(n) = f(n-1) + f(n-2) = a + b
    ; f(n+1) = f(n) + f(n-1) = a' + b'
    ; a' = a+b
    ; b' = a
    (labels ((ff (a b count)
                (if (= count n)
                    (+ a b)
                    (ff (+ a b) a (1+ count)))))
        (if (< n 2)
            1
            (ff 1 1 2))))

(defun memoized (f)
  (let ((cache (js-object)))
    (lambda (&rest x)
      (let ((cached-result (aref cache x)))
        (if (= cached-result undefined)
            (setf (aref cache x)
                  (apply f x))
            cached-result)))))
