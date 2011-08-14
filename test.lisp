;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                          ;;;
;;;  Copyright (c) 2011 by Andrea Griffini                                   ;;;
;;;                                                                          ;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining   ;;;
;;;  a copy of this software and associated documentation files (the         ;;;
;;;  "Software"), to deal in the Software without restriction, including     ;;;
;;;  without limitation the rights to use, copy, modify, merge, publish,     ;;;
;;;  distribute, sublicense, and/or sell copies of the Software, and to      ;;;
;;;  permit persons to whom the Software is furnished to do so, subject to   ;;;
;;;  the following conditions:                                               ;;;
;;;                                                                          ;;;
;;;  The above copyright notice and this permission notice shall be          ;;;
;;;  included in all copies or substantial portions of the Software.         ;;;
;;;                                                                          ;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         ;;;
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      ;;;
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                   ;;;
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE  ;;;
;;;  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION  ;;;
;;;  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION   ;;;
;;;  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(setq test-passed 0)
(setq test-total 0)
(setq test-start (clock))

(let ((result (gensym)))
  (defmacro test (form expected-result)
    `(progn
       (incf test-total)
       (try (let ((,result (str-value ,form)))
              (when (/= ,result ,expected-result)
                (error (+ "Wrong result: ("
                          ,result
                          " instead of "
                          ,expected-result
                          ")")))
              (incf test-passed))
            (progn
              (display (+ "Test #" test-total " ** FAILED **"))
              (display ,(str-value form))
              (display (+ "ERROR: " *exception*))
              (display ""))))))

(test (mangle "foo")
      "\"$$foo\"")

(test (mangle "foo1")
      "\"$$foo$49$\"")

(test (mangle "foo-1")
      "\"$$foo_$49$\"")

(test (mangle "foo_1")
      "\"$$foo$95$$49$\"")

(test (demangle "$$foo")
      "\"foo\"")

(test (demangle "$$foo$49$")
      "\"foo1\"")

(test (demangle "$$foo_$49$")
      "\"foo-1\"")

(test (demangle "$$foo$95$$49$")
      "\"foo_1\"")

(test (intern "xx")
      "xx")

(test (= (intern ":xx") (intern ":xx"))
      "true")

(test (symbolp (intern "xx"))
      "true")

(test (= (symbol-value :xx) :xx)
      "true")

(test (listp (list))
      "true")

(test (listp 12)
      "false")

(test (listp undefined)
      "false")

(test (symbolp 'x)
      "true")

(test (symbolp :x)
      "true")

(test (symbolp 12)
      "false")

(test (symbolp "foo")
      "false")

(test (= '|foo| 'foo)
      "true")

(test (= '|foo bar| 'foo\ bar)
      "true")

(test (symbolp null)
      "false")

(test (symbolp undefined)
      "false")

(test (symbolp true)
      "false")

(test (symbolp false)
      "false")

(test (symbolp NaN)
      "false")

(test (numberp null)
      "false")

(test (numberp undefined)
      "false")

(test (numberp true)
      "false")

(test (numberp false)
      "false")

(test (numberp NaN)
      "true")

(test (js-eval "1+1")
      "2")

(test (symbol-function 'foo)
      "undefined")

(test (symbol-function 'listp)
      "#CODE")

(test (set-symbol-function 'foo (lambda (x) (* x x)))
      "#CODE")

(test (symbol-function 'foo)
      "#CODE")

(test (foo 12)
      "144")

(test (symbol-value 'foo)
      "undefined")

(test (set-symbol-value 'foo 42)
      "42")

(test (symbol-value 'foo)
      "42")

(test (symbol-macro 'foo)
      "undefined")

(test (set-symbol-macro 'foo (lambda () 42))
      "#CODE")

(test (symbol-macro 'foo)
      "#CODE")

(test (foo)
      "42")

(test (funcall #'foo 6)
      "36")

(test (if (< 1 2) 1 2)
      "1")

(test (if (> 1 2) 1 2)
      "2")

(test (if (> 1 2) 1)
      "undefined")

(test (defvar *foo* 42)
      "42")

(test (defun bar () *foo*)
      "#CODE")

(test (setq xxx 42)
      "42")

(test (defun xar () xxx)
      "#CODE")

(test (funcall (lambda (*foo*) (bar)) 9)
      "9")

(test (funcall (lambda (xxx) (xar)) 9)
      "42")

(test (let((x 20))(let((x 42)(y x))y))
      "20")

(test (let((x 20))(let*((x 42)(y x))y))
      "42")

(test (let ((a (lambda (x)
                 (lambda (n) (incf x n)))))
        (let ((a10 (funcall a 10))
              (a8 (funcall a 8)))
          (list (funcall a10 1)
                (funcall a8 2)
                (funcall a10 3)
                (funcall a8 4)
                (funcall a10 5)
                (funcall a8 6))))
      "(11 10 14 14 19 20)")

(test (defun Y (f)
        (funcall
         (lambda (x) (funcall x x))
         (lambda (y)
           (funcall
            f (lambda (&rest args)
                (apply (funcall y y) args))))))
      "#CODE")

(test (defun fac (f)
        (lambda (n)
          (if (= 0 n)
              1
              (* n (funcall f (1- n))))))
      "#CODE")

(test (funcall (Y #'fac) 10)
      "3628800")

(test (logcount 0)
      "0")

(test (logcount 65537)
      "2")

(test (logcount 65535)
      "16")

(test (defmacro square (x) `(* ,x ,x))
      "#CODE")

(test (let ((x 2)) (square (incf x)))
      "12")

(test (list
       (labels ((square (x)
                  (if (< x 2)
                      1
                      (* x (square (1- x))))))
         (square 12))
       (square 12))
      "(479001600 144)")

(test (defun xfoo () 1)
      "#CODE")

(test (labels ((xfoo () 2))
        (list (funcall #'xfoo)
              (funcall (symbol-function
                        'xfoo))))
      "(2 1)")

(test (let ((f 1))
        (dotimes (i 10)
          (setf f (* f (1+ i))))
        f)
      "3628800")

(test (let ((L (list)))
        (labels ((add () (push *foo* L)))
          (dotimes (*foo* 10)
            (add)))
        L)
      "(0 1 2 3 4 5 6 7 8 9)")

(test (let ((f 1))
        (dolist (i (range 10))
          (setf f (* f (1+ i))))
        f)
      "3628800")

(test (let ((L (list)))
        (labels ((add () (push *foo* L)))
          (dolist (*foo* (range 10))
            (add)))
        L)
      "(0 1 2 3 4 5 6 7 8 9)")

(test (let ((x 20))
        (list (let ((x 30))
                (setq x 40)
                x) x))
      "(40 20)")

(test (eval '(+ 1 2 3))
      "6")

(test (setq global-x 99)
      "99")

(test (let ((global-x 42))
        (eval 'global-x))
      "99")

(test (append (list 1 2 3)
              (list 4 5)
              (list)
              (list 6))
      "(1 2 3 4 5 6)")

(test (apply #'+ (list 1 2 3))
      "6")

(test (let ((res (list)))
        (labels ((foo ()
                   (push 1 res)
                   true)
                 (bar ()
                   (push 2 res)
                   false))
          (push (and (foo) (bar)) res)
          (push 0 res)
          (push (and (bar) (foo)) res)
          (push 0 res)
          (push (or (foo) (bar)) res)
          (push 0 res)
          (push (or (bar) (foo)) res))
        res)
      "(1 2 false 0 2 false 0 1 true 0 2 1 true)")

(test (map (lambda (x)
             (cond
               ((= null x) 'C)
               ((< x 1) 'A)
               ((< x 2) 'B)
               ((= x undefined) 'D)
               (true 'E)))
           (list 0 1 2 null undefined))
      "(A B E C C)")

(test (list (when (< 1 2) 1)
            (when (> 1 2) 2)
            (unless (< 1 2) 1)
            (unless (> 1 2) 2))
      "(1 null null 2)")

(test (defvar *x* 0)
      "0")

(test (labels
          ((bar ()
             (* *x* (incf *x*))))
        (reduce #'+ (do ((res (list))
                         (x 0 (1+ x))
                         (y 0 (+ y 2))
                         (z 9)
                         (*x* 13 (1- *x*)))
                        ((> x 10) res)
                      (push x res)
                      (push y res)
                      (push (bar) res)
                      (push (incf z) res))))
      "2332")

(test (defmacro foo (x) `(* ,x ,x))
      "#CODE")

(test (let ((x 3))
        (list
         (foo (incf x))
         (macrolet ((foo (x)
                      (let ((xv (gensym)))
                        `(let ((,xv ,x))
                           (* ,xv ,xv)))))
           (foo (incf x)))))
      "(20 36)")

(test (let ((res (list)))
        (symbol-macrolet ((x (progn (push 1 res) 12)))
          (push (list x x x
                      (let ((x 20))
                        x)) res))
        res)
      "(1 1 1 (12 12 12 20))")

(let ((pairs (list (list 'listp '(list))
                   (list 'zerop '0)
                   (list 'NaNp 'NaN)
                   (list 'undefinedp 'undefined)
                   (list 'nullp 'null)
                   (list 'stringp '"")
                   (list 'boolp 'false)
                   (list 'objectp '(js-object))))
      (errors 0))
  (dotimes (i (length pairs))
    (dotimes (j (length pairs))
      (let ((tester (first (aref pairs i)))
            (value (second (aref pairs j))))
        (eval `(test (funcall (symbol-function ',tester) ,value)
                     ,(str-value (= i j))))))))

(display (+ test-passed "/" test-total
            " tests passed in "
            (- (clock) test-start) "ms"))
