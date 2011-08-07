(defmacro test (form expected-result)
  (let ((result (gensym)))
    `(let ((,result (str-value ,form)))
       (if (= ,result ,expected-result)
           ,(+ "Test " (str-value form) " *PASSED*")
           (error (+ ,(+ "TEST FAILED: " (str-value form) " --> " )
                     (str-value ,result)
                     ,(+ " instead of " (str-value expected-result))))))))

(test (mangle "foo")                      "\"$$foo\"")
(test (mangle "foo1")                     "\"$$foo$49$\"")
(test (mangle "foo-1")                    "\"$$foo_$49$\"")
(test (mangle "foo_1")                    "\"$$foo$95$$49$\"")
(test (demangle "$$foo")                  "\"foo\"")
(test (demangle "$$foo$49$")              "\"foo1\"")
(test (demangle "$$foo_$49$")             "\"foo-1\"")
(test (demangle "$$foo$95$$49$")          "\"foo_1\"")

(test (intern "xx")                       "xx")
(test (= (intern ":xx") (intern ":xx"))   "true")
(test (symbolp (intern "xx"))             "true")
(test (= (symbol-value :xx) :xx)          "true")

(test (listp (list))                      "true")
(test (listp 12)                          "false")
(test (listp undefined)                   "false")

(test (symbolp 'x)                        "true")
(test (symbolp :x)                        "true")
(test (symbolp 12)                        "false")
(test (symbolp "foo")                     "false")
(test (= '|foo| 'foo)                     "true")
(test (= '|foo bar| 'foo\ bar)            "true")

(test (symbolp null)                      "false")
(test (symbolp undefined)                 "false")
(test (symbolp true)                      "false")
(test (symbolp false)                     "false")
(test (symbolp NaN)                       "false")
(test (numberp null)                      "false")
(test (numberp undefined)                 "false")
(test (numberp true)                      "false")
(test (numberp false)                     "false")
(test (numberp NaN)                       "true")

(test (js-eval "1+1")                     "2")
(test (symbol-function 'foo)              "undefined")
(test (symbol-function 'listp)            "#CODE")

(set-symbol-function 'foo (lambda (x) (* x x)))
(test (symbol-function 'foo)              "#CODE")
(test (foo 12)                            "144")

(test (symbol-value 'foo)                 "undefined")
(test (set-symbol-value 'foo 42)          "42")
(test (symbol-value 'foo)                 "42")

(test (symbol-macro 'foo)                 "undefined")
(set-symbol-macro 'foo (lambda () 42))
(test (symbol-macro 'foo)                 "#CODE")
(test (foo)                               "42")
(test (funcall #'foo 6)                   "36")

(test (if (< 1 2) 1 2)                    "1")
(test (if (> 1 2) 1 2)                    "2")
(test (if (> 1 2) 1)                      "undefined")

(defvar *foo* 42)
(defun bar () *foo*)
(setq xxx 42)
(defun xar () xxx)
(test (funcall (lambda (*foo*) (bar)) 9)  "9")
(test (funcall (lambda (xxx) (xar)) 9)    "42")

(test (let((x 20))(let((x 42)(y x))y))    "20")
(test (let((x 20))(let*((x 42)(y x))y))   "42")

(test (let ((a (lambda (x)
                 (lambda (n) (incf x n)))))
        (let ((a10 (funcall a 10))
              (a8 (funcall a 8)))
          (list (funcall a10 1)
                (funcall a8 2)
                (funcall a10 3)
                (funcall a8 4)
                (funcall a10 5)
                (funcall a8 6))))         "(11 10 14 14 19 20)")

(defun Y (f)
  (funcall
   (lambda (x) (funcall x x))
   (lambda (y)
     (funcall
      f (lambda (&rest args)
          (apply (funcall y y) args))))))

(defun fac (f)
   (lambda (n)
      (if (= 0 n)
          1
          (* n (funcall f (1- n))))))

(test (funcall (Y #'fac) 10)              "3628800")

(test (logcount 0)                        "0")
(test (logcount 65537)                    "2")
(test (logcount 65535)                    "16")

(defmacro square (x) `(* ,x ,x))
(test (let ((x 2)) (square (incf x)))     "12")
(test (list
       (labels ((square (x)
                  (if (< x 2)
                      1
                      (* x (square (1- x))))))
         (square 12))
       (square 12))                       "(479001600 144)")
