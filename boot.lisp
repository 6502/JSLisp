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

(set-symbol-macro 'defmacro
                  (lambda (name args &rest body)
                    (let ((doc (str-value (append (list name) args))))
                      (if (stringp (js-code "d$$body[0]"))
                          (setq doc (js-code "d$$doc+\"\\n\"+d$$body.splice(0,1)[0]")))
                      (list 'progn
                            (list 'set-symbol-macro
                                  (list 'quote name)
                                  (append (list 'lambda args) body))
                            (list 'set-documentation
                                  (list 'symbol-macro (list 'quote name))
                                  doc)
                            (list 'set-arglist
                                  (list 'symbol-macro (list 'quote name))
                                  (list 'quote args))
                            (list 'quote name)))))

(defmacro defun (name args &rest body)
  ;; Shut up warnings about unknown function in recursive definitions
  ;; Must not set the function value unconditionally because of the
  ;; double-stage bootstrap of some basic operators (the poor version
  ;; must be available during macro expansion time of the better version)
  (unless (symbol-function name)
    (set-symbol-function name 42))
  (let ((doc (str-value (append (list name) args))))
    (if (stringp (js-code "d$$body[0]"))
        (setq doc (js-code "d$$doc+\"\\n\"+d$$body.splice(0,1)[0]")))
    (list 'progn
          (list 'set-symbol-function
                (list 'quote name)
                (append (list 'lambda args) body))
          (list 'set-documentation
                (list 'symbol-function (list 'quote name))
                doc)
          (list 'set-arglist
                (list 'symbol-function (list 'quote name))
                (list 'quote args))
          (list 'quote name))))
(set-documentation (symbol-macro 'defmacro)
                   "(defmacro name (args) ...)
Defines or redefines a compile-time macro.")

(set-arglist (symbol-macro 'defmacro)
             '(name args &rest body))

(set-documentation (symbol-macro 'defun)
                   "(defun name (args) ...)
Defines or redefines a regular function")
(set-arglist (symbol-macro 'defun)
             '(name args &rest body))

; Length function
(defun length (x)
  "Returns the length of string/list x"
  (js-code "d$$x.length"))

; Simple versions of a few operators needed for bootstrap, they will be redefined
(defun = (a b) (js-code "(d$$a==d$$b)"))
(defun < (a b) (js-code "(d$$a<d$$b)"))
(defun > (a b) (js-code "(d$$a>d$$b)"))
(defun - (a b) (js-code "(d$$a-d$$b)"))
(defun + (&rest args)
  (cond
    ((= (length args) 2) (js-code "(d$$args[0]+d$$args[1])"))
    (true (js-code "(d$$args[0]+f$$$43$.apply(null,d$$args.slice(1)))"))))

; Logical not
(defun not (x)
  "Logical negation of x"
  (js-code "!d$$x"))

(defmacro not (x)
  "Logical negation of x"
  (list 'js-code (+ "(!" (js-compile x) ")")))

; Error throwing
(defun error (x)
  "Throws an error message (doesn't return)"
  (js-code "((function(x){throw new String(x);})(d$$x))"))

; Function accessor
(defmacro function (x)
  "Returns the function currently bound to the /unevaluated/ symbol x (including lexical bindings)"
  (list 'js-code (+ "f" (mangle (symbol-name x)))))

(defmacro set-function (x value)
  "Sets the function currently bound to the /unevaluated/ symbol x (including lexical bindings)"
  (list 'js-code (+ "(f"
                    (mangle (symbol-name x)) "="
                    (js-compile value)
                    ")")))

; Javascript crazyness
(defun boolp (x)
  "True if and only if x is a boolean"
  (js-code "((typeof d$$x)=='boolean')"))

(defun undefinedp (x)
  "True if and only if x is the undefined value"
  (js-code "((typeof d$$x)=='undefined')"))

(defun nullp (x)
  "True if and only if x is the null value"
  (js-code "((typeof d$$x)=='object'&&!d$$x)"))

(defun NaNp (x)
  "True if and only if x is the NaN value"
  (js-code "((typeof d$$x)=='number'&&!d$$x&&!(d$$x==0))"))

(defun objectp (x)
  "True if and only if x is a javascript object"
  (js-code "((d$$x&&d$$x.constructor&&d$$x.constructor==Object)==true)"))

(defun zerop (x)
  "True if and only if x is the number zero"
  (and (numberp x) (= x 0)))

(defmacro aref (x k)
  "Returns the k-th element of a list/string or the value associated to the key k in an object"
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile k) "])")))
(defun aref (x k)
  "Returns the k-th element of a list/string or the value associated to the key k in an object"
  (aref x k))

(defmacro set-aref (x k value)
  "Sets the k-th element of a list or the value associated to the key k in an object"
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile k) "]=" (js-compile value) ")")))

(defun set-aref (x k value)
  "Sets the k-th element of a list or the value associated to the key k in an object"
  (set-aref x k value))

; Funcall macro
(defmacro funcall (f &rest args)
  (let ((res (+ (js-compile f) "("))
        (sep ""))
    (dolist (x args)
      (setq res (+ res sep (js-compile x)))
      (setq sep ","))
    (list 'js-code (+ res ")"))))

; List-related macros (can't be defined before '+')
(defmacro length (x)
  "Returns the length of a list or string object"
  (list 'js-code (+ "(" (js-compile x) ".length)")))

(defmacro list (&rest args)
  "Returns a new fresh list containing the passed values"
  (let ((res "[")
        (sep ""))
    (dolist (x args)
      (setq res (+ res sep (js-compile x)))
      (setq sep ","))
    ;; Note that the following is still the list FUNCTION because the
    ;; macro will be available AFTER this defmacro form is evaluated
    (list 'js-code (+ res "]"))))

(defun push (value list)
  "Adds the specified value to the end of the list"
  (js-code "d$$list.push(d$$value)"))

(defmacro rest (x)
  "Returns a string or list obtained from x by removing first element"
  (list 'js-code (+ "(" (js-compile x) ".slice(1))")))

(defun rest (x)
  "Returns a list or string obtained from x by removing first element"
  (js-code "(d$$x.slice(1))"))

(defmacro splice (x start size)
  "Removes and returns size elements (or all remaining) from a given start point."
  (list 'js-code (+ "("
                    (js-compile x)
                    ".splice("
                    (js-compile start)
                    ","
                    (js-compile size)
                    "))")))

(defun splice (x start size)
  "Removes and returns size elements (or all remaining) from a given start point."
  (js-code "(d$$x.splice(d$$start,d$$size))"))

(defun insert (x i y)
  "Inserts element y into list x at index i"
  (js-code "(d$$x.splice(d$$i,0,d$$y),d$$y)"))

; Quasiquoting
(defun bqconst (x)
  "True if the form 'x' is constant in respect to backquoting"
  (if (listp x)
      (if (or (= (aref x 0) '\,)
              (= (aref x 0) '\`)
              (= (aref x 0) '\,@))
          false
          (do ((i 0 (+ i 1)))
              ((or (= i (length x)) (not (bqconst (aref x i))))
               (= i (length x)))))
      true))

(defun bquote (x)
  "Returns the backquote expansion of x"
  (cond
   ((or (numberp x) (stringp x) (= x null))
    x)
   ((bqconst x)
    (list 'quote x))
   ((listp x)
    (cond
     ((= (aref x 0) '\`)
      (list '\` (bquote (aref x 1))))
     ((= (aref x 0) '\,)
      (aref x 1))
     ((= (aref x 0) '\,@)
      (error ",@ must be used inside lists"))
     (true
      (let ((res (list 'append))
            (clist (list 'list)))
        (dolist (el x)
          (cond
           ((and (listp el) (= (aref el 0) '\,@))
            (when (> (length clist) 1)
              (push clist res)
              (setq clist (list 'list)))
            (push (aref el 1) res))
           (true
            (push (bquote el) clist))))
        (when (> (length clist) 1)
          (push clist res))
        (if (> (length res) 2)
            res
            (aref res 1))))))
   (true (list 'quote x))))

(defmacro |`| (x)
    "Backquoting macro"
    (bquote x))

;; defmacro/f
(defmacro defmacro/f (name args &rest body)
  "Defines a macro and an equivalent function"
  ;; Note: Defmacro must be done at macro expansion time
  ;;       because we need the macro in place when
  ;;       defun is macroexpanded
  (eval `(defmacro ,name ,args ,@body))
  (let ((doc (if (stringp (aref body 0))
                 (splice body 0 1)
                 (list))))
    `(progn
       (defun ,name ,args (,name ,@args))
       (set-documentation (symbol-function ',name)
                          (documentation (symbol-macro ',name)))
       ',name)))

;; Utilities
(defmacro/f slice (x start size)
  "Returns size elements (or all remaining) from a given start point. Without args does a full shallow copy."
  (cond
    ((and (undefinedp start) (undefinedp size))
     `(js-code ,(+ "(" (js-compile x) ").slice()")))
    ((undefinedp size)
     `(js-code ,(+ "(" (js-compile x) ").slice(" (js-compile start) ")")))
    (true
     `(js-code ,(+ "(" (js-compile x) ".slice(" (js-compile start) "," (js-compile size) "))")))))

(defmacro/f reverse (list)
  "Returns a copy of the elements in the opposite ordering"
  `(js-code ,(+ "(" (js-compile list) ".slice().reverse())")))

(defmacro/f nreverse (list)
  "Reverses in place the ordering of the elements"
  `(js-code ,(+ "(" (js-compile list) ".reverse())")))

(defmacro/f first (x) "First element of list/string" `(aref ,x 0))
(defmacro/f second (x) "Second element of list/string" `(aref ,x 1))
(defmacro/f third (x) "Third element of list/string" `(aref ,x 2))
(defmacro/f fourth (x) "Fourth element of list/string" `(aref ,x 3))
(defmacro/f fifth (x) "Fifth element of list/string" `(aref ,x 4))
(defmacro/f sixth (x) "Sixth element of list/string" `(aref ,x 5))
(defmacro/f seventh (x) "Seventh element of list/string" `(aref ,x 6))
(defmacro/f eighth (x) "Eighth element of list/string" `(aref ,x 7))
(defmacro/f nineth (x) "Nineth element of list/string" `(aref ,x 8))
(defmacro/f tenth (x) "Tenth element of list/string" `(aref ,x 9))

;; String splitting
(defmacro/f split (x separator)
  "Splits a string x using the specified separator"
  `(js-code ,(+ "("
                (js-compile x)
                ".split("
                (js-compile separator)
                "))")))

(defmacro defmathop (name comment none single jsname)
  "Defines a math operator macro with the given comment (comment), the value to use in case of no operands (none), the value in case of a single operand x (single) and the Javascript operator name (jsname) when more operands are present."
  `(defmacro ,name (&rest args)
     ,comment
     (cond
       ((= (length args) 0)
        ,none)
       ((= (length args) 1)
        ,single)
       ((= (length args) 2)
        `(js-code ,(+ "(("
                      (js-compile (aref args 0))
                      ")" ,jsname "("
                      (js-compile (aref args 1))
                      "))")))
       (true
        `(js-code ,(+ "(("
                      (js-compile `(,',name ,@(slice args 0 (- (length args) 1))))
                      ")" ,jsname "("
                      (js-compile (aref args (- (length args) 1)))
                      "))"))))))

(defmacro defmathop-func (name)
  "Defines a math function based on a math operator macro defined with defmathop."
  `(progn
     (defun ,name (&rest args)
       (cond
         ((= (length args) 0) (,name))
         ((= (length args) 1) (,name (aref args 0)))
         ((= (length args) 2) (,name (aref args 0)
                                     (aref args 1)))
         (true
          (let ((res (aref args 0)))
            (dolist (x (slice args 1))
              (setq res (,name res x)))
            res))))
     (set-documentation (symbol-function ',name)
                        (documentation (symbol-macro ',name)))
     ',name))

; Math n-ary operators macros and functions

(defmathop + "Numeric addition or string concatenation"
  0 (aref args 0) "+")
(defmathop - "Numeric subtraction. When called with a single argument negates the operand."
  0 `(js-code ,(+ "-" (js-compile (aref args 0)))) "-")
(defmathop * "Numeric multiplication"
  1 (aref args 0) "*")
(defmathop / "Numeric division. When called with a single operand returns the inverse."
  1 `(/ 1 ,(aref args 0)) "/")
(defmathop logior "Bitwise inclusive or"
  0 (aref args 0) "|")
(defmathop logand "Bitwise and"
  -1 (aref args 0) "&")
(defmathop logxor "Bitwise exclusive or"
  0 (aref args 0) "^")
(defmathop-func +)
(defmathop-func -)
(defmathop-func *)
(defmathop-func /)
(defmathop-func logior)
(defmathop-func logand)
(defmathop-func logxor)

(defmacro/f % (a b)
  "Modulo operation"
  `(js-code ,(+ "("
                (js-compile a)
                "%"
                (js-compile b)
                ")")))

(defmacro/f ash (x count)
  "Arithmetic shift left (> count 0) or right (< count 0)"
  (if (numberp count)
      (if (> count 0)
          `(js-code ,(+ "((" (js-compile x) ")<<(" count "))"))
          `(js-code ,(+ "((" (js-compile x) ")>>(" (- count) "))")))
      `(js-code ,(+ "(function(x,c){return (c>0)?(x<<c):(x>>-c);})("
                    (js-compile x)
                    ","
                    (js-compile count)
                    ")"))))

; Make symbol / gensym
(defun make-symbol (name)
  "Creates a new uninterned symbol"
  (js-code "(new Symbol(f$$mangle(d$$name)))"))

(defvar *gensym-count* 0)

(defun gensym-prefix (prefix)
  "Returns a new uninterned unique symbol using the specified prefix"
  (make-symbol (+ "G:" prefix "/"
                  (setq *gensym-count* (+ 1 *gensym-count*)))))

(defun gensym-noprefix ()
  "Returns a new uninterned unique symbol"
  (make-symbol (+ "G:" (setq *gensym-count* (+ 1 *gensym-count*)))))

; Comparisons
(defmacro defrelop (name comment jsname)
  "Defines a relational operator short-circuiting macro given the name comment and Javascript operator name."
  `(defmacro ,name (&rest args)
     ,comment
     (cond
      ((= (length args) 0)
       true)
      ((= (length args) 1)
       true)
      ((= (length args) 2)
       `(js-code ,(+ "(" (js-compile (aref args 0)) ,jsname (js-compile (aref args 1)) ")")))
      (true
       (let ((x1 (gensym-noprefix))
             (x2 (gensym-noprefix)))
         `(let ((x1 ,(aref args 0))
                (x2 ,(aref args 1)))
            (and (,',name x1 x2) (,',name x2 ,@(slice args 2)))))))))

(defmacro defrelop-func (name)
  "Defines a variadic relational operator function given a corresponding macro name"
  `(defun ,name (&rest args)
     ,(documentation (symbol-macro name))
     (if (< (length args) 2)
         true
         (do ((current (first args))
              (n (length args))
              (i 1 (+ i 1)))
             ((or (= i n) (not (,name current (aref args i))))
                (= i n))
           (setq current (aref args i))))))

(defrelop <  "Strictly less than comparison" "<")
(defrelop <= "Less than or equal comparison" "<=")
(defrelop =  "Equality comparison" "==")
(defrelop == "Typed-equality comparison" "===")
(defrelop >= "Greater than or equal comparison" ">=")
(defrelop >  "Strictly greather than comparison" ">")
(defrelop-func <  )
(defrelop-func <= )
(defrelop-func =  )
(defrelop-func == )
(defrelop-func >= )
(defrelop-func >  )

;; /= operator is different from others as no transitivity can be used
;; (it means "arguments are all distinct")
(defmacro /= (&rest args)
  "True if and only if the passed values are all distinct"
  (cond
    ((< (length args) 2)
     true)
    ((= (length args) 2)
     `(js-code ,(+ "(" (js-compile (first args)) "!=" (js-compile (second args)) ")")))
    (true
     `(js-code ,(let ((res "(function(){")
                      (prev (list)))
                  (dolist (x args)
                    (setq res (+ res "var x$" (length prev) "=" (js-compile x) ";"))
                    (dolist (p prev)
                      (setq res (+ res "if(" p "==x$" (length prev) ")return false;")))
                    (push (+ "x$" (length prev)) prev))
                  (+ res "return true;})()"))))))

(defun /= (&rest args)
  "True if and only if the passed values are all distinct"
  (if (< (length args) 2)
      true
      (do ((n (- (length args) 1))
           (i 0 (+ i 1)))
          ((or (= i n) (/= -1 (js-code "d$$args.indexOf(d$$args[d$$i],d$$i+1)")))
             (= i n)))))

(defmacro let* (bindings &rest body)
  "Evaluates the body forms in sequence by first establishing lexical/dynamic bindings 'one at a time' so that during the evaluation of n-th binding all previous ones are already visible"
  (if (> (length bindings) 1)
     `(let (,(aref bindings 0))
        (let* ,(rest bindings) ,@body))
     `(let ,bindings ,@body)))

(defmacro setf (place value)
  "Sets the content of a place to be the specified value. A place is either a symbol or a form (e.g. (aref x i)) for which a corresponding setting form is defined (e.g. (set-aref x i value)) either as function or macro eventually after macro expansion."
  (do ()
      ((or (not (symbolp place))
           (and (not (lexical-symbol-macro place))
                (not (js-code "d$$place.symbol_macro")))))
    (setq place (or (lexical-symbol-macro place)
                    (js-code "d$$place.symbol_macro"))))
  (cond
    ((symbolp place)
     `(setq ,place ,value))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "set-" (symbol-name f)))))
       (cond
         ((or (lexical-macro sf)
              (lexical-function sf)
              (symbol-macro sf)
              (symbol-function sf))
          `(,sf ,@(rest place) ,value))
         ((lexical-macro f)
          `(setf ,(apply (lexical-macro f) (rest place)) ,value))
         ((symbol-macro f)
          `(setf ,(macroexpand-1 place) ,value))
         (true
          (error "Unsupported setf place")))))
    (true (error "Invalid setf place"))))

(defmacro incf (place inc)
  "Increments the content of a place by the specified increment or by 1 if not specified.
A place is either a symbol or a form (e.g. (aref x i)) for which a corresponding increment form is defined (e.g. (inc-aref x i inc)) either as function or macro eventually after macro expansion."
  (if (= inc undefined) (setf inc 1))
  (do ()
      ((or (not (symbolp place))
           (and (not (lexical-symbol-macro place))
                (not (js-code "d$$place.symbol_macro")))))
    (setq place (or (lexical-symbol-macro place)
                    (js-code "d$$place.symbol_macro"))))
  (cond
    ((symbolp place)
     `(setq ,place (+ ,place ,inc)))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "inc-" (symbol-name f)))))
       (cond
         ((or (lexical-macro sf)
              (lexical-function sf)
              (symbol-function sf)
              (symbol-macro sf))
          `(,sf ,@(rest place) ,inc))
         ((lexical-macro f)
          `(incf ,(apply (lexical-macro f) (rest place)) ,inc))
         ((symbol-macro f)
          `(incf ,(macroexpand-1 place) ,inc))
         (true
          (error "Unsupported incf place")))))
    (true (error "Invalid incf place"))))

(defmacro decf (place dec)
  "Decrements the content of a place by the specified decrement or by 1 if not specified.
A place is either a symbol or a form (e.g. (aref x i)) for which a corresponding decrement form is defined (e.g. (dec-aref x i dec)) either as function or macro eventually after macro expansion."
  (if (= dec undefined) (setf dec 1))
  (do ()
      ((or (not (symbolp place))
           (and (not (lexical-symbol-macro place))
                (not (js-code "d$$place.symbol_macro")))))
    (setq place (or (lexical-symbol-macro place)
                    (js-code "d$$place.symbol_macro"))))
  (cond
    ((symbolp place)
     `(setq ,place (- ,place ,dec)))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "dec-" (symbol-name f)))))
       (cond
         ((or (lexical-macro sf)
              (lexical-function sf)
              (symbol-function sf)
              (symbol-macro sf))
          `(,sf ,@(rest place) ,dec))
         ((lexical-macro f)
          `(decf ,(apply (lexical-macro f) (rest place)) ,dec))
         ((symbol-macro f)
          `(decf ,(macroexpand-1 place) ,dec))
         (true
          (error "Unsupported decf place")))))
    (true (error "Invalid decf place"))))

(defmacro inc-js-code (x delta)
  "Increment of js literal lvalue"
  `(js-code ,(+ "((" x ")+=(" (js-compile delta) "))")))

(defmacro dec-js-code (x delta)
  "Decrement of js literal lvalue"
  `(js-code ,(+ "((" x ")-=(" (js-compile delta) "))")))

(defmacro/f 1+ (x)
  "Returns (+ x 1)"
  `(+ ,x 1))

(defmacro/f 1- (x)
  "Returns (- x 1)"
  `(- ,x 1))

(defmacro inc-aref (x k value)
  "Increments the k-th element of a list or the element associated to key k in a javascript object"
  `(js-code ,(+ "(" (js-compile x)
                "[" (js-compile k)
                "]+=" (js-compile value) ")")))

(defmacro dec-aref (x k value)
  "Decrements the k-th element of a list or the element associated to key k in a javascript object"
  `(js-code ,(+ "(" (js-compile x)
                "[" (js-compile k)
                "]-=" (js-compile value) ")")))

(defmacro/f set-length (L n)
  "Sets the length of list L to n"
  `(js-code ,(+ "(" (js-compile L) ".length=" (js-compile n) ")")))

; Sequence utilities
(defmacro/f pop (x)
  "Removes and returns last element from list x"
  `(js-code ,(+ "(" (js-compile x) ".pop())")))

(defmacro empty (x)
  "True if length of array or string x is zero"
  `(not (length ,x)))

(defun last (x) "Last element of list/string" (aref x (1- (length x))))

(defun set-last (x y) "Sets the last element of list/string" (setf (aref x (1- (length x))) y))

(defun reduce (f seq)
  "Reduces a sequence to a single value by repeating application of function f to pairs of elements in the sequence seq.
For an empty sequence the return value is the result of calling the function without parameters"
  (if (= 0 (length seq))
      (funcall f)
      (let ((res (first seq)))
        (dolist (x (rest seq))
          (setf res (funcall f res x)))
        res)))

(defmacro min (&rest seq)
  "Returns the minimum value of all arguments under comparison with <"
  `(js-code ,(let ((code "(Math.min(")
                   (sep ""))
               (dolist (x seq)
                 (setf code (+ code sep (js-compile x)))
                 (setf sep ","))
               (+ code "))"))))
(defun min (&rest seq)
  "Returns the minimum value of all arguments under comparison with <"
  (js-code "(Math.min.apply(Math,d$$seq))"))

(defmacro max (&rest seq)
  "Returns the maximum value of all arguments under comparison with >"
  `(js-code ,(let ((code "(Math.max(")
                   (sep ""))
               (dolist (x seq)
                 (setf code (+ code sep (js-compile x)))
                 (setf sep ","))
               (+ code "))"))))
(defun max (&rest seq)
  "Returns the maximum value of all arguments under comparison with >"
  (js-code "(Math.max.apply(Math,d$$seq))"))

(defun map (f seq)
  "Returns the sequence obtained by applying function f to each element of sequence seq"
  (let ((res (list)))
    (dolist (x seq)
      (push (funcall f x) res))
    res))

(defun zip (&rest sequences)
  "Returns a list of lists built from corresponding elements in all sequences.
The resulting list length is equal to the shortest input sequence."
  (let ((n (apply #'min (map #'length sequences))))
    (let ((res (list)))
      (dotimes (i n)
        (push (map (lambda (seq) (aref seq i)) sequences) res))
      res)))

(defun mapn (f &rest sequences)
  "Returns the sequence of calling function f passing as parameters corresponding elements from the sequences.
The resulting list length is equal to the length of the shortest sequence."
  (map (lambda (args) (apply f args))
       (apply #'zip sequences)))

(defun filter (f seq)
  "Returns the subset of elements from sequence seq for which the function f returned a logical true value"
  (let ((res (list)))
    (dolist (x seq)
      (when (funcall f x)
        (push x res)))
    res))

(defun index0 (x L)
  "Returns the index position in which x appears in list/string L or -1 if it's not present"
  (js-code "d$$L.indexOf(d$$x)"))

(defun last-index (x L)
  "Returns the last index position in which x appears in list/string L or -1 if it's not present"
  (js-code "d$$L.lastIndexOf(d$$x)"))

(defun find (x L)
  "True if element is included in L"
  (/= -1 (index0 x L)))

(defun remove (x L)
  "Returns a copy of L after any x has been removed"
  (let ((res (list)))
    (dolist (y L)
      (unless (= x y)
        (push y res)))
    res))

(defun subset (L1 L2)
  "True if every element in L1 is also in L2"
  (do ((i 0 (1+ i)))
      ((or (>= i (length L1))
           (not (find (aref L1 i) L2)))
         (>= i (length L1)))))

(defun set-union (L1 L2)
  "List of elements appearing in L1 or in L2"
  (let ((res (slice L1)))
    (dolist (x L2)
      (unless (find x res)
        (push x res)))
    res))

(defun set-difference (L1 L2)
  "List of elements appearing in L1 but not in L2"
  (let ((res (list)))
    (dolist (x L1)
      (unless (find x L2)
        (push x res)))
    res))

(defun set-intersection (L1 L2)
  "List of elements appearing in both L1 and L2"
  (let ((res (list)))
    (dolist (x L1)
      (when (find x L2)
        (push x res)))
    res))

(defmacro/f nsort (x cond)
  "Modifies a sequence inplace by sorting the elements according to the specified condition or #'< if no condition is given."
  (if (= cond undefined)
      `(js-code ,(+ "(" (js-compile x) ").sort(function(a,b){return a<b?-1:1;})"))
      `(js-code ,(+ "(" (js-compile x) ").sort(function(a,b){return (" (js-compile cond) ")(a,b)?-1:1;})"))))

(defmacro/f sort (x cond)
  "Returns a copy of a sequence with elements sorted according to the specified condition or #'< if no condition is given."
  `(nsort (slice ,x) ,cond))

; &optional

(defmacro argument-count ()
  `(js-code "arguments.length"))

(setf (compile-specialization 'lambda)
      (let* ((oldcf (compile-specialization 'lambda))
             (olddoc (documentation oldcf))
             (f (lambda (whole)
                  (let* ((args (second whole))
                         (body (slice whole 2))
                         (doc (if (stringp (first body))
                                  (js-code "d$$body.slice(0,1)")
                                  (list)))
                         (i (index0 '&optional args))
                         (r (index0 '&rest args)))
                    (if (= i -1)
                        (funcall oldcf
                                 `(lambda ,args
                                    ,@doc
                                    ,@(if (= r -1)
                                          `((unless (= (argument-count) ,(length args))
                                              (error "Invalid number of arguments")))
                                          (if (> r 0)
                                              `((when (< (argument-count) ,r)
                                                  (error "Invalid number of arguments")))
                                              (list)))
                                    ,@body))
                        (let ((defaults (list))
                              (checks (list))
                              (args (append (slice args 0 i)
                                            (slice args (1+ i)))))
                          (unless (= i 0)
                            (push `(when (< (argument-count) ,i)
                                     (error "Invalid number of arguments"))
                                  checks))
                          (when (= r -1)
                            (push `(when (> (argument-count) ,(length args))
                                     (error "Invalid number of arguments"))
                                  checks))
                          (dotimes (i (length args))
                            (when (listp (aref args i))
                              (push `(when (< (argument-count) ,(1+ i))
                                       (setf ,(first (aref args i))
                                             ,(second (aref args i))))
                                    defaults)
                              (setf (aref args i) (first (aref args i)))))
                          (funcall oldcf
                                   `(lambda ,args
                                      ,@doc
                                      ,@checks
                                      ,@defaults
                                      ,@body))))))))
        (setf (documentation f) olddoc)
        f))

; Keyword arguments

(setf (compile-specialization 'lambda)
      (let* ((oldcf (compile-specialization 'lambda))
             (oldcomm (documentation (compile-specialization 'lambda)))
             (unassigned (gensym-noprefix))
             (f (lambda (whole)
                  (let* ((args (second whole))
                         (body (slice whole 2))
                         (i (index0 '&key args)))
                    (if (= i -1)
                        (funcall oldcf whole)
                        (let ((rest (gensym-prefix "rest"))
                              (nrest (gensym-prefix "nrest"))
                              (ix (gensym-prefix "ix")))
                          (unless (= -1 (index0 '&rest args))
                            (error "&key and &rest are incompatible"))
                          (funcall oldcf
                                   `(lambda (,@(slice args 0 i) &rest ,rest)
                                      (let ((,nrest (length ,rest))
                                            ,@(map (lambda (x)
                                                     (if (listp x)
                                                         `(,(first x) ',unassigned)
                                                         `(,x undefined)))
                                                   (slice args (1+ i))))
                                        (do ((,ix 0 (+ ,ix 2)))
                                            ((>= ,ix ,nrest)
                                             (when (> ,ix ,nrest)
                                               (error "Invalid number of parameters")))
                                          (cond
                                            ,@(append
                                               (map (lambda (x)
                                                      `((= (aref ,rest ,ix)
                                                           ,(intern (+ ":" (symbol-name (if (listp x) (first x) x)))))
                                                        (setf ,(if (listp x) (first x) x) (aref ,rest (1+ ,ix)))))
                                                    (slice args (1+ i)))
                                               `((true (error "Invalid parameters"))))))
                                        ,@(let ((res (list)))
                                               (dolist (x (slice args (1+ i)))
                                                 (when (listp x)
                                                   (push `(when (= ,(first x) ',unassigned)
                                                            (setf ,(first x) ,(second x)))
                                                         res)))
                                               res)
                                        ,@body)))))))))
        (setf (documentation f) oldcomm)
        f))

; Parameter conditions
(setf (compile-specialization 'lambda)
      (let* ((old (compile-specialization 'lambda))
             (f (lambda (x)
                  (let ((checks (list))
                        (j 0))
                    (dolist (f (second x))
                      (let* ((n (symbol-name (if (symbolp f) f (first f))))
                             (i (index0 "/" n)))
                        (when (> i 0)
                          (let ((ns (intern (slice n 0 i)))
                                (cf (intern (+ (slice n (1+ i))))))
                            (push (list ns cf) checks)
                            (if (symbolp f)
                                (setf (aref (second x) j) ns)
                                (setf (first (aref (second x) j)) ns)))))
                      (incf j))
                    (if (zerop (length checks))
                        (funcall old x)
                        (let ((doc (list))
                              (args (second x))
                              (body (slice x 2)))
                          (when (stringp (first body))
                            (setf doc (list (first body)))
                            (setf body (rest body)))
                          (js-eval (js-compile `(lambda ,(second x)
                                                  ,@doc
                                                  ,@(map (lambda (check)
                                                           `(unless (,(second check) ,(first check))
                                                              (error ,(+ (symbol-name (first check))
                                                                         ": invalid precondition ("
                                                                         (symbol-name (second check))
                                                                         ")"))))
                                                         checks)
                                                  ,@body)))))))))
        (setf (documentation f) (documentation old))
        f))

; Destructuring

(setf (compile-specialization 'lambda)
      (let* ((oldcf (compile-specialization 'lambda))
             (olddoc (documentation oldcf))
             (f (lambda (whole)
                  (let* ((args (second whole))
                         (body (slice whole 2))
                         (doc (if (stringp (first body))
                                  (js-code "d$$body.slice(0,1)")
                                  (list)))
                         (dslist (list)))
                    (do ((n (length args))
                         (i 0 (1+ i)))
                        ((or (= i n)
                             (and (symbolp (aref args i))
                                  (= "&" (aref (symbol-name (aref args i)) 0)))))
                      (when (listp (aref args i))
                        (let ((tname (gensym-noprefix)))
                          (push (list tname (aref args i)) dslist)
                          (setf (aref args i) tname))))
                    (when (> (length dslist) 0)
                      (labels ((expand (expr template)
                                 (cond
                                   ((symbolp template)
                                    `((,template ,expr)))
                                   ((listp template)
                                    (let ((res (list)))
                                      (dotimes (i (length template))
                                        (setf res (append res
                                                          (expand `(aref ,expr ,i)
                                                                  (aref template i)))))
                                      res))
                                   (true (error "Invalid destructuring list")))))
                        (setf body
                              `((let (,@(apply #'append (map (lambda (x) (expand (first x) (second x)))
                                                             dslist)))
                                  ,@body)))))
                    (funcall oldcf `(lambda ,args ,@doc ,@body))))))
        (setf (documentation f) olddoc)
        f))

; Array construction

(defun make-array (n &optional initial-value)
  "Creates a list containing n elements all equal to the specified initial value"
  (let ((x (list)))
    (dotimes (i n)
      (setf (aref x i) initial-value))
    x))

; Range

(defun range (start &optional stop step)
  "Returns a list containing all numbers from start (0 if not specified) up to stop counting by step (1 if not specified).
If only one parameter is passed it's assumed to be 'stop'."
  (when (= step undefined)
    (setf step 1))
  (when (= stop undefined)
    (setf stop start)
    (setf start 0))
  (let ((res (list)))
    (do ((x start (incf start step)))
        ((>= (* step (- x stop)) 0))
      (push x res))
    res))

(defun fp-range (n from to)
  "A list of <n> equally spaced floating point values starting with <from> and ending with <to>"
  (let ((result (list)))
    (dotimes (i (1- n))
      (push (+ from (/ (* i (- to from)) (1- n)))
            result))
    (push to result)
    result))

; Generic gensym
(defmacro gensym (&optional prefix)
  (if prefix
      `(gensym-prefix ,prefix)
      `(gensym-noprefix)))

; Generic index
(defmacro index (x seq &optional start)
  "Returns the index of element x in seq or -1 if not present, eventually starting from the specified position"
  (if start
      `(js-code ,(+ "("
                    (js-compile seq)
                    ".indexOf("
                    (js-compile x)
                    ","
                    (js-compile start)
                    "))"))
      `(js-code ,(+ "("
                    (js-compile seq)
                    ".indexOf("
                    (js-compile x)
                    "))"))))

(defun index (x seq &optional start)
  "Returns the index of element x in seq or -1 if not present, eventually starting from the specified position"
  (if start
      (js-code "(d$$seq.indexOf(d$$x,d$$start))")
      (js-code "(d$$seq.indexOf(d$$x))")))

; Any/all
(defmacro any (var &rest body)
  (let ((index (gensym))
        (list (gensym)))
    `(do ((,list ,(second var))
          (,index 0 (1+ ,index)))
         ((or (>= ,index (length ,list))
              (let ((,(first var) (aref ,list ,index)))
                ,@body))
            (< ,index (length ,list))))))

(defmacro all (var &rest body)
  `(not (any ,var (not (progn ,@body)))))

; Defstruct
(defmacro defstruct (name &rest fields)
  "Defines a structure with the specified fields. Each field can be either a symbol or a list with a symbol
and a default value form that will be used when instantiating the structure if no values are passed for
that field. When absent the default value is assumed to be the undefined value."
  (let ((fnames (map (lambda (f) (if (listp f)
                                     (first f)
                                     f))
                     fields)))
    `(progn
       (defun
           ,(intern (+ "make-" (symbol-name name)))
           (&key ,@fields)
         (list ',name ,@fnames))
       (defun ,(intern (+ (symbol-name name) #\?)) (self)
         (and (listp self) (= ',name (aref self 0))))
       (defvar ,(intern (+ "*" (symbol-name name) "-fields*"))
         ',fnames)
       ,@(let ((res (list))
               (index 1))
           (dolist (f fnames)
             (let ((fn (intern (+ (symbol-name name) "-" (symbol-name f)))))
               (push `(defun ,fn (self)
                        (unless (,(intern (+ (symbol-name name) #\?)) self)
                          (error ,(+ "Not a " (symbol-name name) " instance")))
                        (aref self ,index)) res)
               (push `(defun ,(intern (+ "set-" (symbol-name fn))) (self value)
                        (unless (,(intern (+ (symbol-name name) #\?)) self)
                          (error ,(+ "Not a " (symbol-name name) " instance")))
                        (setf (aref self ,index) value)) res)
               (push `(defun ,(intern (+ "inc-" (symbol-name fn))) (self value)
                        (unless (,(intern (+ (symbol-name name) #\?)) self)
                          (error ,(+ "Not a " (symbol-name name) " instance")))
                        (incf (aref self ,index) value)) res)
               (push `(defun ,(intern (+ "dec-" (symbol-name fn))) (self value)
                        (unless (,(intern (+ (symbol-name name) #\?)) self)
                          (error ,(+ "Not a " (symbol-name name) " instance")))
                        (decf (aref self ,index) value)) res)
               (incf index)))
           res)
       ',name)))

; Math functions
(defmacro/f sqrt (x) "Square root of x"
  `(js-code ,(+ "Math.sqrt(" (js-compile x) ")")))
(defmacro/f sin (x) "Sine of angle x in radians"
  `(js-code ,(+ "Math.sin(" (js-compile x) ")")))
(defmacro/f cos (x) "Cosine of angle x in radians"
  `(js-code ,(+ "Math.cos(" (js-compile x) ")")))
(defmacro/f tan (x) "Tangent of angle x in radians"
  `(js-code ,(+ "Math.tan(" (js-compile x) ")")))
(defmacro/f exp (x) "e=2.718281828... raised to power of x"
  `(js-code ,(+ "Math.exp(" (js-compile x) ")")))
(defmacro/f log (x) "Natural logarithm of x"
  `(js-code ,(+ "Math.log(" (js-compile x) ")")))
(defmacro/f atan (x) "Arc-tangent in radians of the value x"
  `(js-code ,(+ "Math.atan(" (js-compile x) ")")))
(defmacro/f floor (x) "Biggest integer that is not bigger than x"
  `(js-code ,(+ "Math.floor(" (js-compile x) ")")))
(defmacro/f abs (x) "Absolute value of x"
  `(js-code ,(+ "Math.abs(" (js-compile x) ")")))
(defmacro/f atan2 (y x) "Arc-tangent of y/x in radians with proper handling of all quadrants"
  `(js-code ,(+ "Math.atan2(" (js-compile y) "," (js-compile x) ")")))
(setq pi (js-code "Math.PI"))

; Swap

(defmacro swap (a b)
  "Swaps the content of two places (evaluating each place twice: once for reading and once for writing)"
  (let ((xa (gensym))
        (xb (gensym)))
    `(let ((,xa ,a)
           (,xb ,b))
       (setf ,a ,xb)
       (setf ,b ,xa)
       null)))

; Random

(defmacro random ()
  "Random number between 0 < x < 1"
  `(js-code "(Math.random())"))

(defun random-int (n)
  "Random integer number 0 <= x < n"
  (ash (* (random) n) 0))

(defun random-shuffle (L)
  "Randomly shuffles an array inplace and returns null"
  (dotimes (i (length L))
    (let ((j (random-int (length L))))
      (swap (aref L i) (aref L j)))))

(defun random-shuffled (L)
  "Returns a randomly shuffled version of an array"
  (let ((x (slice L)))
    (random-shuffle x)
    x))

; Filler string

(defun filler (n &optional (c " "))
  "Returns a string composed by replicating a specified string (a space by default)"
  (cond
    ((<= n 0) "")
    ((= n 1) c)
    ((logand n 1) (+ c (filler (1- n) c)))
    (true
       (let ((h (filler (ash n -1) c)))
         (+ h h)))))

; JS exception support
(defvar *exception* null)
(setf (compile-specialization 'try)
      (lambda (x)
        (+ "((function(){try{return("
           (js-compile (aref x 1))
           ");}catch(err){var olderr=d$$$42$exception$42$;d$$$42$exception$42$=err;var res=("
           (js-compile (aref x 2))
           ");d$$$42$exception$42$=olderr;return res;}})())")))
(setf (documentation (compile-specialization 'try))
      "(try expr on-error)
Evaluates expr and in case of exception evaluates the on-error form setting *exception* to the current exception")

; Timing
(defun clock ()
  "Returns the number of millisecond passed since 00:00:00.000 of January 1st, 1970"
  (js-code "(new Date).getTime()"))

(defmacro time (&rest body)
  "Measures and returns the number of millisecond needed for the evaluation of all body forms in sequence"
  (let ((start (gensym)))
    `(let ((,start (clock)))
       ,@body
       (- (clock) ,start))))

; Regular expression
(defun regexp (x &optional options)
  "Returns a new Javascript regular expression object"
  (js-code "(new RegExp(d$$x,d$$options||\"\"))"))

(defun replace (x a b)
  "Replaces all instances of regular expression a with b."
  (js-code "d$$x.replace(new RegExp(d$$a,'g'), d$$b)"))

; JS object access/creation
(defmacro . (obj &rest fields)
  "Returns the javascript object value selected by traversing the specified chain of fields.
A field is either an unevaluated symbol, a number, a string or an (evaluated) form."
  (let ((res (js-compile obj)))
    (dolist (x fields)
      (setf res (cond
                  ((symbolp x) (+ res "." (symbol-name x)))
                  ((listp x) (+ res "[" (js-compile x) "]"))
                  (true (+ res "[" (str-value x) "]")))))
    `(js-code ,res)))

(defmacro set-. (obj &rest fields)
  "Sets the javascript object value selected by traversing the specified chain of fields.
A field is either an unevaluated symbol, a number, a string or an (evaluated) form."
  (let ((res (js-compile obj)))
    (dolist (x (slice fields 0 (1- (length fields))))
      (setf res (cond
                  ((symbolp x) (+ res "." (symbol-name x)))
                  ((listp x) (+ res "[" (js-compile x) "]"))
                  (true (+ res "[" (str-value x) "]")))))
    (setf res (+ res "=" (js-compile (aref fields (1- (length fields))))))
    `(js-code ,res)))

;; Funcall specialization for (funcall (. a b) ...) to avoid wrapper creation
(setf (symbol-macro 'funcall)
      (let ((om (symbol-macro 'funcall)))
        (lambda (f &rest args)
          (if (and (listp f)
                   (= (first f) '.)
                   (= (length f) 3))
              `(js-code ,(+ (js-compile (second f))
                            "."
                            (symbol-name (third f))
                            "("
                            (let ((sep "")
                                  (res ""))
                              (dolist (x args)
                                (setf res (+ res sep (js-compile x)))
                                (setf sep ","))
                              res)
                            ")"))
              (apply om (append (list f) args))))))

(defmacro js-object (&rest fields)
  "Creates a javascript object and eventually assigns fields.
Each field is a list of an unevaluated atom as name and a value."
  (let ((self (gensym)))
    `(let ((,self (js-code "({})")))
       ,@(let ((res (list)))
           (dolist (f fields)
             (push `(setf (. ,self ,(first f)) ,(second f)) res))
           res)
       ,self)))

(defun keys (obj)
  "Returns a list of all keys defined in the specified javascript object"
  (js-code "((function(){var res=[];for(var $i in d$$obj)res.push($i);return res})())"))

; String interpolation reader
(setf (reader "~")
      (lambda (src)
        (funcall src 1)
        (let ((x (parse-value src))
              (pieces (list))  ; list of parts
              (part "")        ; current part
              (expr false)     ; is current part an expression?
              (escape false))  ; is next char verbatim ?
          (unless (stringp x)
            (error "string interpolation requires a string literal"))
          (dolist (c x)
            (cond
              (escape
               (setf escape false)
               (incf part c))
              ((= c "\\")
               (setf escape true))
              ((= c (if expr "}" "{"))
               (when (> (length part) 0)
                 (push (if expr (parse-value part) part) pieces)
                 (setf part ""))
               (setf expr (not expr)))
              (true (incf part c))))
          (if escape
              (error "Invalid escaping"))
          (if (> (length part) 0)
              (push (if expr (parse-value part) part) pieces))
          (cond
            ((= 0 (length pieces)) "")
            ((= 1 (length pieces)) (aref pieces 0))
            (true `(+ ,@pieces))))))

; Javscript simple function/method binding
(defmacro bind-js-functions (&rest names)
  "Creates a JsLisp wrapper for the provided function names or methods of singletons (e.g. (document.write 'hello'))"
  `(progn
      ,@(map (lambda (name)
               (let* ((s (symbol-name name))
                      (i (last-index "." s)))
                 (if (= i -1)
                     `(defun ,name (&rest args)
                        ,~"Calls Javascript function {name} passing specified arguments"
                        (js-code ,~"({s}.apply(window,d$$args))"))
                     `(defun ,name (&rest args)
                        (js-code ,~"({s}.apply({(slice s 0 i)},d$$args))")))))
             names)))

(defmacro bind-js-methods (&rest names)
  "Creates a JsLisp wrapper for method call (e.g. (write document 'hello'))"
  `(progn
      ,@(map (lambda (name)
               `(defun ,name (self &rest args)
                  ,~"Calls Javascript method {name} on self object passing the specified arguments"
                  (js-code ,~"(d$$self.{(symbol-name name)}.apply(d$$self,d$$args))")))
             names)))

; DOM
(setf document (js-code "document"))
(setf window (js-code "window"))
(defun htm (x)
  "Escapes characters so that the content string x can be displayed correctly as HTML"
  (setf x (replace x "&" "&amp;"))
  (setf x (replace x "<" "&lt;"))
  (setf x (replace x ">" "&gt;"))
  (setf x (replace x "\"" "&quot;"))
  x)

(defun get-element-by-id (id)
  "Returns the DOM element with the specified id value"
  (funcall (. document getElementById) id))

(defun create-element (type)
  "Creates a new DOM element with the specified type passed as a string"
  (funcall (. document createElement) type))

(defun append-child (parent child)
  "Appends the DOM element 'child' as last (frontmost) children of the parent DOM element"
  (funcall (. parent appendChild) child))

(defun remove-child (parent child)
  "Removes the DOM element child from the list of children of parent DOM element"
  (funcall (. parent removeChild) child))

; Timer events
(defun set-timeout (f delay)
  "Invokes the specified function f after a delay (in ms). Returns an id usable in clear-timeout."
  (js-code "setTimeout(function(){d$$f()}, d$$delay)"))

(defun clear-timeout (id)
  "Disables a specified delayed call if it has not been already executed."
  (js-code "clearTimeout(d$$id)"))

(defun set-interval (f interval)
  "Invokes the specified function f every `interval` ms. Returns an id usable in clear-interval"
  (js-code "setInterval(function(){d$$f()}, d$$interval)"))

(defun clear-interval (id)
  "Stops a scheduled interval call."
  (js-code "clearInterval(d$$id)"))

; Line split utility
(defun maplines (f str)
  "Calls a function for each line in a string"
  (do ((i 0)
       (n (length str)))
      ((>= i n))
    (let ((j (index "\n" str i)))
      (when (= j -1)
        (setf j (1+ n)))
      (funcall f (slice str i (- j i)))
      (setf i (1+ j)))))

(defmacro dolines ((var str) &rest body)
  "Executes a body for each line of a string"
  `(maplines (lambda (,var) ,@body) ,str))

; Destructuring dolist utility
(defmacro dolist-d ((vars L) &rest body)
  "dolist-like utility destructuring elements to multiple variables"
  (let ((x (gensym)))
    `(dolist (,x ,L)
       (let ,(let ((res (list)))
               (dotimes (i (length vars))
                 (push `(,(aref vars i) (aref ,x ,i)) res))
               res)
         ,@body))))

; Loose number parsing
(defmacro/f atof (s)
  "Returns a float from the start of the specified string (NaN if fails)"
  `(js-code ,(+ "parseFloat(" (js-compile s) ")")))

(defmacro/f atoi (s)
  "Returns an integer from the start of the specified string (NaN if fails)"
  `(js-code ,(+ "parseInt(" (js-compile s) ")")))

; Round formatting
(defun to-fixed (x n)
  "Formats a number using the specified amounts of decimals"
  (js-code "(d$$x.toFixed(d$$n))"))

; Javascript blocking interaction
(defun prompt (x)
  "Asks the user for a string providing x as a prompt message"
  (js-code "prompt(d$$x)"))

(defun alert (x)
  "Displays an alert message x to the user"
  (js-code "alert(d$$x)"))

(defun yesno (x)
  "Asks the user to reply either yes or no to a question. Returns True if the answer is yes or False otherwise"
  (do ((reply (prompt x)
              (prompt ~"I don't understand...\n{x}\nPlease answer \"yes\" or \"no\" without quotes.")))
      ((or (= reply "yes")
           (= reply "no"))
       (= reply "yes"))))

; documentation support
(defmacro help (name)
  "Displays any documentation for compile specialization, macro, function or value bound to the (unevaluated) specified symbol."
  (labels ((doc (x)
             (unless (stringp x)
               (setf x (or (documentation x) "- no documentation -")))
             (do ((i (if (/= -1 (index "\n" x))
                         (index "\n" x)
                         60)
                     (1+ i)))
                 ((or (>= i (length x))
                      (/= -1 (index (aref x i) *spaces*)))
                  (+ (slice x 0 i) "\n"
                     (if (< i (length x))
                         (doc (slice x (1+ i)))
                         ""))))))
    (let ((found false))
      (when (compile-specialization name)
        (display ~"Compile specialization {(symbol-name name)}\n{(doc (compile-specialization name))}")
        (setf found true))
      (when (symbol-macro name)
        (display ~"Macro {(symbol-name name)}\n{(doc (symbol-macro name))}")
        (setf found true))
      (when (symbol-function name)
        (display ~"Function {(symbol-name name)}\n{(doc (symbol-function name))}")
        (setf found true))
      (when (symbol-value name)
        (display ~"Variable {(symbol-name name)}\nCurrent value: {(str-value (symbol-value name))}")
        (setf found true))
      (unless found
        (display ~"No documentation available for {(symbol-name name)}"))
      `',name)))

;; Tagbody

(defmacro go (tagname)
  `(js-code ,(+ "((function(){throw "
                "$tag$" (mangle (symbol-name tagname))
                ";})())")))

(defmacro tagbody (&rest body)
  (let ((tagdecl "")
        (eelist "")
        (sep "var $tag$=null,")
        (eesep "")
        (fragments (list (list))))
    (dolist (x body)
      (if (symbolp x)
          (progn
            (setf tagdecl (+ tagdecl sep "$tag$" (mangle (symbol-name x)) "=[]"))
            (setf sep ",")
            (setf eelist (+ eelist eesep "$ee$==$tag$" (mangle (symbol-name x))))
            (setf eesep "||")
            (push (list x) fragments))
          (push x (last fragments))))
    (cond
      ((= tagdecl "")
       `(progn ,@body null))
      ((and (= (length fragments) 2)
            (= (length (aref fragments 1)) 1))
       (let ((exit-tag (aref (aref fragments 1) 0)))
         `(js-code ,(+ "((function(){"
                       "var $tag$" (mangle (symbol-name exit-tag)) "=[];"
                       "try{"
                       (js-compile `(progn ,@(first fragments)))
                       "}catch($ee$){if($ee$!=$tag$" (mangle (symbol-name exit-tag)) "){throw $ee$}}"
                       "return null;})())"))))
      (true
       `(js-code ,(+ "((function(){"
                     tagdecl
                     ";for(;;){try{switch($tag$){case null:"
                     (js-compile `(progn ,@(first fragments)))
                     ";"
                     (let ((cases ""))
                       (dolist (x (rest fragments))
                         (setf cases (+ cases
                                        "case $tag$" (mangle (symbol-name (first x))) ":"
                                        (js-compile `(progn ,@(rest x))) ";")))
                       cases)
                     "}break}catch($ee$){if(" eelist "){$tag$=$ee$}else throw $ee$;}}})(),null)"))))))

;; Block / return / return-from
(defvar ret@ (js-object))

(defmacro return-from (name &optional value)
  (when (undefinedp (aref ret@ name))
    (error ~"(return-from {name} ...) can be used only inside (block {name} ...)"))
  (setf (aref ret@ name) true)
  `(progn
     (setf ,(intern ~"{name}@result") ,value)
     (go ,(intern ~"ret@{name}"))))

(defmacro return (&optional value)
  `(return-from null ,value))

(defmacro block (name &rest body)
  (let ((old-ret (aref ret@ name)))
    (setf (aref ret@ name) false)
    (let ((jsbody (js-compile `(progn ,@body))))
      (let ((mr (if (aref ret@ name)
                    `(let ((,(intern ~"{name}@result") null))
                       (tagbody
                          (setf ,(intern ~"{name}@result")
                                (js-code ,jsbody))
                          ,(intern ~"ret@{name}"))
                       ,(intern ~"{name}@result"))
                    `(js-code ,jsbody))))
        (setf (aref ret@ name) old-ret)
        mr))))

(let ((odoc (documentation (symbol-macro 'defun))))
  (setf (symbol-macro 'defun)
        (let ((om (symbol-macro 'defun)))
          (lambda (name args &rest body)
            (let ((doc (if (stringp (first body))
                           (js-code "d$$body.splice(0,1)")
                           (list))))
              (apply om `(,name ,args ,@doc (block ,name ,@body)))))))
  (setf (documentation (symbol-macro 'defun) odoc)))

(setf (compile-specialization 'lambda)
      (let ((om (compile-specialization 'lambda)))
        (lambda (x)
          (let* ((args (second x))
                 (body (slice x 2))
                 (doc (if (stringp (first body))
                          (js-code "d$$body.splice(0,1)")
                          (list))))
            (apply om `((lambda ,args
                          ,@doc
                          ,(append `(block null) body))))))))

;; Compile-time argument checking

(defun static-check-args (form args)
  (let ((fi 1)
        (ai 0)
        (fn (length form))
        (an (length args))
        (uae false))
    (do ()
        ((or (= fi fn)
             (= ai an)
             (find (aref args ai) '(&rest &key &optional))))
      (incf ai)
      (incf fi))
    (when (and (= ai an) (< fi fn))
      (setf uae true)
      (warning ~"Unexpected arguments in {(str-value form)}"))
    (when (and (< ai an)
               (not (find (aref args ai) '(&rest &key &optional)))
               (= fi fn))
      (warning ~"Not enough arguments in {(str-value form)}"))
    (when (= (aref args ai) '&optional)
      (incf ai)
      (do ()
          ((or (= ai an)
               (= fi fn)
               (find (aref args ai) '(&key &rest))))
        (incf ai)
        (incf fi)))
    (when (and (not uae) (= ai an) (< fi fn))
      (warning ~"Unexpected arguments in {(str-value form)}"))
    (when (and (< ai an) (= (aref args ai) '&key))
      (when (% (- fn fi) 2)
        (warning ~"Odd number of arguments in keyword pairing in {(str-value form)}"))
      (incf ai)
      (let ((keys (map (lambda (kwa)
                         (if (listp kwa)
                             (first kwa)
                             kwa))
                       (slice args ai))))
        (do ()
            ((>= fi fn))
          (let ((k (aref form fi)))
            (when (and (symbolp k)
                       (= ":" (aref (symbol-name k) 0)))
              (unless (find (intern (slice (symbol-name k) 1)) keys)
                (warning ~"Invalid keyword parameter {k} in {(str-value form)}"))))
          (incf fi 2))))))

; Simple destructuring let
(defmacro dlet (vars list &rest body)
  (if (symbolp list)
      `(if (= (length ,list) ,(length vars))
           (let (,@(let ((res (list))
                         (index -1))
                     (dolist (v vars)
                       (push `(,v (aref ,list ,(incf index))) res))
                     res))
             ,@body)
           (error "Invalid destructuring assignment"))
      (let ((x (gensym)))
        `(let ((,x ,list))
           (dlet ,vars ,x ,@body)))))

; Defconstant
(defmacro defconstant (name value)
  "Defines a constant value associated with the global variable identified by name"
  `(progn
     (setf (symbol-value ',name) ,value)
     (setf (. ',name constant) true)))

; Define/undefine symbol-macro
(defmacro define-symbol-macro (x y)
  `(setf (. ',x symbol_macro) ',y))

(defmacro undefine-symbol-macro (x)
  `(js-code ,(+ "((function(){delete s"
                (mangle (symbol-name x))
                ".symbol_macro;})())")))

; Import
(defmacro import (name)
  `(unless (symbol-value ',(intern ~"+{name}.lisp.included+"))
     (defconstant ,(intern ~"+{name}.lisp.included+") true)
     (load (http-get ,(+ (symbol-name name) ".lisp")))))

; Lexical symbol properties support
(defun lexical-property (x name)
  (js-code "(lexvar.props[d$$x.name][d$$name])"))

(defun set-lexical-property (x name value)
  (js-code "(lexvar.props[d$$x.name][d$$name]=d$$value)"))

; Case
(defmacro case (expr &rest cases)
  (let ((v (gensym)))
    `(let ((,v ,expr))
       (cond
         ,@(map (lambda (c)
                  (if (= (first c) 'otherwise)
                      `(true ,@(rest c))
                      `((= ,v ,(first c))
                        ,@(rest c))))
            cases)))))