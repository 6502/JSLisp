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
                    (setq name (module-symbol name))
                    (let ((doc (str-value (append (list name) args))))
                      (setq doc (js-code "('[['+d$$doc+']]')"))
                      (if (and (string? (js-code "d$$body[0]"))
                               (js-code "(d$$body.length>1)"))
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

(defmacro when (condition &rest body)
  "Evaluates all forms in [body] returning last value only if [condition] evaluates to a true value, otherwise the value is [null]"
  (list 'if condition (append (list 'progn) body) null))

(defmacro unless (condition &rest body)
  "Evaluates all forms in [body] returning last value only if [condition] evaluates to a false value, otherwise the value is [null]"
  (list 'if condition null (append (list 'progn) body)))

(defmacro defun (name args &rest body)
  (setq name (module-symbol name))
  (let ((doc (str-value (append (list name) args))))
    (setq doc (js-code "('[['+d$$doc+']]')"))
    (if (and (string? (js-code "d$$body[0]"))
             (js-code "(d$$body.length>1)"))
        (setq doc (js-code "d$$doc+\"\\n\"+d$$body.splice(0,1)[0]")))
    (list 'progn
          ;; Shut up warnings about unknown function in recursive definitions
          ;; Must not set the function value unconditionally because of the
          ;; double-stage bootstrap of some basic operators (the poor version
          ;; must be available during macro expansion time of the better version)
          (list 'unless (list 'symbol-function (list 'quote name))
                (list 'set-symbol-function
                      (list 'quote name)
                      (list 'lambda args))
                ;; Sets the arglist for this function so static check
                ;; can be performed also on recursive functions
                (list 'set-arglist
                      (list 'symbol-function (list 'quote name))
                      (list 'quote args)))
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
                   "[[(defmacro name args &rest body)]]\nDefines or redefines a compile-time macro.")

(set-arglist (symbol-macro 'defmacro)
             '(name args &rest body))

(set-documentation (symbol-macro 'defun)
                   "[[(defun name args &rest body)]]\nDefines or redefines a regular function.")
(set-arglist (symbol-macro 'defun)
             '(name args &rest body))

;; Length function
(defun length (x)
  "Returns the length of string/list [x]"
  (js-code "d$$x.length"))

;; Uppercase/lowercase
(defun uppercase (x)
  "Returns the string [x] converted to uppercase"
  (js-code "d$$x.toUpperCase()"))

(defun lowercase (x)
  "Returns the string [x] converted to lowercase"
  (js-code "d$$x.toLowerCase()"))

;; Simple versions of a few operators needed for bootstrap, they will be redefined
(defun = (a b) (js-code "(d$$a===d$$b)"))
(defun < (a b) (js-code "(d$$a<d$$b)"))
(defun > (a b) (js-code "(d$$a>d$$b)"))
(defun - (a b) (js-code "(d$$a-d$$b)"))
(defun + (&rest args)
  (cond
    ((= (length args) 2) (js-code "(d$$args[0]+d$$args[1])"))
    (true (js-code "(d$$args[0]+f$$$43_.apply(null,d$$args.slice(1)))"))))

;; Logical not
(defun not (x)
  "Logical negation of [x]"
  (js-code "!d$$x"))

(defmacro not (x)
  "Logical negation of [x]"
  (list 'js-code (+ "(!" (js-compile x) ")")))

;; Error throwing
(defun error (x)
  "Throws an error message [x] (doesn't return)"
  (js-code "((function(x){throw new String(x);})(d$$x))"))

;; Function accessor
(defmacro function (x)
  "Returns the function currently bound to the unevaluated symbol [x] (including lexical bindings)"
  (js-code "(lexfunc.vars[d$$x.name]?null:(d$$$42_outgoing_calls$42_[d$$x.name]=true))")
  (list 'js-code (+ "f" (js-code "d$$x.name"))))

;; Macro accessor
(defmacro macro (x)
  "Returns the macro currently bound to the unevaluated symbol [x] (including lexical bindings)"
  (list 'or
        (list 'lexical-macro (list 'quote x))
        (list 'symbol-macro (list 'quote x))))

;; Javascript crazyness
(defun callable? (x)
  "True if [x] can be called"
  (js-code "((typeof d$$x)==='function')"))

(defun bool? (x)
  "True if and only if [x] is a boolean"
  (js-code "((typeof d$$x)==='boolean')"))

(defun undefined? (x)
  "True if and only if [x] is the undefined value"
  (js-code "((typeof d$$x)==='undefined')"))

(defun null? (x)
  "True if and only if [x] is the null value"
  (js-code "(d$$x===null)"))

(defun infinity? (x)
  "True if and only if [x] is the positive infinity value"
  (js-code "(d$$x===Infinity)"))

(defun -infinity? (x)
  "True if and only if [x] is the negative infinity value"
  (js-code "(d$$x===-Infinity)"))

(defun NaN? (x)
  "True if and only if [x] is the NaN value"
  (js-code "((typeof d$$x)==='number'&&!d$$x&&!(d$$x===0))"))

(defun object? (x)
  "True if and only if [x] is a javascript object"
  (js-code "((d$$x&&d$$x.constructor&&d$$x.constructor===Object)===true)"))

(defun zero? (x)
  "True if and only if [x] is the number zero"
  (and (number? x) (= x 0)))

(defun odd? (x)
  "True if number [x] is odd"
  (js-code "(!!(d$$x&1))"))

(defun even? (x)
  "True if number [x] is even"
  (js-code "(!(d$$x&1))"))

;; Dolist macro
(defmacro dolist (var+list &rest body)
  "Evaluates [body] forms after binding [var] to each element of [list]"
  (list 'js-code (+ "((function(list){var f="
                    (js-compile (append (list 'lambda
                                              (list (js-code "d$$var$43_list[0]")))
                                        body))
                    ";for(var i=0,n=list.length;i<n;i++){f(list[i])}})("
                    (js-compile (js-code "d$$var$43_list[1]"))
                    "))")))

;; Dotimes macro
(defmacro dotimes (var+count &rest body)
  "Evaluates [body] forms after binding [var] to 0, 1, ... [(1- count)]"
  (list 'js-code (+ "((function(n){var f="
                    (js-compile (append (list 'lambda
                                              (list (js-code "d$$var$43_count[0]")))
                                        body))
                    ";for(var i=0;i<n;i++){f(i)}})("
                    (js-compile (js-code "d$$var$43_count[1]"))
                    "))")))

;; Funcall macro
(defmacro funcall (f &rest args)
  "Calls the function object [f] passing specified values as parameters."
  (let ((res (+ (js-compile f) "("))
        (sep ""))
    (dolist (x args)
      (setq res (+ res sep (js-compile x)))
      (setq sep ","))
    (list 'js-code (+ res ")"))))

;; List-related macros (can't be defined before '+')
(defmacro length (x)
  "Returns the length of a list or string object [x]"
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
  "Adds the specified [value] to the end of [list]"
  (js-code "d$$list.push(d$$value)"))

(defmacro rest (x)
  "Returns a string or list obtained from [x] by removing first element"
  (list 'js-code (+ "(" (js-compile x) ".slice(1))")))

(defun rest (x)
  "Returns a list or string obtained from [x] by removing first element"
  (js-code "(d$$x.slice(1))"))

(defmacro splice (x start size)
  "Removes and returns [size] elements (or all remaining) from a given [start] point or from the beginning."
  (list 'js-code (+ "("
                    (js-compile x)
                    ".splice("
                    (js-compile start)
                    ","
                    (js-compile size)
                    "))")))

(defun splice (x start size)
  "Removes and returns [size] elements (or all remaining) from a given [start] point or from the beginning."
  (js-code "(d$$x.splice(d$$start,d$$size))"))

(defun insert (x i y)
  "Inserts element [y] into list [x] at index [i]"
  (js-code "(d$$x.splice(d$$i,0,d$$y),d$$y)"))

;; Indexing
(defmacro aref (x &rest indexes)
  "Returns the element of [x] indexed by the specified values."
  (let ((res (js-compile x)))
    (dolist (i indexes)
      (setq res (+ res "[" (js-compile i) "]")))
    (list 'js-code (+ "(" res ")"))))

(defun aref (x &rest indexes)
  "Returns the element of [x] indexed by the specified values."
  (dolist (i indexes)
    (setq x (aref x i)))
  x)

;; Quasiquoting
(defun bqconst (x)
  "True if the form [x] is constant in respect to backquoting"
  (if (list? x)
      (if (or (= (aref x 0) '\,)
              (= (aref x 0) '\`)
              (= (aref x 0) '\,@))
          false
          (do ((i 0 (+ i 1)))
              ((or (= i (length x)) (not (bqconst (aref x i))))
               (= i (length x)))))
      true))

(defun bquote (x)
  "Returns the backquote expansion of [x]"
  (cond
   ((or (number? x) (string? x) (= x null))
    x)
   ((bqconst x)
    (list 'quote x))
   ((list? x)
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
           ((and (list? el) (= (aref el 0) '\,@))
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
  "Defines a macro and an equivalent function. Variadic macros are not supported."
  ;; Note: Defmacro must be done at macro expansion time
  ;;       because we need the macro in place when
  ;;       defun is macroexpanded
  (setq name (module-symbol name))
  (eval `(defmacro ,name ,args ,@body))
  (let ((doc (if (and (> (length body) 1)
                      (string? (aref body 0)))
                 (splice body 0 1)
                 (list))))
    `(progn
       (defun ,name ,args (,name ,@args))
       (set-documentation (symbol-function ',name)
                          (documentation (symbol-macro ',name)))
       ',name)))

;; Utilities
(defmacro/f slice (x start end)
  "Returns elements of [x] from [start] to before [end] or all remaining if no [end] is specified. Without args does a full shallow copy."
  (cond
    ((and (undefined? start) (undefined? end))
     `(js-code ,(+ "(" (js-compile x) ").slice()")))
    ((undefined? end)
     `(js-code ,(+ "(" (js-compile x) ").slice(" (js-compile start) ")")))
    (true
     `(js-code ,(+ "(" (js-compile x) ".slice(" (js-compile start) "," (js-compile end) "))")))))
(set-arglist (symbol-function 'slice) '(x &optional start end))
(set-arglist (symbol-macro 'slice) '(x &optional start end))

(defmacro/f reverse (list)
  "Returns a copy of the elements in the opposite ordering"
  `(js-code ,(+ "(" (js-compile list) ".slice().reverse())")))

(defmacro/f nreverse (list)
  "Reverses in place the ordering of the elements"
  `(js-code ,(+ "(" (js-compile list) ".reverse())")))

(defmacro/f first (x) "First element of list/string [x]" `(aref ,x 0))
(defmacro/f second (x) "Second element of list/string [x]" `(aref ,x 1))
(defmacro/f third (x) "Third element of list/string [x]" `(aref ,x 2))
(defmacro/f fourth (x) "Fourth element of list/string [x]" `(aref ,x 3))
(defmacro/f fifth (x) "Fifth element of list/string [x]" `(aref ,x 4))
(defmacro/f sixth (x) "Sixth element of list/string [x]" `(aref ,x 5))
(defmacro/f seventh (x) "Seventh element of list/string [x]" `(aref ,x 6))
(defmacro/f eighth (x) "Eighth element of list/string [x]" `(aref ,x 7))
(defmacro/f ninth (x) "Ninth element of list/string [x]" `(aref ,x 8))
(defmacro/f tenth (x) "Tenth element of list/string [x]" `(aref ,x 9))

;; String splitting and joining
(defmacro/f split (x separator)
  "Splits a string [x] using the specified separator"
  `(js-code ,(+ "("
                (js-compile x)
                ".split("
                (js-compile separator)
                "))")))

(defmacro/f join (x separator)
  "Joins the content of list [x] of strings placing specified [separator] between elements"
  `(js-code ,(+ "("
                (js-compile x)
                ".join("
                (js-compile separator)
                "))")))

(defmacro defmathop (name comment none single jsname)
  "Defines a math operator macro with the given [comment], the value [none]
   to use in case of no operands, the value [single] in case of a single operand
   and the Javascript operator name [jsname] when more operands are present.
   For numeric or string literals operands the computation is perfomed compile time."
  `(defmacro ,name (&rest args)
     ,comment
     (let ((res (cond
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
       (when (and (list? res)
                  (= (first res) 'js-code)
                  (js-code "d$$res[1].match(/^\\(\\((-?[0-9]+\\.?[0-9]*|\"([^\"]|\\\\.)*\")\\).\\((-?[0-9]+\\.?[0-9]*|\"([^\"]|\\\\.)*\")\\)\\)$/)"))
         (js-code "(d$$res[1]=JSON.stringify(eval(d$$res[1])))"))
       res)))

(defmacro defmathop-func (name)
  "Defines a math function based on a math operator macro defined with [defmathop]."
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

;; Math n-ary operators macros and functions

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
  "Arithmetic shift left [(> count 0)] or right [(< count 0)]"
  (if (number? count)
      (if (> count 0)
          `(js-code ,(+ "((" (js-compile x) ")<<(" count "))"))
          `(js-code ,(+ "((" (js-compile x) ")>>(" (- count) "))")))
      `(js-code ,(+ "(function(x,c){return (c>0)?(x<<c):(x>>-c);})("
                    (js-compile x)
                    ","
                    (js-compile count)
                    ")"))))

;; Make symbol / gensym
(defun make-symbol (name)
  "Creates a new uninterned symbol"
  (js-code "(new Symbol(f$$mangle(d$$name)))"))

(defvar *gensym-count* 0)

(defun gensym-prefix (prefix)
  "Returns a new uninterned unique symbol using the specified [prefix]"
  (make-symbol (+ "G#" prefix "/"
                  (setq *gensym-count* (+ 1 *gensym-count*)))))

(defun gensym-noprefix ()
  "Returns a new uninterned unique symbol"
  (make-symbol (+ "G#" (setq *gensym-count* (+ 1 *gensym-count*)))))

;; Comparisons
(defmacro defrelop (name comment jsname)
  "Defines a relational operator short-circuiting macro given [name], [comment] and Javascript operator name [jsname]."
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
         `(let ((,x1 ,(aref args 0))
                (,x2 ,(aref args 1)))
            (and (,',name ,x1 ,x2) (,',name ,x2 ,@(slice args 2)))))))))

(defmacro defrelop-func (name)
  "Defines a variadic relational operator function given a corresponding macro name"
  `(progn
     (defun ,name (&rest args)
       (if (< (length args) 2)
           true
           (do ((current (first args))
                (n (length args))
                (i 1 (+ i 1)))
               ((or (= i n) (not (,name current (aref args i))))
                  (= i n))
             (setq current (aref args i)))))
     (set-documentation (symbol-function ',name)
                        (documentation (symbol-macro ',name)))
     ;; Trick... fixes macro documentation here
     (set-documentation (symbol-macro ',name)
                        (+ (documentation (symbol-macro ',name))
                           " Evaluation is short-circuiting."))))

(defrelop <  "Strictly less than comparison." "<")
(defrelop <= "Less than or equal comparison." "<=")
(defrelop =  "Equality comparison." "===")
(defrelop ~= "Equivalence comparison." "==")
(defrelop >= "Greater than or equal comparison." ">=")
(defrelop >  "Strictly greather than comparison." ">")
(defrelop-func <  )
(defrelop-func <= )
(defrelop-func =  )
(defrelop-func ~= )
(defrelop-func >= )
(defrelop-func >  )

;; /= operator is different from others as no transitivity can be used
;; (it means "arguments are all distinct")
(defmacro /= (&rest args)
  "True if and only if the passed values are all distinct. Evaluation is short-circuiting."
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
                      (setq res (+ res "if(" p "===x$" (length prev) ")return false;")))
                    (push (+ "x$" (length prev)) prev))
                  (+ res "return true;})()"))))))

(defun /= (&rest args)
  "True if and only if the passed values are all distinct."
  (if (< (length args) 2)
      true
      (do ((n (- (length args) 1))
           (i 0 (+ i 1)))
          ((or (= i n) (/= -1 (js-code "d$$args.indexOf(d$$args[d$$i],d$$i+1)")))
             (= i n)))))

(defmacro let* (bindings &rest body)
  "Evaluates the body forms in sequence by first establishing lexical/dynamic bindings
   'one at a time' so that during the evaluation of n-th binding all previous ones are
   already visible"
  (if (> (length bindings) 1)
     `(let (,(aref bindings 0))
        (let* ,(rest bindings) ,@body))
     `(let ,bindings ,@body)))

(defun symbol-macro-expand (x)
  "Returns recursive expansion of [x] until it's not a symbol or there's no
   associated [define-symbol-macro] or [symbol-macrolet]."
  (do ()
      ((or (not (symbol? x))
           (and (not (lexical-symbol-macro x))
                (not (js-code "d$$x.symbol_macro"))))
       x)
    (setq x (or (lexical-symbol-macro x)
                (js-code "d$$x.symbol_macro")))))

(defmacro setf (place value)
  "Sets the content of a place to be the specified value. A place is either a symbol
   or a form (e.g. [(aref x i)]) for which a corresponding setting form is defined
   (e.g. [(set-aref x i value)]) either as function or macro eventually after
   macro expansion."
  (setq place (symbol-macro-expand place))
  (cond
    ((symbol? place)
     `(setq ,place ,value))
    ((list? place)
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
          (error (+ "Unsupported setf place " (str-value place)))))))
    (true (error "Invalid setf place"))))

(defmacro incf (place inc)
  "Increments the content of a place by the specified increment or by 1 if not specified.
   A place is either a symbol or a form (e.g. [(aref x i)]) for which a corresponding
   increment form is defined (e.g. [(inc-aref x i inc)]) either as function or macro
   eventually after macro expansion."
  (if (= inc undefined) (setf inc 1))
  (do ()
      ((or (not (symbol? place))
           (and (not (lexical-symbol-macro place))
                (not (js-code "d$$place.symbol_macro")))))
    (setq place (or (lexical-symbol-macro place)
                    (js-code "d$$place.symbol_macro"))))
  (cond
    ((symbol? place)
     `(setq ,place (+ ,place ,inc)))
    ((list? place)
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
(set-arglist (symbol-macro 'incf) '(place &optional (inc 1)))

(defmacro decf (place dec)
  "Decrements the content of a place by the specified decrement or by 1 if not specified.
   A place is either a symbol or a form (e.g. [(aref x i)]) for which a corresponding
   decrement form is defined (e.g. [(dec-aref x i dec)]) either as function or macro
   eventually after macro expansion."
  (if (= dec undefined) (setf dec 1))
  (do ()
      ((or (not (symbol? place))
           (and (not (lexical-symbol-macro place))
                (not (js-code "d$$place.symbol_macro")))))
    (setq place (or (lexical-symbol-macro place)
                    (js-code "d$$place.symbol_macro"))))
  (cond
    ((symbol? place)
     `(setq ,place (- ,place ,dec)))
    ((list? place)
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
(set-arglist (symbol-macro 'decf) '(place &optional (dec 1)))

(defmacro set-js-code (x value)
  "Set of js literal lvalue as last resort on (setf (...))"
  `(js-code ,(+ "((" x ")=(" (js-compile value) "))")))

(defmacro inc-js-code (x delta)
  "Increment of js literal lvalue as last resort on (incf (...))"
  `(js-code ,(+ "((" x ")+=(" (js-compile delta) "))")))

(defmacro dec-js-code (x delta)
  "Decrement of js literal lvalue as last resort on (decf (...))"
  `(js-code ,(+ "((" x ")-=(" (js-compile delta) "))")))

(defmacro/f 1+ (x)
  "Returns [(+ x 1)]"
  `(+ ,x 1))

(defmacro/f 1- (x)
  "Returns [(- x 1)]"
  `(- ,x 1))

;; Sequence utilities
(defmacro/f pop (x)
  "Removes and returns last element from list [x]"
  `(js-code ,(+ "(" (js-compile x) ".pop())")))

(defmacro empty (x)
  "True if length of array or string [x] is zero"
  `(not (length ,x)))

(defun last (x)
  "Last element of list/string [x]."
  (aref x (1- (length x))))

(defmacro last (x)
  "Last element of list/string [x]."
  (setq x (symbol-macro-expand x))
  (if (symbol? x)
      `(js-code ,(+ (js-compile x) "["
                    (js-compile x) ".length-1]"))
      `(funcall #'last ,x)))

(defun reduce (f seq)
  "Reduces a sequence to a single value by repeating application of
   function [f] to pairs of elements in the sequence [seq]. For an empty
   sequence the return value is the result of calling the function
   without parameters"
  (if (= 0 (length seq))
      (funcall f)
      (let ((res (first seq)))
        (dolist (x (rest seq))
          (setf res (funcall f res x)))
        res)))

(defmacro min (&rest seq)
  "Returns the minimum value of all arguments under comparison with [<]"
  `(js-code ,(let ((code "(Math.min(")
                   (sep ""))
               (dolist (x seq)
                 (setf code (+ code sep (js-compile x)))
                 (setf sep ","))
               (+ code "))"))))
(defun min (&rest seq)
  "Returns the minimum value of all arguments under comparison with [<]"
  (js-code "(Math.min.apply(Math,d$$seq))"))

(defmacro max (&rest seq)
  "Returns the maximum value of all arguments under comparison with [>]"
  `(js-code ,(let ((code "(Math.max(")
                   (sep ""))
               (dolist (x seq)
                 (setf code (+ code sep (js-compile x)))
                 (setf sep ","))
               (+ code "))"))))
(defun max (&rest seq)
  "Returns the maximum value of all arguments under comparison with [>]"
  (js-code "(Math.max.apply(Math,d$$seq))"))

(defun map (f seq)
  "Returns the list obtained by applying function [f] to each element in [seq]"
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
  "Returns the sequence of calling function [f] passing as parameters
   corresponding elements from the sequences. The resulting list length
   is equal to the length of the shortest sequence."
  (map (lambda (args) (apply f args))
       (apply #'zip sequences)))

(defun filter (f seq)
  "Returns the subset of elements from [seq] for which the function [f] returned a logical true value"
  (let ((res (list)))
    (dolist (x seq)
      (when (funcall f x)
        (push x res)))
    res))

(defun index0 (x L)
  "Returns the index position in which [x] appears in list/string [L] or -1 if it's not present"
  (js-code "d$$L.indexOf(d$$x)"))

(defun last-index (x L)
  "Returns the last index position in which [x] appears in list/string [L] or -1 if it's not present"
  (js-code "d$$L.lastIndexOf(d$$x)"))

(defun find (x L)
  "True if element [x] is included in [L]"
  (/= -1 (index0 x L)))

(defun remove (x L)
  "Returns a copy of [L] after all instances of [x] have been removed"
  (let ((res (list)))
    (dolist (y L)
      (unless (= x y)
        (push y res)))
    res))

(defun nremove (x L)
  "Removes all elements [x] from [L] and returns how many elements have been removed"
  (let ((wp 0)
        (n (length L)))
    (dotimes (rp n)
      (unless (= (aref L rp) x)
        (setf (aref L (1- (incf wp))) (aref L rp))))
    (setf (length L) wp)
    (- n wp)))

(defun remove-first (x L)
  "Returns a copy of [L] after removing first instance of [x] if present"
  (let ((i (index0 x L)))
    (if (>= i 0)
        (append (slice L 0 i) (slice L (1+ i)))
        (slice L))))

(defun nremove-first (x L)
  "Remove first element [x] from list [L] if present. Returns true if found."
  (let ((i (index0 x L)))
    (when (>= i 0)
      (splice L i 1))
    (>= i 0)))

(defun remove-last (x L)
  "Returns a copy of [L] after removing first instance of [x] if present"
  (let ((i (last-index x L)))
    (if (>= i 0)
        (append (slice L 0 i) (slice L (1+ i)))
        (slice L))))

(defun nremove-last (x L)
  "Remove first element [x] from list [L] if present. Returns true if found."
  (let ((i (last-index x L)))
    (when (>= i 0)
      (splice L i 1))
    (>= i 0)))

(defun subset (L1 L2)
  "True if every element in [L1] is also in [L2]"
  (do ((i 0 (1+ i)))
      ((or (>= i (length L1))
           (not (find (aref L1 i) L2)))
         (>= i (length L1)))))

(defun set-union (L1 L2)
  "List of elements appearing in [L1] or in [L2]"
  (let ((res (slice L1)))
    (dolist (x L2)
      (unless (find x res)
        (push x res)))
    res))

(defun set-difference (L1 L2)
  "List of elements appearing in [L1] but not in [L2]"
  (let ((res (list)))
    (dolist (x L1)
      (unless (find x L2)
        (push x res)))
    res))

(defun set-intersection (L1 L2)
  "List of elements appearing in both [L1] and [L2]"
  (let ((res (list)))
    (dolist (x L1)
      (when (find x L2)
        (push x res)))
    res))

(defmacro/f nsort (x condition)
  "Modifies a sequence [x] inplace by sorting the elements according to the
   specified [condition] or [#'<] if no condition is given."
  (if (= condition undefined)
      `(js-code ,(+ "(" (js-compile x) ").sort(function(a,b){return a<b?-1:1;})"))
      `(js-code ,(+ "(" (js-compile x) ").sort(function(a,b){return (" (js-compile condition) ")(a,b)?-1:1;})"))))
(set-arglist (symbol-function 'nsort) '(x &optional (condition #'<)))
(set-arglist (symbol-macro 'nsort) '(x &optional (condition #'<)))

(defmacro/f sort (x condition)
  "Returns a copy of a sequence [x] with elements sorted according to the specified [condition] or [#'<] if no condition is given."
  `(nsort (slice ,x) ,condition))
(set-arglist (symbol-function 'sort) '(x &optional (condition #'<)))
(set-arglist (symbol-macro 'sort) '(x &optional (condition #'<)))

;; &optional

(defmacro argument-count ()
  "Number of arguments passed to current function"
  `(js-code "arguments.length"))

(setf (symbol-macro 'lambda)
      (let* ((oldcf (symbol-macro 'lambda))
             (olddoc (documentation oldcf))
             (f (lambda (args &rest body)
                  (let* ((doc (if (string? (first body))
                                  (js-code "d$$body.slice(0,1)")
                                  (list)))
                         (i (index0 '&optional args))
                         (r (index0 '&rest args)))
                    (if (= i -1)
                        (apply oldcf `(,args
                                       ,@doc
                                       ,@(if (> r 0)
                                             `((when (< (argument-count) ,r)
                                                 (error "Invalid number of arguments")))
                                             (list))
                                       ,@body))
                        (let ((defaults (list))
                              (checks (list))
                              (nargs (append (slice args 0 i)
                                             (slice args (1+ i)))))
                          (unless (= i 0)
                            (push `(when (< (argument-count) ,i)
                                     (error "Invalid number of arguments"))
                                  checks))
                          (when (= r -1)
                            (push `(when (> (argument-count) ,(length nargs))
                                     (error "Invalid number of arguments"))
                                  checks))
                          (dotimes (i (length nargs))
                            (when (list? (aref nargs i))
                              (push `(when (< (argument-count) ,(1+ i))
                                       (setf ,(first (aref nargs i))
                                             ,(second (aref nargs i))))
                                    defaults)
                              (setf (aref nargs i) (first (aref nargs i)))))
                          (let ((L (apply oldcf `(,nargs
                                                  ,@doc
                                                  ,@checks
                                                  ,@defaults
                                                  ,@body))))
                            `(let ((L ,L))
                               (setf (arglist L) ',args)
                               L))))))))
        (setf (documentation f) olddoc)
        (setf (arglist f) (arglist oldcf))
        f))

;; Keyword arguments

(setf (symbol-macro 'lambda)
      (let* ((oldcf (symbol-macro 'lambda))
             (oldcomm (documentation oldcf))
             (unassigned '#.(gensym-noprefix))
             (f (lambda (args &rest body)
                  (let ((i (index0 '&key args)))
                    (if (= i -1)
                        (apply oldcf (append (list args) body))
                        (let ((rest '#.(gensym-prefix "rest"))
                              (nrest '#.(gensym-prefix "nrest"))
                              (ix '#.(gensym-prefix "ix")))
                          (unless (= -1 (index0 '&rest args))
                            (error "&key and &rest are incompatible"))
                          (apply oldcf `((,@(slice args 0 i) &rest ,rest)
                                         (let ((,nrest (length ,rest))
                                               ,@(map (lambda (x)
                                                        (if (list? x)
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
                                                              ,(intern (+ ":" (symbol-name (if (list? x) (first x) x)))))
                                                           (setf ,(if (list? x) (first x) x) (aref ,rest (1+ ,ix)))))
                                                       (slice args (1+ i)))
                                                  `((true (error "Invalid parameters"))))))
                                           ,@(let ((res (list)))
                                               (dolist (x (slice args (1+ i)))
                                                 (when (list? x)
                                                   (push `(when (= ,(first x) ',unassigned)
                                                            (setf ,(first x) ,(second x)))
                                                         res)))
                                               res)
                                           ,@body)))))))))
        (setf (documentation f) oldcomm)
        (setf (arglist f) (arglist oldcf))
        f))

;; Destructuring

(setf (symbol-macro 'lambda)
      (let* ((oldcf (symbol-macro 'lambda))
             (olddoc (documentation oldcf))
             (f (lambda (args &rest body)
                  (let* ((doc (if (string? (first body))
                                  (js-code "d$$body.slice(0,1)")
                                  (list)))
                         (dslist (list))
                         (oargs (slice args)))
                    (do ((n (length args))
                         (i 0 (1+ i)))
                        ((or (= i n)
                             (and (symbol? (aref args i))
                                  (= "&" (aref (symbol-name (aref args i)) 0)))))
                      (when (list? (aref args i))
                        (let ((tname (gensym-noprefix)))
                          (push (list tname (aref args i)) dslist)
                          (setf (aref args i) tname))))
                    (when (> (length dslist) 0)
                      (labels ((expand (expr template)
                                 (cond
                                   ((symbol? template)
                                    `((,template ,expr)))
                                   ((list? template)
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
                    (let ((L (apply oldcf `(,args ,@doc ,@body))))
                      (setf (arglist L) oargs)
                      L)))))
        (setf (documentation f) olddoc)
        (setf (arglist f) (arglist oldcf))
        f))

;; Array construction

(defun make-array (n &optional initial-value)
  "Creates a list containing [n] elements all equal to the specified [initial-value].
   Passing a list of dimensions as [n] creates a multidimensional matrix."
  (unless (list? n)
    (setf n (list n)))
  (let ((sz (length n)))
    (cond
      ((= sz 0)
       initial-value)
      ((= sz 1)
       (let ((result (list)))
         (dotimes (i (first n))
           (setf (aref result i) initial-value))
         result))
      (true
       (let ((result (list)))
         (dotimes (i (first n))
           (setf (aref result i)
                 (make-array (rest n) initial-value)))
         result)))))

(defmacro make-array (n &optional initial-value)
  (if (and (number? n)
           (< n 100)
           (or (number? initial-value)
               (string? initial-value)
               (undefined? initial-value)))
      `(list ,@(funcall #'make-array n initial-value))
      `(funcall #'make-array ,n ,initial-value)))

;; Range

(defun range (start &optional stop step)
  "Returns a list containing all numbers from [start] (0 if not specified) up to [stop] counting by [step] (1 if not specified).
If only one parameter is passed it's assumed to be [stop]."
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

(defun fp-range (from to n)
  "A list of [n] equally spaced floating point values starting with [from] and ending with [to]."
  (let ((result (list)))
    (dotimes (i (1- n))
      (push (+ from (/ (* i (- to from)) (1- n)))
            result))
    (push to result)
    result))

;; Generic gensym
(defmacro gensym (&optional prefix)
  "Generates an unique symbol optionally named with the specified [prefix]."
  (if prefix
      `(gensym-prefix ,prefix)
      `(gensym-noprefix)))

;; Generic index
(defmacro index (x seq &optional start)
  "Returns the index of element [x] in [seq] or -1 if not present, eventually starting from the specified [start] position."
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
  "Returns the index of element [x] in [seq] or -1 if not present, eventually starting from the specified [start] position"
  (if start
      (js-code "(d$$seq.indexOf(d$$x,d$$start))")
      (js-code "(d$$seq.indexOf(d$$x))")))

;; Any/all
(defmacro any ((var list) &rest body)
  "Returns the first logical true evaluation of [body] forms after binding [var] to [list] elements or [null] if none
   every evaluations returns a logical false value"
  (let ((index (gensym))
        (L (gensym))
        (result (gensym)))
    `(do ((,L ,list)
          (,result null)
          (,index 0 (1+ ,index)))
         ((or (>= ,index (length ,L))
              (let ((,var (aref ,L ,index)))
                (setf ,result (progn ,@body))))
          ,result))))

(defmacro all ((var list) &rest body)
  "True if after binding [var] to each of the values in [list] the [body] forms always evaluate to a true value"
  `(not (any (,var ,list) (not (progn ,@body)))))

;; Reader customization
(defmacro reader (char)
  "The reader function associated to [char]. A setf-able place."
  (unless (and (string? char) (= (length char) 1))
    (error "(reader x) requires a one-char literal"))
  `(aref *readers* ,char))

(defmacro hash-reader (char)
  "The reader function associated to [char] when following [\"#\"]. A setf-able place."
  (unless (and (string? char) (= (length char) 1))
    (error "(hash-reader x) requires a one-char literal"))
  `(aref *hash-readers* ,char))

(defun next-char (src)
  "Advances to next character or character source [src]"
  (js-code "(d$$src.i++)"))

(defun current-char (src)
  "Returns current character from character source [src]"
  (js-code "(d$$src.s[d$$src.i])"))

;; String interpolation reader
(setf (reader "~")
      (lambda (src)
        (next-char src)
        (if (= (current-char src) "\"")
            (let ((x (parse-value src))
                  (pieces (list))  ; list of parts
                  (part "")        ; current part
                  (expr false)     ; is current part an expression?
                  (escape false))  ; is next char verbatim ?
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
                (true `(+ ,@pieces))))
            (parse-symbol src "~"))))

;; Computed symbol reader macro #"foo{(+ 1 2)}" --> foo3
(setf (hash-reader "\"")
      (lambda (src)
        (let ((x (parse-value src)))
          `(intern ,(parse-value ~"~{(str-value x)}")))))

;; Math functions
(defmacro/f sqrt (x) "Square root of [x]"
  `(js-code ,(+ "Math.sqrt(" (js-compile x) ")")))
(defmacro/f sin (x) "Sine of angle [x] in radians"
  `(js-code ,(+ "Math.sin(" (js-compile x) ")")))
(defmacro/f cos (x) "Cosine of angle [x] in radians"
  `(js-code ,(+ "Math.cos(" (js-compile x) ")")))
(defmacro/f tan (x) "Tangent of angle [x] in radians"
  `(js-code ,(+ "Math.tan(" (js-compile x) ")")))
(defmacro/f exp (x) "[e=2.718281828...] raised to power of [x]"
  `(js-code ,(+ "Math.exp(" (js-compile x) ")")))
(defmacro/f log (x) "Natural logarithm of [x]"
  `(js-code ,(+ "Math.log(" (js-compile x) ")")))
(defmacro/f atan (x) "Arc-tangent in radians of the value [x]"
  `(js-code ,(+ "Math.atan(" (js-compile x) ")")))
(defmacro/f floor (x) "Biggest integer that is not bigger than [x]"
  `(js-code ,(+ "Math.floor(" (js-compile x) ")")))
(defmacro/f round (x) "Integer closest to [x]"
  `(js-code ,(+ "Math.round(" (js-compile x) ")")))
(defmacro/f ceil (x) "Smallest integer that is not smaller than [x]"
  `(js-code ,(+ "Math.ceil(" (js-compile x) ")")))
(defmacro/f abs (x) "Absolute value of [x]"
  `(js-code ,(+ "Math.abs(" (js-compile x) ")")))
(defmacro/f atan2 (y x) "Arc-tangent of [y/x] in radians with proper handling of all quadrants"
  `(js-code ,(+ "Math.atan2(" (js-compile y) "," (js-compile x) ")")))
(setq pi (js-code "Math.PI"))
(setq 2pi (* 2 pi))

;; Swap

(defmacro swap (a b)
  "Swaps the content of two places (evaluating each place twice: once for reading and once for writing)"
  (let ((xa (gensym))
        (xb (gensym)))
    `(let ((,xa ,a)
           (,xb ,b))
       (setf ,a ,xb)
       (setf ,b ,xa)
       null)))

;; Random

(defmacro random ()
  "Random number between [0 < x < 1]"
  `(js-code "(Math.random())"))

(defun random-int (n)
  "Random integer number 0 [<= x < n]"
  (ash (* (random) n) 0))

(defun random-shuffle (L)
  "Randomly shuffles an array [L] inplace and returns null"
  (dotimes (i (length L))
    (let ((j (random-int (length L))))
      (swap (aref L i) (aref L j)))))

(defun random-shuffled (L)
  "Returns a randomly shuffled version of an array [L]"
  (let ((x (slice L)))
    (random-shuffle x)
    x))

;; Filler string

(defun filler (n &optional (c " "))
  "Returns a string composed by replicating a specified string (a space by default)"
  (cond
    ((<= n 0) "")
    ((= n 1) c)
    ((logand n 1) (+ c (filler (1- n) c)))
    (true
       (let ((h (filler (ash n -1) c)))
         (+ h h)))))

;; JS exception support
(defvar *exception* null)
(defmacro try (expr on-error)
  "Evaluates [expr] and in case of exception evaluates the [on-error] form setting [*exception*] to the current exception"
  `(js-code ,(+ "((function(){try{return("
                (js-compile expr)
                ");}catch(err){var olderr=d$$$42_exception$42_;d$$$42_exception$42_=err;var res=("
                (js-compile on-error)
                ");d$$$42_exception$42_=olderr;return res;}})())")))

;; Timing
(defun clock ()
  "Returns the number of millisecond passed since 00:00:00.000 of January 1st, 1970"
  (js-code "(new Date).getTime()"))

(defmacro time (&rest body)
  "Measures and returns the number of millisecond needed for the evaluation of all [body] forms in sequence"
  (let ((start (gensym)))
    `(let ((,start (clock)))
       ,@body
       (- (clock) ,start))))

;; Regular expression
(defun regexp (x &optional options)
  "Returns a new Javascript regular expression object"
  (js-code "(new RegExp(d$$x,d$$options||\"\"))"))

(defun replace (x a b)
  "Replaces regular expression [a] with [b] in [x]. When using a string as regexp assumes global replacement."
  (if (string? a)
      (js-code "d$$x.replace(new RegExp(d$$a,'g'), d$$b)")
      (js-code "d$$x.replace(d$$a, d$$b)")))

(defun regexp-escape (x)
  "Returns a string with all regexp-meaningful characters escaped"
  (replace x "([$^\\][()+*?\\\\])" "\\$1"))

;; Whitespace stripping
(defun lstrip (x)
  "Removes initial spaces from string [x]"
  (replace x "^\\s+" ""))

(defun rstrip (x)
  "Removes final spaces from string [x]"
  (replace x "\\s+$" ""))

(defun strip (x)
  "Remove both initial and final spaces from string [x]"
  (replace x (regexp "^\\s*(.*?)\\s*$") "$1"))

;; Anonymous JS object access/creation
(defun valid-js-name (x)
  "True if and only if string [x] is a valid identifier for Javascript"
  (not (not (js-code "d$$x.match(/^[a-zA-Z_$][a-zA-Z_$0-9]*$/)"))))

(defmacro . (obj &rest fields)
  "Returns the javascript object value selected by traversing the specified chain of [fields].
A field is either an unevaluated symbol, a number, a string or an (evaluated) form."
  (let ((res (js-compile obj)))
    (dolist (x fields)
      (setf res (cond
                  ((symbol? x)
                   (if (valid-js-name (symbol-name x))
                       (+ res "." (symbol-name x))
                       (+ res "[" (str-value (symbol-name x)) "]")))
                  ((list? x) (+ res "[" (js-compile x) "]"))
                  (true (+ res "[" (str-value x) "]")))))
    `(js-code ,res)))

;; Funcall specialization for (funcall (. a b) ...) to avoid wrapper creation
(setf (symbol-macro 'funcall)
      (let ((om (symbol-macro 'funcall)))
        (lambda (f &rest args)
          (if (and (list? f)
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

(defmacro js-object (&rest body)
  "Creates a javascript object and eventually assigns fields in [body].
Each field is either a list (name value) or just a name (associated to null).
A name is either an unevaluated atom or an evaluated list."
  (if (any (f body) (and (list? f) (list? (first f))))
      (let ((self (gensym)))
        ;; Dynamic field name creation, cannot compile to a
        ;; javascript literal
        `(let ((,self (js-code "({})")))
           ,@(let ((res (list)))
                  (dolist (f body)
                    (if (list f)
                        (push `(setf (. ,self ,(first f)) ,(second f)) res)
                        (push `(setf (. ,self ,f null)) res)))
                  res)
           ,self))
      (let ((res "({"))
        (dolist (f body)
          (unless (list? f)
            (setf f (list f null)))
          (incf res (+ (str-value (if (symbol? (first f))
                                      (symbol-name (first f))
                                      (first f)))
                       ":"
                       (js-compile (second f))
                       ",")))
        `(js-code ,(+ res "})")))))

;; hash-reader macro for object creation literal #((x 10) (y 20))
(setf (hash-reader "(")
      (lambda (src)
        (next-char src)
        `(js-object ,@(parse-delimited-list src ")"))))

(defun keys (obj)
  "Returns a list of all keys defined in the specified javascript object [obj]."
  (js-code "((function(){var res=[];for(var $i in d$$obj)res.push($i);return res})())"))

(defun remove-key (object key)
  "Removes [key] entry from [object]"
  (js-code "((function(){delete d$$object[d$$key]})())"))

;; dot reader

(setf #'parse-value
      (let ((oldf #'parse-value))
        (lambda (src)
          (when (string? src)
            (setf src (make-source src)))
          (if (= (current-char src) ".")
              (progn
                (next-char src)
                '.)
              (do ((x (funcall oldf src)))
                  ((/= (current-char src) ".") x)
                (next-char src)
                (setf x `(. ,x ,(funcall oldf src))))))))

(incf *stopchars* ".")

;; shallow-copy

(defun copy (x)
  "Returns a shallow copy of atoms, lists, nameless javascript objects, named objects. It doesn't handle recursive data structures."
  (cond
    ((list? x)
     (slice x))
    ((and x x.%copy (callable? x.%copy))
     (x.%copy x))
    ((object? x)
     (let ((res (js-object)))
       (dolist (k (keys x))
         (when ((. x "hasOwnProperty") k)
           (setf (aref res k) (aref x k))))
       res))
    (true x)))

;; An empty function needed for deploy machinery to ensure that
;; all necessary objects will be included in the output
(defmacro deploy-ref (&rest args)
  "Just compiles but ignores arguments and returns [null]. Internal use only."
  (js-compile `(progn ,@args)) ;; Trows away compiled code!
  'null)

;; Named JS object creation

(defvar *constructors* #())

(defmacro defobject (name fields)
  "Defines a new object type named [name] and with specified
   [fields]. Each field is either a symbol or a list of a symbol and a
   default value. This macro will define a function named [new-{name}]
   as the object type working as a positional constructor, a function
   named [make-{name}] as a keyword constructor and a function named
   [{name}?] as a type-testing predicate.  Each instance will inherit
   a [%class] field containing a list with the class name and class
   field names and [%getters] and [%setters] fields contaning
   getters/setters definitions."
  (setf name (module-symbol name))
  (let* ((fieldnames (map (lambda (x)
                            (if (list? x)
                                (first x)
                                x))
                          fields))
         (class (append (list (symbol-full-name name))
                        (map #'symbol-name fieldnames))))
    `(progn
       (defun ,(intern ~"{name}-constructor") ,fieldnames
         ,@(map (lambda (f)
                  (if (valid-js-name (symbol-name f))
                      `(js-code ,~"this.{(symbol-name f)}=d{(. f name)}")
                      `(js-code ,~"this[{(str-value (symbol-name f))}]=d{(. f name)}")))
                fieldnames)
         (js-code "this"))
       (defun ,(intern ~"new-{name}") (&optional ,@fields)
         ,~"Creates a new instance of {name}"
         ;; Next line is a NOP but needed for deploy machinery
         (deploy-ref (function ,#"{name}-constructor")
                     ',class)
         (js-code ,(let ((res "(new f")
                         (sep ""))
                        (incf res (. (intern ~"{name}-constructor") name))
                        (incf res "(")
                        (dolist (f fieldnames)
                          (incf res sep)
                          (incf res "d")
                          (incf res (. f name))
                          (setf sep ","))
                        (incf res "))")
                        res)))
       (let ((prototype (. #',(intern ~"{name}-constructor") prototype)))
         (setf (aref *constructors* ,(symbol-full-name name))
               (list ',(map #'symbol-name fieldnames)
                     #',#"new-{name}"))
         (setf prototype.%class ',class)
         (setf prototype.%getters (list))
         (setf prototype.%setters (list))
         (setf prototype.%copy
               (lambda ()
                 (js-code ,(let ((res "(new this.constructor(")
                                 (sep ""))
                                (dolist (f fieldnames)
                                  (incf res sep)
                                  (incf res (js-compile `(. (js-code "this") ,f)))
                                  (setf sep ","))
                                (+ res "))"))))))
       (defun ,(intern ~"{name}?") (x)
         ,~"True if and only if [x] is an instance of [{name}]"
         ;; Next line is a NOP but needed for deploy machinery
         (deploy-ref (function ,#"new-{name}")
                     ',class)
         (if (and x (= (. x %class) ',class)) true false))
       (defun ,(intern ~"make-{name}") (&key ,@fields)
         ,~"Creates a new instance of {name}"
         (,(intern ~"new-{name}") ,@fieldnames))
       ',name)))

(defmacro defgetter (class name &rest body)
  "Defines a getter for a [class] with specified [name] and implementation [body].
   In [body] forms the symbol [this] is available to refer to current instance."
  (let ((lf (gensym))
        (proto (gensym)))
    `(let ((,lf (lambda () (symbol-macrolet ((this (js-code "this")))
                             ,@body)))
           (,proto (function ,#"{class}-constructor")."prototype"))
       (push (list ,(symbol-name name) ,lf)
             (. ,proto "%getters"))
       ((. ,proto "__defineGetter__") ,(symbol-name name) ,lf)
       ',name)))

(defmacro defsetter (class name &rest body)
  "Defines a setter for a [class] with specified [name] and implementation [body].
   In [body] forms the symbol [this] is available to refer to current instance
   and the symbol [value] is bound to the value being written."
  (let ((lf (gensym))
        (proto (gensym)))
    `(let ((,lf (lambda (value) (symbol-macrolet ((this (js-code "this")))
                                  ,@body)))
           (,proto (function ,#"{class}-constructor")."prototype"))
       (push (list ,(symbol-name name) ,lf)
             (. ,proto "%setters"))
       ((. ,proto "__defineSetter__") ,(symbol-name name) ,lf)
       ',name)))

(defmacro deftuple (name fields)
  "Defines an object with specified class [name] and [fields] and also
   ensures that instances can be considered as lists for apply/map.
   The constructor new-{name} is also abbreviated as {name}."
  `(progn
     (defobject ,name ,fields)
     ,@(map (lambda (i)
              `(defgetter ,name ,(intern (str-value i))
                 (. this ,(aref fields i))))
            (range (length fields)))
     ,@(map (lambda (i)
              `(defsetter ,name ,(intern (str-value i))
                 (setf (. this ,(aref fields i)) value)))
            (range (length fields)))
     (defgetter ,name length ,(length fields))
     (defmacro ,name (&optional ,@fields)
       ,~"Creates an instance of tuple [{name}]"
       (list ',#"new-{name}" ,@fields))))

;; Char <-> numeric code conversion
(defun char (x)
  "Character associated to code [x]"
  (js-code "String.fromCharCode(d$$x)"))

(defun char-code (x)
  "Numeric code of character [x]"
  (js-code "d$$x.charCodeAt(0)"))

;; Case
(defmacro case (expr &rest cases)
  "Evaluates [expr] and the form associated to a case with a matching value or
   associated to [otherwise] symbol. Each of the [cases] is a list in which the
   first element is a matching value or the symbol [otherwise] and remaining
   elements are considered an implicit [progn] form. If no case matches and there
   is no [otherwise] case the result is [null]."
  (let ((v (gensym)))
    `(let ((,v ,expr))
       (cond
         ,@(map (lambda (c)
                  (if (= (first c) 'otherwise)
                      `(true ,@(rest c))
                      `((= ,v ,(first c))
                        ,@(rest c))))
            cases)))))

(defmacro ecase (expr &rest cases)
  "Evaluates [expr] and the form associated to a case with a matching value. Each of
   the [cases] is a list in which the first element is a matching value and remaining
   elements are considered an implicit [progn] form. If no case matches an error
   is raised."
  `(case ,expr ,@cases (otherwise (error "No matching case"))))

;; Loose number parsing
(defmacro/f atof (s)
  "Returns a float from the start of the specified string (NaN if fails)"
  `(js-code ,(+ "parseFloat(" (js-compile s) ")")))

(defmacro/f atoi (s)
  "Returns an integer from the start of the specified string (NaN if fails)"
  `(js-code ,(+ "parseInt(" (js-compile s) ")")))

;; Serialization (when str-value/load/eval is not appropriate, e.g. for rpc)

(defobject serialization-out (buf seen add backref))

(defun serialization-out ()
  "Creates a new serialization output on which .add and .backref can be called"
  (let ((so (new-serialization-out "" (list))))
    (setf so.add (lambda (x) (incf so.buf x)))
    (setf so.backref (lambda (x)
                       (let ((ix (index x so.seen)))
                         (if (>= ix 0)
                             (let ((six (+ ix "")))
                               (so.add (+ "r" (char (+ 64 (length six))) six))
                               true)
                             (progn
                               (push x so.seen)
                               false)))))
    so))

(defun serialize (so x)
  "Writes object [x] to specified serialization out object [so]"
  (cond
    ((number? x)
     (let ((sx (+ x "")))
       (so.add (+ "n" (char (+ 64 (length sx))) sx))))
    ((string? x)
     (let ((sx (+ (length x) "")))
       (so.add (+ "s" (char (+ 64 (length sx))) sx x))))
    ((bool? x)
     (so.add (if x "T" "F")))
    ((list? x)
     (unless (so.backref x)
       (let ((sx (+ (length x) "")))
         (so.add (+ "l" (char (+ 64 (length sx))) sx))
         (dolist (item x) (serialize so item)))))
    ((null? x) (so.add "@"))
    ((undefined? x) (so.add "?"))
    ((object? x)
     (unless (so.backref x)
       (let* ((keys (keys x))
              (sz (length keys))
              (ssz (+ sz "")))
         (so.add (+ "O" (char (+ 64 (length ssz))) ssz))
         (dolist (k keys)
           (serialize so k)
           (serialize so (aref x k))))))
    (true (error ~"Unable to serialize {x}"))))

(defun serialize-tag (so x)
  "Adds a tag string [x] for a custom object"
  (so.add (+ "t" (char (+ 64 (length x))) x)))

(defun to-buffer (x)
  "Serializes object [x] into a string. Supported objects are [null],
   [undefined], numbers, strings, lists of supported objects and objects
   supporting serialization where all fields contain supported objects.
   Reference loops in lists and objects are supported and loops are rebuilt
   on deserialization. Note that symbols are NOT supported."
  (let ((so (serialization-out)))
    (serialize so x)
    so.buf))

(defobject deserialization-in (buf p seen))

(defun deserialize-tag (si tag)
  "Deserializes custom object associated to [tag]"
  (error ~"Unable to deserialize {tag}"))

(defun deserialize (si)
  "Reads a value from a deserialization in object"
  (labels ((next ()
             (aref si.buf (1- (incf si.p))))
           (size ()
             (- (char-code (next)) 64))
           (get (sz)
             (let ((p0 si.p))
               (slice si.buf p0 (incf si.p sz)))))
    (case (next)
      ("n" (atof (get (size))))
      ("l" (let ((res (list)))
             (push res si.seen)
             (dotimes (i (atoi (get (size))))
               (push (deserialize si) res))
             res))
      ("s" (get (atoi (get (size)))))
      ("r" (aref si.seen (atoi (get (size)))))
      ("t" (deserialize-tag si (get (size))))
      ("?" undefined)
      ("@" null)
      ("T" true)
      ("F" false)
      ("O" (let ((res #()))
             (push res si.seen)
             (dotimes (i (atoi (get (size))))
               (let ((k (deserialize si))
                     (v (deserialize si)))
                 (setf (aref res k) v)))
             res))
      (otherwise (error "Invalid serialized object")))))

(defun from-buffer (x)
  "Deserializes an object from a string [x]"
  (let* ((si (new-deserialization-in x 0 (list)))
         (res (deserialize si)))
    (unless (= si.buf.length si.p)
      (error "Invalid serialized object"))
    res))

;; Extending defobject macro to support serialization
(setf (symbol-macro 'defobject)
      (let* ((old (symbol-macro 'defobject))
             (old-doc (documentation old))
             (old-args (arglist old))
             (so '#.(gensym))
             (newm (lambda (name fields)
                     (let ((x (gensym))
                           (fnames (map (lambda (f)
                                          (if (list? f)
                                              (first f)
                                              f))
                                        fields)))
                       `(progn
                          ,(funcall old name fields)
                          (defmethod serialize (,so ,x) (,#"{name}?" ,x)
                                     (unless ((. ,so backref) ,x)
                                       (serialize-tag ,so ,(symbol-name name))
                                       ,@(map (lambda (f)
                                                `(serialize ,so (. ,x ,f)))
                                              fnames)))
                          (defmethod deserialize-tag (,so tag) (= tag ,(symbol-name name))
                                     (let ((,x (,#"new-{name}")))
                                       (push ,x (. ,so seen))
                                       ,@(map (lambda (f)
                                                `(setf (. ,x ,f) (deserialize ,so)))
                                              fnames)
                                       ,x))
                          ',name)))))
        (setf (documentation newm) old-doc)
        (setf (arglist newm) old-args)
        newm))

;; JSON serialization

(defun json (x)
  "Standard JSON serialization of [x]"
  (or (js-code "(JSON.stringify(d$$x))") "null"))

(defun json-parse (x)
  "Standard JSON parsing of string [x]"
  (js-code "(JSON.parse(d$$x))"))

;; Class-aware JSON serialization

(defun json* (x)
  "A json-formatted string representation of object x. No loops, undefined, infinity, NaN or symbols allowed.
   Named objects get an extra %class field used to find constructor on parsing."
  (cond
    ((list? x)
     (+ "[" (join (map #'json* x) ",") "]"))
    ((and x x.%class)
     (+ "{"
        (join (append (list (+ "\"%class\":" (json (first x.%class))))
                      (map (lambda (k)
                             ~"{(json k)}:{(json* (aref x k))}")
                           (rest x.%class)))
              ",")
        "}"))
    (true (json x))))

(defun json-parse* (x)
  "Rebuilds an object from a [json*] string [x]"
  (labels ((fix (x)
             (cond
               ((list? x)
                (map #'fix x))
               ((and x (object? x) x.%class)
                (let ((cs (aref *constructors* x.%class)))
                  (if cs
                      (apply (second cs)
                             (map (lambda (k)
                                    (fix (aref x k)))
                                  (first cs)))
                      (error ~"Unable to parse named object {x.%class}"))))
               (true x))))
    (fix (json-parse x))))

;; Javscript simple function/method binding
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

(defun htm (x)
  "Escapes characters so that the content string x can be displayed correctly as HTML"
  (setf x (replace x "&" "&amp;"))
  (setf x (replace x "<" "&lt;"))
  (setf x (replace x ">" "&gt;"))
  (setf x (replace x "\"" "&quot;"))
  x)

;; DOM
(define-symbol-macro document (js-code "document"))
(define-symbol-macro window (js-code "window"))
(defmacro get-element-by-id (id)
  "Returns the DOM element with the specified id value"
  `(js-code ,(+ "(document.getElementById(" (js-compile id) "))")))
(defmacro create-element (type)
  "Creates a new DOM element with the specified type passed as a string"
  `(js-code ,(+ "(document.createElement(" (js-compile type) "))")))
(defmacro append-child (parent child)
  "Appends the DOM element [child] as last (frontmost) children of the [parent] DOM element"
  `(js-code ,(+ "(" (js-compile parent) ".appendChild(" (js-compile child) "))")))
(defmacro remove-child (parent child)
  "Removes the DOM element [child] from the list of children of [parent] DOM element"
  `(js-code ,(+ "(" (js-compile parent) ".removeChild(" (js-compile child) "))")))

;; Timer events
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

;; Line split utility
(defun maplines (f str)
  "Calls a function for each line in a string"
  (do ((i 0)
       (n (length str)))
      ((>= i n))
    (let ((j (index "\n" str i)))
      (when (= j -1)
        (setf j (1+ n)))
      (funcall f (slice str i j))
      (setf i (1+ j)))))

(defmacro dolines ((var str) &rest body)
  "Executes a body for each line of a string"
  `(maplines (lambda (,var) ,@body) ,str))

;; Round formatting
(defun to-fixed (x n)
  "Formats a number using the specified number of decimals"
  (js-code "(d$$x.toFixed(d$$n))"))

;; Javascript blocking interaction
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

;; documentation support
(defmacro help (name)
  "Displays any documentation for compile specialization, macro, function or value bound to the (unevaluated) specified symbol."
  (labels ((doc (x)
             (unless (string? x)
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
  "Non-local control transfer to the specified [tagname] of a [tagbody] form"
  `(js-code ,(+ "((function(){throw "
                "$tag$" (mangle (symbol-name tagname))
                ";})())")))

(defmacro tagbody (&rest body)
  "Evaluates [body] forms. When an element of [body] is a symbol it's interpreted as
   a tag name to which a [go] form can transfer control. Tag names can be captured by
   closures but control transfer cannot be executed once the [tagbody] form ends."
  (let ((tagdecl "")
        (eelist "")
        (sep "var $tag$=null,")
        (eesep "")
        (fragments (list (list))))
    (dolist (x body)
      (if (symbol? x)
          (progn
            (setf tagdecl (+ tagdecl sep "$tag$" (mangle (symbol-name x)) "=[]"))
            (setf sep ",")
            (setf eelist (+ eelist eesep "$ee$===$tag$" (mangle (symbol-name x))))
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
  "Exits the closest enclosing [block] with the specified [name] returning the
   specified [value] or [undefined]"
  (when (undefined? (aref ret@ name))
    (error ~"(return-from {name} ...) can be used only inside (block {name} ...)"))
  (setf (aref ret@ name) true)
  `(progn
     (setf ,(intern ~"{name}@result") ,value)
     (go ,(intern ~"ret@{name}"))))

(defmacro return (&optional value)
  "Exits the closest enclosing [block] returning the specified [value] or [undefined]"
  `(return-from null ,value))

(defmacro block (name &rest body)
  "Defines a lexical block with the specified [name] that allows nested
   [return] or [return-from] forms."
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

(let ((odoc (documentation (symbol-macro 'defun)))
      (oargs (arglist (symbol-macro 'defun))))
  (setf (symbol-macro 'defun)
        (let ((om (symbol-macro 'defun)))
          (lambda (name args &rest body)
            (let ((doc (if (and (> (length body) 1)
                                (string? (first body)))
                           (js-code "d$$body.splice(0,1)")
                           (list))))
              (apply om `(,name ,args ,@doc (block ,name ,@body)))))))
  (setf (documentation (symbol-macro 'defun) odoc))
  (setf (arglist (symbol-macro 'defun)) oargs))

(let* ((om (symbol-macro 'lambda))
       (odoc (documentation om))
       (oargs (arglist om)))
  (setf (symbol-macro 'lambda)
        (lambda (args &rest body)
          (let ((doc (if (and (> (length body) 1)
                              (string? (first body)))
                         (js-code "d$$body.splice(0,1)")
                         (list))))
            (apply om `(,args
                        ,@doc
                        ,(append `(block null) body))))))
  (setf (documentation (symbol-macro 'lambda)) odoc)
  (setf (arglist (symbol-macro 'lambda)) oargs))

(let* ((om (symbol-macro 'labels))
       (odoc (documentation om))
       (oargs (arglist om)))
  (setf (symbol-macro 'labels)
        (lambda (defs &rest body)
          (apply om (append (list (map (lambda (def)
                                         (let ((name (first def))
                                               (args (second def))
                                               (doc (list))
                                               (body (slice def 2)))
                                           (when (and (> (length body) 1)
                                                      (string? (first body)))
                                             (setf doc (list (first body)))
                                             (setf body (rest body)))
                                           `(,name ,args ,@doc (block ,name ,@body))))
                                       defs))
                            body))))
  (setf (documentation (symbol-macro 'labels)) odoc)
  (setf (arglist (symbol-macro 'labels)) oargs))

;; Unwind-protect

(defmacro unwind-protect (form &rest body)
  "Evaluates all [body] forms even if an non-local exit jump is done during evaluation of [form]"
  `(js-code ,(+ "((function(){var cl=function(){"
                (js-compile `(progn ,@body))
                "};try{var res="
                (js-compile form)
                ";cl();return res}catch(res){cl();throw res;}})())")))

;; Throw-catch

(defmacro throw (tag x)
  "Throws the value of form [x] so that last established [catch] for the specified [tag] receives the value"
  `(js-code ,(+ "((function(){throw["
                (js-compile tag)
                ","
                (js-compile x)
                "]})())")))

(defmacro catch (tag &rest body)
  "Evaluates [body] forms and returns values of last of them or a value is [throw]n at the specified [tag]"
  `(js-code ,(+ "((function(){try{return "
                (js-compile `(progn ,@body))
                "}catch(x){if(f$$list$63_(x)&&x[0]===("
                (js-compile tag)
                "))return x[1];throw x;}})())")))

;; Generic condition dispatching
(defmacro defmethod (name args test &rest body)
  "Defines a conditioned implementation of a function; when [test] doesn't match the previous implementation is called instead"
  (let ((of (gensym)))
    `(progn
       ,@(if (symbol-function name)
             (list)
             `((defun ,name ,args
                 (error ,~"No matching method [{name}]"))))
       (setf #',of #',name)
       (setf #',name
             (lambda ,args
               (block ,name
                 (if ,test
                     (progn ,@body)
                     (,of ,@args)))))
       (list ',name ',test))))

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
                         (intern (+ ":" (symbol-name (if (list? kwa)
                                                         (first kwa)
                                                         kwa)))))
                       (slice args ai))))
        (do ()
            ((>= fi fn))
          (let ((k (aref form fi)))
            (when (and (symbol? k)
                       (= ":" (aref (symbol-name k) 0)))
              (unless (find k keys)
                (warning ~"Invalid keyword parameter {k} in {(str-value form)}"))))
          (incf fi 2))))))

;; Defconstant
(defmacro defconstant (name value)
  "Defines a constant value associated with the global variable identified by name"
  `(progn
     (setf (symbol-value ',name) ,value)
     (setf (. ',name constant) true)))

;; Define/undefine symbol-macro
(defmacro define-symbol-macro (x y)
  "Defines a global symbol macro replacing symbol [x] in data position with form [y]"
  `(setf (. ',x symbol_macro) ',y))

(defmacro undefine-symbol-macro (x)
  "Removes global symbol macro definition for symbol [x]"
  `(js-code ,(+ "((function(){delete s"
                (js-code "d$$x.name")
                ".symbol_macro;})())")))

;; Module support

(defvar *exports* (list))

(defmacro export (&rest symbols)
  "Lists specified [symbols] as to be exported to who uses [(import * from <module>)] import syntax.
   A string literal in the [symbols] list is assumed to be a start-with match for symbols already
   interned in the module (therefore the [export] form should be the last form of the module)."
  (let ((fullsymbols (filter #'symbol? symbols))
        (wildcards (filter #'string? symbols)))
    `(progn
       ,@(map (lambda (n)
                `(push ',(intern (symbol-name n) *current-module*)
                       ,(intern "*exports*" *current-module*)))
              fullsymbols)
       ,@(let ((res (list)))
              (when (> (length wildcards) 0)
                (let ((re ""))
                  (dolist (w wildcards)
                    (incf re ~"|{(regexp-escape (slice (mangle w) 2))}"))
                  (let ((regexp (regexp ~"^s{(regexp-escape (slice (mangle *current-module*) 2))}\\\\$\\\\$({(slice re 1)}.*)")))
                    (dolist (k (keys (js-code "glob")))
                      (when (funcall (. regexp exec) k)
                        (let ((name (demangle k)))
                          (push `(push ',(intern name *current-module*)
                                       ,(intern "*exports*" *current-module*))
                                res)))))))
              res))))

(defmacro import (&rest args)
  "Imports a module and optionally copies specific symbol values in current module:
   [(import <module>)] simply imports a module without defining any symbol alias.
   [(import * from <module>)] imports a module and defines symbol aliases for all exported symbols.
   [(import (...) from <module>)] imports a module and defines specified aliases.
   Adding \"[as <nick>]\" after the module name allows defining a short nickname for the module."
  (let ((names (list))
        (alias null))
    (cond
      ((= (first args) '*)
       (unless (= (second args 'from))
         (error "Syntax is (import * from <module>)"))
       (setf names '*)
       (setf args (slice args 2)))
      ((list? (first args))
       (unless (= (second args 'from))
         (error "Syntax is (import (..names..) from <module>)"))
       (setf names (first args))
       (setf args (slice args 2))))
    (when (and (= (length args) 3)
               (= (second args) 'as)
               (symbol? (third args)))
      (setf alias (third args))
      (setf args (slice args 0 1)))
    (unless (and (= (length args) 1)
                 (symbol? (first args)))
      (error "Syntax is (import [*/(.. names..) from] <module> [as <nick>])"))
    (let* ((module (first args))
           (guard (intern ~"*{(symbol-name module)}-imported*" "")))
      `(progn
         (unless (symbol-value ',guard)
           (setf ,guard true)
           (let ((cmod *current-module*)
                 (calias *module-aliases*)
                 (salias *symbol-aliases*))
             (setf *module-aliases* (js-object))
             (setf *symbol-aliases* (js-object))
             (setf *current-module* ,(symbol-name module))
             (setf ,(intern "*exports*" (symbol-name module)) (list))
             (load ,(if node-js
                        `(get-file ,(+ (symbol-name module) ".lisp"))
                        `(http-get ,(+ (symbol-name module) ".lisp"))))
             (setf *current-module* cmod)
             (setf *symbol-aliases* salias)
             (setf *module-aliases* calias)))
         ,@(if alias
               (list `(setf (aref *module-aliases* ,(symbol-name alias))
                            ,(symbol-name module)))
               (list))
         ,(if (= names '*)
              `(dolist (s (symbol-value ',(intern "*exports*" (symbol-name module))))
                 (setf (aref *symbol-aliases* (symbol-name s)) s))
              `(progn ,@(let ((res (list)))
                          (dolist (s names)
                            (push `(setf (aref *symbol-aliases* ,(symbol-name s))
                                         ',(intern (symbol-name s) (symbol-name module)))
                                  res))
                          res)))
         (map #'symbol-name (symbol-value ',(intern "*exports*" (symbol-name module))))))))

;; Uri encoding/decoding support
(defun uri-decode (x)
  "Decode an uri-encoded string [x]"
  (js-code "(decodeURIComponent(d$$x))"))

(defun uri-encode (x)
  "Returns uri-encoding of string [x]"
  (js-code "(encodeURIComponent(d$$x))"))

;; Lexical symbol properties support
(defun lexical-property (x name)
  (js-code "(lexvar.props[d$$x.name][d$$name])"))

;; Conditions

(defvar *condition-handlers* (js-object (* (list))))

(defmacro defcondition (name args)
  `(progn
     (setf (aref *condition-handlers* ',name) (list))
     (defobject ,name (,@args restarts))))

(defmacro handler-case (expr &rest handlers)
  `(progn
     ,@(map (lambda (h)
              `(push (lambda ,@(rest h))
                     (aref *condition-handlers* ',(first h))))
            handlers)
     (unwind-protect ,expr
       ,@(map (lambda (h)
                `(pop (aref *condition-handlers* ',(first h))))
              handlers))))

(defmacro signal (condition data &rest restarts)
  (unless (aref *condition-handlers* condition)
    (error "Not a valid condition"))
  `(let ((condition (,#"new-{condition}"
                     ,@data
                     (js-object ,@(map (lambda (r)
                                         `(,(first r)
                                            (lambda ,@(rest r))))
                                       restarts))))
         (handler (or (last (aref *condition-handlers* ',condition))
                      (last (aref *condition-handlers* '*)))))
     (if handler
         (funcall handler condition)
         (error ,~"Unhandled condition {condition}"))))

(defun interactive-handler (x)
  "Displays an interactive handler for a generic condition [x] (browser only)"
  (do ((repl "")) ()
    (let ((msg ~"A special condition {x.%class} was signaled\n")
          (keys (keys x.restarts))
          (n 0))
      (dolist (k (rest x.%class))
        (unless (= k 'restarts)
          (incf msg ~"    {k} = {(str-value (aref x k))}\n")))
      (incf msg "\n")
      (when (> (length (keys x.restarts)) 0)
        (incf msg "Possible restarts are:\n")
        (dolist (k keys)
          (incf msg ~"{(incf n)} -> {k} {(str-value (aref x.restarts k).arglist)}\n"))
        (incf msg "\nenter '*n' followed by restart arguments or"))
      (incf msg "\nenter an expression to be evaluated\n")
      (incf msg repl)
      (let ((cmd (prompt msg)))
        (cond
          ((null? cmd) (error "Unhandled condition"))
          ((= (first cmd) "*")
           (let* ((args (parse-delimited-list (make-source (+ (slice cmd 1) ")")) ")"))
                  (rnum (atoi (first args))))
             (return-from interactive-handler (apply (aref x.restarts (aref keys (1- rnum)))
                                                     (map #'eval (rest args))))))
          (true (incf repl (+ ~"\n{cmd} ==> {(str-value (eval (parse-value cmd)))}"))))))))

;; simple destructuring let
(setf (symbol-macro 'let)
      (let* ((oldm (symbol-macro 'let))
             (newm (lambda (bindings &rest body)
                     (let ((exp (list))
                           (new-bindings (slice bindings)))
                       (dotimes (i (length new-bindings))
                         (let ((b (aref new-bindings i)))
                           (when (and (list? b)
                                      (list? (first b)))
                             (let ((temp (gensym)))
                               (push (list temp (first b)) exp)
                               (setf (first b) temp)))))
                       (if (length exp)
                           `(let ,new-bindings
                              (let ,(let ((res (list)))
                                      (dolist ((sym list) exp)
                                        (let ((i 0))
                                          (dolist (s list)
                                            (push `(,s (aref ,sym ,i)) res)
                                            (incf i))))
                                      res)
                                ,@body))
                           (apply oldm `(,bindings ,@body)))))))
        (setf (arglist newm) (arglist oldm))
        (setf (documentation newm) (documentation oldm))
        newm))

;; tracing
(defmacro trace (name)
  `(unless (. #',name org-func)
     (let ((i "")
           (org-func #',name))
       (setf #',name
             (lambda (&rest args)
               (display (+ i (str-value (append (list ',name) args) false)))
               (incf i "  ")
               (let ((result (apply org-func args)))
                 (setf i (slice i 2))
                 (display (+ i "--> " (str-value result false)))
                 result)))
       (setf (. #',name org-func) org-func)
       ',name)))

(defmacro untrace (name)
  `(when (. #',name org-func)
     (setf #',name (. #',name org-func))
     ',name))

;; Inline (experimental)

(defmacro defun/inline (name args &rest body)
  "Like [defun] but also defining if possible a macro for inline expansion"
  (if (or (find '&rest args)
          (find '&key args)
          (find '&optional args))
      `(defun ,name ,args ,@body)
      `(progn
         (defun ,name ,args ,@body)
         (defmacro ,name ,args
           (let ((syms (list))
                 (vars (list)))
             (dolist (x (list ,@args))
               (setf x (symbol-macro-expand x))
               (if (or (symbol? x)
                       (number? x)
                       (string? x))
                   (push x syms)
                   (progn
                     (push (gensym) syms)
                     (push `(,(last syms) ,x) vars))))
             `(let ,vars
                (symbol-macrolet ,(let ((res (list)))
                                    (dotimes (i ,(length args))
                                      (unless (= (aref ',args i) (aref syms i))
                                        (push `(,(aref ',args i) ,(aref syms i))
                                              res)))
                                    res)
                  ,',@body)))))))