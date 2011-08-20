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
                    (let ((doc null))
                      (if (stringp (js-code "d$$body[0]"))
                          (setq doc (js-code "d$$body.splice(0,1)[0]")))
                      (list 'progn
                            (list 'set-symbol-macro
                                  (list 'quote name)
                                  (append (list 'lambda args) body))
                            (list 'set-documentation
                                  (list 'symbol-macro (list 'quote name))
                                  doc)
                            (list 'quote name)))))

(defmacro defun (name args &rest body)
  ;; Shut up warnings about unknown function in recursive definitions
  ;; Must not set the function value unconditionally because of the
  ;; double-stage bootstrap of some basic operators (the poor version
  ;; must be available during macro expansion time of the better version)
  (unless (symbol-function name)
    (set-symbol-function name 42))
  (let ((doc null))
    (if (stringp (js-code "d$$body[0]"))
        (setq doc (js-code "d$$body.splice(0,1)[0]")))
    (list 'progn
          (list 'set-symbol-function
                (list 'quote name)
                (append (list 'lambda args) body))
          (list 'set-documentation
                (list 'symbol-function (list 'quote name))
                doc)
          (list 'quote name))))

; Length function
(defun length (x)
  "(length x) -> number
Returns the length of string/list x"
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

; Function accessor
(defmacro function (x)
  "(function x)
Returns the function currently bound to the /unevaluated/ symbol x (including lexical bindings)"
  (list 'js-code (+ "f" (mangle (symbol-name x)))))

(defmacro set-function (x value)
  "(set-function x value)
Sets the function currently bound to the /unevaluated/ symbol x (including lexical bindings)"
  (list 'js-code (+ "(f"
                    (mangle (symbol-name x)) "="
                    (js-compile value)
                    ")")))

; Javascript crazyness
(defun boolp (x)
  "(boolp x)
True if and only if x is a boolean"
  (js-code "((typeof d$$x)=='boolean')"))

(defun undefinedp (x)
  "(undefinedp x)
True if and only if x is the undefined value"
  (js-code "((typeof d$$x)=='undefined')"))

(defun nullp (x)
  "(nullp x)
True if and only if x is the null value"
  (js-code "((typeof d$$x)=='object'&&!d$$x)"))

(defun NaNp (x)
  "(NaNp x)
True if and only if x is the NaN value"
  (js-code "((typeof d$$x)=='number'&&!d$$x&&!(d$$x==0))"))

(defun objectp (x)
  "(objectp x)
True if and only if x is a javascript object"
  (js-code "((d$$x&&d$$x.constructor&&d$$x.constructor==Object)==true)"))

(defun zerop (x)
  "(zerop x)
True if and only if x is the number zero"
  (and (numberp x) (= x 0)))

(defmacro aref (x i)
  "(aref list index)
Returns the index-th element of a list/string or the value associated to the key 'index' in an object"
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile i) "])")))
(defun aref (x i)
  "(aref list index)
Returns the index-th element of a list/string or the value associated to the key 'index' in an object"
  (aref x i))

(defmacro set-aref (x i v)
  "(set-aref list index value)
Sets the index-th element of a list or the value associated to the key 'index' in an object"
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile i) "]=" (js-compile v) ")")))

(defun set-aref (x i v)
  "(set-aref list index value)
Sets the index-th element of a list or the value associated to the key 'index' in an object"
  (set-aref x i v))

; List-related macros (can't be defined before '+')
(defmacro length (x)
  "(length x)
Returns the length of a list or string object"
  (list 'js-code (+ "(" (js-compile x) ".length)")))

(defmacro list (&rest args)
  "(list x1 x2 ... xn)
Returns a new fresh list containing the values of x1, x2 ... xn"
  (let ((res "[")
        (sep ""))
    (dolist (x args)
      (setq res (+ res sep (js-compile x)))
      (setq sep ","))
    ;; Note that the following is still the list FUNCTION because the
    ;; macro will be available AFTER this defmacro form is evaluated
    (list 'js-code (+ res "]"))))

(defun push (x v)
  "(push value list)
Adds the specified value to the end of the list 'list'"
  (js-code "d$$v.push(d$$x)"))

(defmacro rest (x)
  "(rest x)
Returns a list obtained from 'x' by removing first element"
  (list 'js-code (+ "(" (js-compile x) ".slice(1))")))

(defun rest (x)
  "(rest x)
Returns a list obtained from 'x' by removing first element"
  (js-code "(d$$x.slice(1))"))

; Quasiquoting
(defun bqconst (x)
  "(bqconst x)
True if the form 'x' is constant in respect to backquoting"
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
  "(bquote x)
Returns the backquote expansion of x"
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

(defmacro |`| (x) (bquote x))

;; defmacro/f
(defmacro defmacro/f (name args &rest body)
  "(defmacro/f <name> <args> &rest <body)
Defines a macro and an equivalent function"
  ;; Note: Defmacro must be done at macro expansion time
  ;;       because we need the macro in place when
  ;;       defun is macroexpanded
  (eval `(defmacro ,name ,args ,@body))
  `(defun ,name ,args (,name ,@args)))

;; Utilities
(defmacro/f slice (x a b)
  "(slice x a b)
Returns a list obtained by copying from x elements from a-th to b-th (excluded)."
  (cond
    ((and (= a undefined) (= b undefined))
     `(js-code ,(+ "(" (js-compile x) ").slice()")))
    ((and (= b undefined))
     `(js-code ,(+ "(" (js-compile x) ").slice(" (js-compile a) ")")))
    (true
     `(js-code ,(+ "(" (js-compile x) ".slice(" (js-compile a) "," (js-compile b) "))")))))

(defmacro/f reverse (x)
  "(reverse x)
Returns a copy of the elements in x in the opposite ordering"
  `(js-code ,(+ "(" (js-compile x) ".slice().reverse())")))

(defmacro/f nreverse (x)
  "(nreverse x)
Inverts the ordering of the elements in list x"
  `(js-code ,(+ "(" (js-compile x) ".reverse())")))

(defmacro/f first (x) "(first  x)\nFirst element of list/string" `(aref ,x 0))
(defmacro/f second (x) "(second x)\nSecond element of list/string" `(aref ,x 1))
(defmacro/f third (x) "(third x)\nThird element of list/string" `(aref ,x 2))
(defmacro/f fourth (x) "(fourth x)\nFourth element of list/string" `(aref ,x 3))
(defmacro/f fifth (x) "(fifth x)\nFifth element of list/string" `(aref ,x 4))
(defmacro/f sixth (x) "(sixth x)\nSixth element of list/string" `(aref ,x 5))
(defmacro/f seventh (x) "(seventh x)\nSeventh element of list/string" `(aref ,x 6))
(defmacro/f eighth (x) "(eighth x)\nEighth element of list/string" `(aref ,x 7))
(defmacro/f nineth (x) "(nineth x)\nNineth element of list/string" `(aref ,x 8))
(defmacro/f tenth (x) "(tenth x)\nTenth element of list/string" `(aref ,x 9))

(defun subseq (x start count)
  "(subseq x start count)
Returns a partial copy of the list/string x starting from 'start' and with 'count' elements.
If count is omitted then the subsequence will contain all elements from start to the end of the list."
  (if (= count undefined)
      (slice x start)
      (slice x start (+ start count))))

(defmacro defmathop (name comment none single jsname)
  `(defmacro ,name (&rest args)
     ,comment
     (cond
       ((= (length args) 0)
        ,none)
       ((= (length args) 1)
        ,single)
       ((= (length args) 2)
        `(js-code ,(+ "("
                      (js-compile (aref args 0))
                      ,jsname
                      (js-compile (aref args 1))
                      ")")))
       (true
        `(js-code ,(+ "("
                      (js-compile `(,',name ,@(slice args 0 (- (length args) 1))))
                      ,jsname
                      (js-compile (aref args (- (length args) 1)))
                      ")"))))))

(defmacro defmathop-func (name)
  `(defun ,name (&rest args)
     ,(documentation (symbol-macro name))
     (cond
       ((= (length args) 0) (,name))
       ((= (length args) 1) (,name (aref args 0)))
       ((= (length args) 2) (,name (aref args 0)
                                   (aref args 1)))
       (true
        (let ((res (aref args 0)))
          (dolist (x (slice args 1))
            (setq res (,name res x)))
          res)))))

; Math n-ary operators macros and functions

(defmathop + "(+ x1 x2 x3 ...)\nAddition/string concatenation"
  0 (aref args 0) "+")
(defmathop - "(- x1 x2 x3 ...)\nSubtraction"
  0 `(js-code ,(+ "-" (js-compile (aref args 0)))) "-")
(defmathop * "(* x1 x2 x3 ...)\nMultiplication"
  1 (aref args 0) "*")
(defmathop / "(/ x1 x2 ...)\nDivision"
  1 `(/ 1 ,(aref args 0)) "/")
(defmathop logior "(logior x1 x2 ...)\nBitwise inclusive or"
  0 (aref args 0) "|")
(defmathop logand "(logand x1 x2 ...)\nBitwise and"
  -1 (aref args 0) "&")
(defmathop logxor "(logxor x1 x2 ...)\nBitwise exclusive or"
  0 (aref args 0) "^")
(defmathop-func +)
(defmathop-func -)
(defmathop-func *)
(defmathop-func /)
(defmathop-func logior)
(defmathop-func logand)
(defmathop-func logxor)

(defmacro/f % (a b)
  "(% a b)
Modulo operation"
  `(js-code ,(+ "("
                (js-compile a)
                "%"
                (js-compile b)
                ")")))

(defun ash (x count)
  "(ash x count)
Arithmetic shift left (< count 0) or right (> count 0)"
  (js-code "(d$$count<0?(d$$x>>-d$$count):(d$$x<<d$$count))"))

; Comparisons

(defvar *gensym-count* 0)
(defun gensym (prefix)
  "(gensym [prefix])
Returns a new unique symbol eventually named using the specified prefix"
  (intern (+ "G:" (if prefix (+ prefix "/") "")
             (setq *gensym-count* (+ 1 *gensym-count*)))))

(defmacro defrelop (name comment jsname)
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
       (let ((x1 (gensym))
             (x2 (gensym)))
         `(let ((x1 ,(aref args 0))
                (x2 ,(aref args 1)))
            (and (,',name x1 x2) (,',name x2 ,@(slice args 2)))))))))

(defmacro defrelop-func (name)
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

(defrelop <  "(< x1 x2 ...)\nStrictly less than comparison" "<")
(defrelop <= "(<= x1 x2 ...)\nLess than or equal comparison" "<=")
(defrelop =  "(= x1 x2 ...)\nEquality comparison" "==")
(defrelop == "(== x1 x2 ...)\nTyped-equality comparison" "===")
(defrelop >= "(>= x1 x2 ...)\nGreater than or equal comparison" ">=")
(defrelop >  "(> x1 x2 ...)\nStrictly greather than comparison" ">")
(defrelop-func <  )
(defrelop-func <= )
(defrelop-func =  )
(defrelop-func == )
(defrelop-func >= )
(defrelop-func >  )

;; /= operator is different from others as no transitivity can be used
;; (it means "arguments are all distinct")
(defmacro /= (&rest args)
  "(/= x1 x2 x3 ...)
True if and only if the values x1, x2, x3 ... are all distinct"
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
  "(/= x1 x2 x3 ...)
True if and only if the values x1, x2, x3 ... are all distinct"
  (if (< (length args) 2)
      true
      (do ((n (- (length args) 1))
           (i 0 (+ i 1)))
          ((or (= i n) (/= -1 (js-code "d$$args.indexOf(d$$args[d$$i],d$$i+1)")))
             (= i n)))))

(defmacro let* (bindings &rest body)
  "(let* ((x1 v1)(x2 v2)...(xn vn)) f1 f2 ... fn)
Evaluates the body forms f1, f2 ... fn in sequence by first establishing lexical/dynamic bindings
x1=v1, x2=v2 ... xn=vn and when during the evaluation of form vk all previous bindings x1, x2 ... xk-1
are already visible."
  (if (> (length bindings) 1)
     `(let (,(aref bindings 0))
        (let* ,(rest bindings) ,@body))
     `(let ,bindings ,@body)))

(defmacro setf (place value)
  "(setf place value)
Sets the content of a place to be the specified value. A place is either a symbol or a form (e.g. (aref x i))
for which a corresponding setting form is defined (e.g. (set-aref x i value)) either as function or macro
eventually after macro expansion."
  (cond
    ((symbolp place)
     `(setq ,place ,value))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "set-" (symbol-name f)))))
       (if (or (symbol-function sf) (symbol-macro sf))
           `(,sf ,@(rest place) ,value)
           (if (symbol-macro f)
               `(setf ,(macroexpand-1 place) ,value)
                (error "Unsupported setf place")))))
    (true (error "Invalid setf place"))))

(defmacro incf (place inc)
  "(incf place [increment])
Increments the content of a place by the specified increment or by 1 if not specified.
A place is either a symbol or a form (e.g. (aref x i)) for which a corresponding
increment form is defined (e.g. (inc-aref x i increment)) either as function or macro
eventually after macro expansion."
  (if (= inc undefined) (setf inc 1))
  (cond
    ((symbolp place)
     `(setq ,place (+ ,place ,inc)))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "inc-" (symbol-name f)))))
       (if (or (symbol-function sf) (symbol-macro sf))
           `(,sf ,@(rest place) ,inc)
           (if (symbol-macro f)
               `(incf ,(macroexpand-1 place) ,inc)
                (error "Unsupported decf place")))))
    (true (error "Invalid incf place"))))

(defmacro decf (place inc)
  "(decf place [decrement])
Decrements the content of a place by the specified decrement or by 1 if not specified.
A place is either a symbol or a form (e.g. (aref x i)) for which a corresponding
decrement form is defined (e.g. (dec-aref x i decrement)) either as function or macro
eventually after macro expansion."
  (if (= inc undefined) (setf inc 1))
  (cond
    ((symbolp place)
     `(setq ,place (- ,place ,inc)))
    ((listp place)
     (let* ((f (first place))
            (sf (intern (+ "dec-" (symbol-name f)))))
       (if (or (symbol-function sf) (symbol-macro sf))
           `(,sf ,@(rest place) ,inc)
           (if (symbol-macro f)
               `(decf ,(macroexpand-1 place) ,inc)
                (error "Unsupported decf place")))))
    (true (error "Invalid decf place"))))

(defmacro/f 1+ (x)
  "(1+ x)
Returns (+ x 1)"
  `(+ ,x 1))

(defmacro/f 1- (x)
  "(1- x)
Returns (- x 1)"
  `(- ,x 1))

(defmacro inc-aref (array index value)
  "(inc-aref list index increment)
Increments the index-th element of a list or the element associated to key 'index' in a javascript object"
  `(js-code ,(+ "(" (js-compile array)
                "[" (js-compile index)
                "]+=" (js-compile value) ")")))

(defmacro dec-aref (array index value)
  "(dec-aref list index decrement)
Decrements the index-th element of a list or the element associated to key 'index' in a javascript object"
  `(js-code ,(+ "(" (js-compile array)
                "[" (js-compile index)
                "]-=" (js-compile value) ")")))

; Sequence utilities
(defun reduce (f seq)
  "(reduce function sequence)
Reduces a sequence to a single value by repeating application of function to pairs of elements in the sequence.
For an empty sequence the return value is the result of calling function without parameters"
  (if (= 0 (length seq))
      (funcall f)
      (let ((res (first seq)))
        (dolist (x (rest seq))
          (setf res (funcall f res x)))
        res)))

(defun min (seq)
  "(min sequence)
Returns the minimum value of a sequence under comparison with <"
  (reduce (lambda (x y) (if (< x y) x y)) seq))

(defun max (seq)
  "(max sequence)
Returns the maximum value of a sequence under comparison with >"
  (reduce (lambda (x y) (if (> x y) x y)) seq))

(defun map (f seq)
  "(map function sequence)
Returns the sequence obtained by applying function to each element of sequence"
  (let ((res (list)))
    (dolist (x seq)
      (push (funcall f x) res))
    res))

(defun zip (&rest sequences)
  "(zip seq1 seq2 ... seqn)
Returns a list of lists built from corresponding elements in all sequences.
The resulting list length is equal to the shorter of seq1, seq2, ... seqn."
  (let ((n (min (map #'length sequences))))
    (let ((res (list)))
      (dotimes (i n)
        (push (map (lambda (seq) (aref seq i)) sequences) res))
      res)))

(defun mapn (f &rest sequences)
  "(mapn function &rest sequences)
Returns the sequence of calling function passing as parameters corresponding elements in the sequences.
The resulting list length is equal to the shotest sequence."
  (map (lambda (args) (apply f args))
       (apply #'zip sequences)))

(defun make-array (n initial-value)
  "(make-array size initial-value)
Creates a list with the specified number of elements all of them with the specified value"
  (let ((x (list)))
    (dotimes (i n)
      (setf (aref x i) initial-value))
    x))

(defun filter (f seq)
  "(filter function sequence)
Returns the subset of elements from sequence for which the function filter returned a logical true value"
  (let ((res (list)))
    (dolist (x seq)
      (when (funcall f x)
        (push x res)))
    res))

(defun range (start stop step)
  "(range [start] stop [step])
Returns a list containing all numbers from start (0 if not specified) up to stop counting by step (1 if not specified)."
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

(defun index (x L)
  "(index x L)
Returns the index position in which x appears in list/string L or -1 if it's not present"
  (js-code "d$$L.indexOf(d$$x)"))

(defun last-index (x L)
  "(last-index x L)
Returns the last index position in which x appears in list/string L or -1 if it's not present"
  (js-code "d$$L.lastIndexOf(d$$x)"))

(defmacro/f nsort (x cond)
  "(nsort sequence condition)
Modifies a sequence by sorting the elements according to the specified condition."
  (if (= cond undefined)
      `(js-code ,(+ "(" (js-compile x) ").sort()"))
      `(js-code ,(+ "(" (js-compile x) ").sort(" (js-compile cond) ")"))))

(defmacro/f sort (x cond)
  "(sort sequence condition)
Returns a copy of a sequence with elements sorted according to the specified condition."
  `(nsort (slice ,x) ,cond))

; Keyword arguments

(setf (compile-specialization 'lambda)
      (let ((oldcf (compile-specialization 'lambda))
            (unassigned (gensym)))
        (lambda (whole)
          (let* ((args (second whole))
                 (body (slice whole 2))
                 (i (index '&key args)))
            (if (= i -1)
                (funcall oldcf whole)
                (let ((rest (gensym "rest"))
                      (nrest (gensym "nrest"))
                      (ix (gensym "ix")))
                  (unless (= -1 (index '&rest args))
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

; Defstruct
(defmacro defstruct (name &rest fields)
  "(defstruct field1 field2 ... fieldn)
Defines a structur with the specified fields. Each field can be either a symbol or a list with a symbol
and a default value that will be used when instantiating the structure if no values are passed for
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
         (if (and (listp self) (= ',name (aref self 0))) true false))
       (defvar ,(intern (+ "*" (symbol-name name) "-fields*"))
         ',fnames)
       ,@(let ((res (list))
               (index 1))
           (dolist (f fnames)
             (let ((fn (intern (+ (symbol-name name) "-" (symbol-name f)))))
               (push `(defmacro/f ,fn (self)
                        `(aref ,self ,,index)) res)
               (incf index)))
           res))))

; Math functions
(defmacro/f sin (x) "(sin x)\nSine of angle x in radians"
  `(js-code ,(+ "Math.sin(" (js-compile x) ")")))
(defmacro/f cos (x) "(cos x)\nCosine of angle x in radians"
  `(js-code ,(+ "Math.cos(" (js-compile x) ")")))
(defmacro/f tan (x) "(tan x)\nTangent of angle x in radians"
  `(js-code ,(+ "Math.tan(" (js-compile x) ")")))
(defmacro/f exp (x) "(exp x)\ne=2.718281828... raised to power of x"
  `(js-code ,(+ "Math.exp(" (js-compile x) ")")))
(defmacro/f log (x) "(log x)\nNatural logarithm of x"
  `(js-code ,(+ "Math.log(" (js-compile x) ")")))
(defmacro/f atan (x) "(atan x)\nArc-tangent in radians of the value x"
  `(js-code ,(+ "Math.atan(" (js-compile x) ")")))
(defmacro/f floor (x) "(floor x)\nBiggest integer that is not bigger than x"
  `(js-code ,(+ "Math.floor(" (js-compile x) ")")))
(defmacro/f abs (x) "(abs x)\nAbsolute value of x"
  `(js-code ,(+ "Math.abs(" (js-compile x) ")")))
(defmacro/f atan2 (y x) "(atan2 y x)\nArc-tangent of y/x in radians with proper handling of all quadrants"
  `(js-code ,(+ "Math.atan(" (js-compile y) "," (js-compile x) ")")))
(setq pi (js-code "Math.PI"))

; JS exception support
(defvar *exception* null)
(setf (compile-specialization 'try)
      (lambda (x)
        (+ "((function(){try{return("
           (js-compile (aref x 1))
           ");}catch(err){var olderr=d$$$42$exception$42$;d$$$42$exception$42$=err;var res=("
           (js-compile (aref x 2))
           ");d$$$42$exception$42$=olderr;return res;}})())")))

; Timing
(defun clock ()
  "(clock)
Returns the number of millisecond passed since midnight of January 1st, 1970"
  (js-code "(new Date).getTime()"))

(defmacro time (&rest body)
  "(time form1 form2 ... formn)
Measures and returns the number of millisecond needed for the evaluation form1 form2 ... formn in sequence"
   (let ((start (gensym)))
     `(let ((,start (clock)))
        ,@body
        (- (clock) ,start))))

; JS object access/creation
(defmacro . (obj &rest fields)
  "(obj field sub-field sub-sub-field ...)
Returns the javascript object value selected by traversing the specified list of fields (unevaluated symbols)"
  (let ((res (js-compile obj)))
    (dolist (x fields)
      (setf res (+ res "." (symbol-name x))))
    `(js-code ,res)))

(defmacro set-. (obj &rest fields)
  "(set-obj field sub-field sub-sub-field ... value)
Sets the javascript object value selected by traversing the specified list of fields (unevaluated symbols)"
    (let ((res (js-compile obj)))
        (dolist (x (slice fields 0 (1- (length fields))))
            (setf res (+ res "." (symbol-name x))))
        (setf res (+ res "=" (js-compile (aref fields (1- (length fields))))))
        `(js-code ,res)))

(defmacro js-object (&rest fields)
  "(js-object (x1 v1)(x2 v2)...(xn vn))
Creates a javascript object and assigns v1 to the field x1, v2 to field x2 ... vn to field xn.
Fields are specified as unevaluated symbols."
  (let ((self (gensym)))
    `(let ((,self (js-code "({})")))
       ,@(let ((res (list)))
           (dolist (f fields)
             (push `(setf (. ,self ,(first f)) ,(second f)) res))
           res)
       ,self)))

; DOM
(setf document (js-code "document"))
(setf window (js-code "window"))

(defun get-element-by-id (id)
  "(get-element-by-id id)
Returns the DOM element with the specified id value"
  (funcall (. document getElementById) id))

(defun create-element (type)
  "(create-element type)
Creates a new DOM element with the specified type passed as a string"
  (funcall (. document createElement) type))

(defun append-child (x child)
  "(append-child parent child)
Appends the DOM element 'child' as last (frontmost) children of the DOM element 'parent'"
  (funcall (. x appendChild) child))

(defun remove-child (x child)
  "(remove-child parent child)
Removes the DOM element child from the list of children of DOM element 'parent'"
  (funcall (. x removeChild) child))

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

; Javascript blocking interaction
(defun prompt (x)
  "(prompt x)
Asks the user for a string providing 'x' as a prompt message"
  (js-code "prompt(d$$x)"))

(defun alert (x)
  "(alert x)
Displays an alert message to the user"
  (js-code "alert(d$$x)"))

(defun yesno (msg)
  "(yesno question)
Asks the user to reply either yes or no to a question. Returns True if the answer is yes or False otherwise"
  (do ((reply (prompt msg)
              (prompt ~"I don't understand...\n{msg}\nPlease answer \"yes\" or \"no\" without quotes.")))
      ((or (= reply "yes")
           (= reply "no"))
       (= reply "yes"))))
