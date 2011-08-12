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
     (list 'set-symbol-macro
           (list 'quote name)
           (append (list 'lambda args) body))))

(defmacro defun (name args &rest body)
  (list 'set-symbol-function
        (list 'quote name)
        (append (list 'lambda args) body)))

; Javascript crazyness
(defun boolp (x) (js-code "((typeof d$$x)=='boolean')"))
(defun undefinedp (x) (js-code "((typeof d$$x)=='undefined')"))
(defun nullp (x) (js-code "((typeof d$$x)=='object'&&!d$$x)"))
(defun NaNp (x) (js-code "((typeof d$$x)=='number'&&!d$$x&&!(d$$x==0))"))
(defun objectp (x) (js-code "((d$$x&&d$$x.constructor&&d$$x.constructor.name=='Object')==true)"))
(defun zerop (x) (and (numberp x) (= x 0)))

; Length function
(defun length (x) (js-code "d$$x.length"))

; Simple versions of a few operators needed for bootstrap, they will be redefined
(defun = (a b) (js-code "(d$$a==d$$b)"))
(defun < (a b) (js-code "(d$$a<d$$b)"))
(defun > (a b) (js-code "(d$$a>d$$b)"))
(defun - (a b) (js-code "(d$$a-d$$b)"))
(defun + (&rest args)
  (cond
    ((= (length args) 1) (aref args 0))
    ((= (length args) 2) (js-code "(d$$args[0]+d$$args[1])"))
    (true (js-code "(d$$args[0]+f$$$43$.apply(null,d$$args.slice(1)))"))))

; List-related macros (can't be defined before '+')
(defmacro length (x)
  (list 'js-code (+ "(" (js-compile x) ".length)")))

(defmacro list (&rest args)
  (let ((res "[")
        (sep ""))
    (dolist (x args)
      (setq res (+ res sep (js-compile x)))
      (setq sep ","))
    ;; Note that the following is still the list FUNCTION because the
    ;; macro will be available AFTER this defmacro form is evaluated
    (list 'js-code (+ res "]"))))

(defmacro aref (x i)
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile i) "])")))
(defun aref (x i) (aref x i))

(defmacro set-aref (x i v)
  (list 'js-code (+ "(" (js-compile x) "[" (js-compile i) "]=" (js-compile v) ")")))
(defun set-aref (x i v) (set-aref x i v))

(defun push (x v)
  (js-code "d$$v.push(d$$x)"))

(defmacro rest (x)
  (list 'js-code (+ "(" (js-compile x) ".slice(1))")))
(defun rest (x)
  (js-code "(d$$x.slice(1))"))

; Quasiquoting
(defun bqconst (x)
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
    `(progn
        (defmacro ,name ,args ,@body)
        (eval `(defun ,',name ,',args
                      ,(apply (symbol-macro ',name) ',args)))))

;; Utilities
(defmacro/f slice (x a b)
  (cond
    ((and (= a undefined) (= b undefined))
     `(js-code ,(+ "(" (js-compile x) ").slice()")))
    ((and (= b undefined))
     `(js-code ,(+ "(" (js-compile x) ").slice(" (js-compile a) ")")))
    (true
     `(js-code ,(+ "(" (js-compile x) ".slice(" (js-compile a) "," (js-compile b) "))")))))

(defmacro/f reverse (x)
  `(js-code ,(+ "(" (js-compile x) ".slice().reverse())")))

(defmacro/f nreverse (x)
  `(js-code ,(+ "(" (js-compile x) ".reverse())")))

(defmacro/f first   (x) `(aref ,x 0))
(defmacro/f second  (x) `(aref ,x 1))
(defmacro/f third   (x) `(aref ,x 2))
(defmacro/f fourth  (x) `(aref ,x 3))
(defmacro/f fifth   (x) `(aref ,x 4))
(defmacro/f sixth   (x) `(aref ,x 5))
(defmacro/f seventh (x) `(aref ,x 6))
(defmacro/f eighth  (x) `(aref ,x 7))
(defmacro/f nineth  (x) `(aref ,x 8))
(defmacro/f tenth   (x) `(aref ,x 9))

(defun subseq (x start count)
  (if (= count undefined)
      (slice x start)
      (slice x start (+ start count))))

(defmacro defmathop (name none single jsname)
    `(defmacro ,name (&rest args)
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

(defmathop + 0 (aref args 0) "+")
(defmathop - 0 `(js-code ,(+ "-" (js-compile (aref args 0)))) "-")
(defmathop * 1 (aref args 0) "*")
(defmathop / 1 `(/ 1 ,(aref args 0)) "/")
(defmathop logior 0 (aref args 0) "|")
(defmathop logand -1 (aref args 0) "&")
(defmathop logxor 0 (aref args 0) "^")
(defmathop-func +)
(defmathop-func -)
(defmathop-func *)
(defmathop-func /)
(defmathop-func logior)
(defmathop-func logand)
(defmathop-func logxor)

(defmacro/f % (a b) `(js-code ,(+ "("
                                  (js-compile a)
                                  "%"
                                  (js-compile b)
                                  ")")))

(defun ash (x count)
  (js-code "(d$$count<0?(d$$x>>-d$$count):(d$$x<<d$$count))"))

; Comparisons

(defvar *gensym-count* 0)
(defun gensym (prefix)
  (intern (+ "G:" (if prefix (+ prefix "/") "")
             (setq *gensym-count* (+ 1 *gensym-count*)))))

(defmacro defrelop (name jsname)
  `(defmacro ,name (&rest args)
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
     (if (< (length args) 2)
         true
         (do ((current (first args))
              (n (length args))
              (i 1 (+ i 1)))
             ((or (= i n) (not (,name current (aref args i))))
                (= i n))
           (setq current (aref args i))))))

(defrelop < "<"   )
(defrelop <= "<=" )
(defrelop = "=="  )
(defrelop >= ">=" )
(defrelop > ">"   )
(defrelop-func <  )
(defrelop-func <= )
(defrelop-func =  )
(defrelop-func >= )
(defrelop-func >  )

;; /= operator is different from others as no transitivity can be used
;; (it means "arguments are all distinct")
(defmacro /= (&rest args)
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
  (if (< (length args) 2)
      true
      (do ((n (- (length args) 1))
           (i 0 (+ i 1)))
          ((or (= i n) (/= -1 (js-code "d$$args.indexOf(d$$args[d$$i],d$$i+1)")))
             (= i n)))))

(defmacro let* (bindings &rest body)
  (if (> (length bindings) 1)
     `(let (,(aref bindings 0))
        (let* ,(rest bindings) ,@body))
     `(let ,bindings ,@body)))

(defmacro setf (place value)
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

(defmacro/f 1+ (x) `(+ ,x 1))
(defmacro/f 1- (x) `(- ,x 1))

(defmacro inc-aref (array index value)
  (let ((aa (gensym))
        (ix (gensym)))
    `(let ((,aa ,array)
           (,ix ,index))
       (setf (aref ,aa ,ix) (+ (aref ,aa ,ix) ,value)))))

(defmacro dec-aref (array index value)
  (let ((aa (gensym))
        (ix (gensym)))
    `(let ((,aa ,array)
           (,ix ,index))
       (setf (aref ,aa ,ix) (- (aref ,aa ,ix) ,value)))))

(defun substr (x start count)
  (if (undefinedp count)
      (js-code "(d$$x.substr(d$$start))")
      (js-code "(d$$x.substr(d$$start,d$$count))")))

(defmacro substr (x start count)
  (if (undefinedp count)
      `(js-code ,(+ "(" (js-compile x)
                    ".substr(" (js-compile start) "))"))
      `(js-code ,(+ "(" (js-compile x)
                    ".substr(" (js-compile start)
                    "," (js-compile count) "))"))))

; Sequence utilities
(defun reduce (f seq)
  (if (= 0 (length seq))
      (funcall f)
      (let ((res (first seq)))
        (dolist (x (rest seq))
          (setf res (funcall f res x)))
        res)))

(defun min (seq)
  (reduce (lambda (x y) (if (< x y) x y)) seq))

(defun max (seq)
  (reduce (lambda (x y) (if (> x y) x y)) seq))

(defun map (f seq)
  (let ((res (list)))
    (dolist (x seq)
      (push (funcall f x) res))
    res))

(defun zip (&rest sequences)
  (let ((n (min (map #'length sequences))))
    (let ((res (list)))
      (dotimes (i n)
        (push (map (lambda (seq) (aref seq i)) sequences) res))
      res)))

(defun mapn (f &rest sequences)
  (map (lambda (args) (apply f args))
       (apply #'zip sequences)))

(defun make-array (n initial-value)
  (let ((x (list)))
    (dotimes (i n)
      (setf (aref x i) initial-value))
    x))

(defun filter (f seq)
  (let ((res (list)))
    (dolist (x seq)
      (when (funcall f x)
        (push x res)))
    res))

(defun range (start stop step)
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
  (js-code "d$$L.indexOf(d$$x)"))

(defun last-index (x L)
  (js-code "d$$L.lastIndexOf(d$$x)"))

(defmacro/f nsort (x cond)
  (if (= cond undefined)
      `(js-code ,(+ "(" (js-compile x) ").sort()"))
      `(js-code ,(+ "(" (js-compile x) ").sort(" (js-compile cond) ")"))))

(defmacro/f sort (x cond)
  `(nsort (slice ,x) ,cond))

; Keyword arguments

(defmacro defun (name args &rest body)
  (let ((i (index '&key args)))
    (if (= i -1)
        `(set-symbol-function ',name (lambda ,args ,@body))
        (let ((rest (gensym "rest"))
              (nrest (gensym "nrest"))
              (ix (gensym "ix")))
          (unless (= -1 (index '&rest args))
            (error "&key and &rest are incompatible"))
          `(defun ,name ,(append (slice args 0 i) (list '&rest rest))
             (let ((,nrest (length ,rest))
                   ,@(map (lambda (x)
                            (if (listp x)
                                x
                                (list x undefined)))
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
               ,@body))))))

; Defstruct
(defmacro defstruct (name &rest fields)
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
(defmacro/f sin (x) `(js-code ,(+ "Math.sin(" (js-compile x) ")")))
(defmacro/f cos (x) `(js-code ,(+ "Math.cos(" (js-compile x) ")")))
(defmacro/f tan (x) `(js-code ,(+ "Math.tan(" (js-compile x) ")")))
(defmacro/f exp (x) `(js-code ,(+ "Math.exp(" (js-compile x) ")")))
(defmacro/f log (x) `(js-code ,(+ "Math.log(" (js-compile x) ")")))
(defmacro/f atan (x) `(js-code ,(+ "Math.atan(" (js-compile x) ")")))
(defmacro/f floor (x) `(js-code ,(+ "Math.floor(" (js-compile x) ")")))
(defmacro/f abs (x) `(js-code ,(+ "Math.abs(" (js-compile x) ")")))
(defmacro/f atan2 (y x) `(js-code ,(+ "Math.atan(" (js-compile y) "," (js-compile x) ")")))
(setq pi (js-code "Math.PI"))

; Timing
(defun clock ()
  (js-code "(new Date).getTime()"))

(defmacro time (&rest body)
   (let ((start (gensym)))
      `(let ((,start (clock)))
          ,@body
          (- (clock) ,start))))

; JS object access/creation
(defmacro . (obj &rest fields)
    (let ((res (js-compile obj)))
        (dolist (x fields)
            (setf res (+ res "." (symbol-name x))))
        `(js-code ,res)))

(defmacro set-. (obj &rest fields)
    (let ((res (js-compile obj)))
        (dolist (x (slice fields 0 (1- (length fields))))
            (setf res (+ res "." (symbol-name x))))
        (setf res (+ res "=" (js-compile (aref fields (1- (length fields))))))
        `(js-code ,res)))

(defmacro js-object (&rest fields)
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
(defun get-element-by-id (id) (funcall (. document getElementById) id))
(defun create-element (id) (funcall (. document createElement) id))
(defun append-child (x child) (funcall (. x appendChild) child))
(defun remove-child (x child) (funcall (. x removeChild) child))
