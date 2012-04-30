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
               (if ,test
                   (progn ,@body)
                   (,of ,@args))))
       (list ',name ',test))))

(defun fields (x)
  "Returns the fields associated to object instance [x]"
  (and (list? x)
       (list? (first x))
       (slice (first x) 1)))

(defun class (x)
  "Returns the class [name] associated to object instance [x]"
  (and (list? x)
       (list? (first x))
       (first (first x))))

(defmacro defobject (name fields)
  "Defines an object with given [name] and [fields]
Each field can be either a symbol for the field name or a list for field name and default value.
After the macro invocation the following functions/macros/methods are defined:
function [(<name> f1 f2 ... fn)] Creates an instance
function [(make-<name> :f1 f1 :f2 f2 ... :fn fn)] Creates an instance
function [(<name>? x)] True if and only if is an instance created by one of the above
method [(<field> x)] field getter
method [(set-<field> x value)] field setter
method [(inc-<field> x inc)] field increment
method [(dec-<field> x dec)] field decrement
macro/f [(<name>-<field> x)] direct field access
macro [(with-<name> x &rest body)] evaluates body after directly binding r/w lexicals to fields
macro [(with-<name>* x &rest body)] evaluates body after dynamically binding r/w lexicals to fields"
  (setf name (module-symbol name))
  (unless (list? fields)
    (error "Syntax is (defobject name (field1 field2 ... fieldn))"))
  (let* ((fnames (map (lambda (x)
                        (cond
                          ((symbol? x) x)
                          ((and (list? x)
                                (= (length x) 2)
                                (symbol? (first x)))
                           (first x))
                          (true
                           (error "Each field must be either a symbol or a symbol/default-value pair"))))
                      fields))
         (id `(,name ,@fnames)))
    `(progn
       (defun ,name (&optional ,@fields)
         ,~"Builds an instance of {name}"
         (list ',id ,@fnames))
       (defun ,(intern ~"make-{name}") (&key ,@fields)
         ,~"Builds an instance of {name}"
         (list ',id ,@fnames))
       (defun ,(intern ~"{name}?") (x)
         (and (list? x) (= (first x) ',id)))
       ,@(map (lambda (f)
                (let ((ix (1+ (index f fnames)))
                      (set (intern ~"set-{f}"))
                      (inc (intern ~"inc-{f}"))
                      (dec (intern ~"dec-{f}"))
                      (access (intern ~"{name}-{f}")))
                  `(progn
                     (defmethod ,f (obj) (,(intern ~"{name}?") obj)
                       (aref obj ,ix))
                     (defmethod ,set (obj value) (,(intern ~"{name}?") obj)
                       (setf (aref obj ,ix) value))
                     (defmethod ,inc (obj inc) (,(intern ~"{name}?") obj)
                       (incf (aref obj ,ix) inc))
                     (defmethod ,dec (obj dec) (,(intern ~"{name}?") obj)
                       (decf (aref obj ,ix) dec))
                     (defmacro/f ,access (obj)
                       `(aref ,obj ,,ix)))))
              fnames)
       (defmacro ,(intern ~"with-{name}") (obj &rest body)
         (let ((var (gensym)))
           (setf obj (symbol-macro-expand obj))
           (if (symbol? obj)
               `(symbol-macrolet ,(map (lambda (f)
                                         `(,f (,(intern (+ ,~"{name}-" f)) ,obj)))
                                       ',fnames)
                  ,@body)
               `(let ((,var ,obj))
                  (symbol-macrolet ,(map (lambda (f)
                                           `(,f (,(intern (+ ,~"{name}-" f)) ,var)))
                                         ',fnames)
                    ,@body)))))
       (defmacro ,(intern ~"with-{name}*") (obj &rest body)
         (let ((var (gensym)))
           (setf obj (symbol-macro-expand obj))
           (if (symbol? obj)
               `(symbol-macrolet ,(map (lambda (f)
                                         `(,f (,f ,obj)))
                                       ',fnames)
                  ,@body)
               `(let ((,var ,obj))
                  (symbol-macrolet ,(map (lambda (f)
                                           `(,f (,f ,var)))
                                         ',fnames)
                    ,@body)))))
       ',name)))

(export defobject defmethod class fields)
