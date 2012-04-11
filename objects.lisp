(defmacro defgeneric (name args &rest body)
  `(progn
     (setf ,(intern ~"*{name}-methods*") `((true (progn ,',@body))))
     (setf ,(intern ~"*{name}-methods-arglist*") ,(str-value args))
     (defun ,name ,args ,@body)))

(defmacro defmethod (name args test &rest body)
  (unless (symbol-value (intern ~"*{name}-methods*"))
    (error ~"[{name}] doesn't identify a generic function"))
  (unless (= (str-value args) (symbol-value (intern ~"*{name}-methods-arglist*")))
    (error ~"Generic [{name}] arglist mismatch"))
  `(progn
     (push `(,',test (progn ,',@body)) ,(intern ~"*{name}-methods*"))
     (defun ,name ,args
       (cond
         (,test (progn ,@body))
         ,@(reverse (symbol-value (intern ~"*{name}-methods*")))))))

(defgeneric fields (x) null)
(defgeneric class (x) null)

(defmacro defobject (name fields)
  (unless (list? fields)
    (error "Syntax is (defobject name (field1 field2 ... fieldn))"))
  `(progn
     (defun ,name ,fields
       ,~"Builds an instance of {name}"
       (list ',fields ,@fields))
     (defun ,(intern ~"make-{name}") (&key ,@fields)
       ,~"Builds an instance of {name}"
       (list ',fields ,@fields))
     (defun ,(intern ~"{name}?") (x)
       ,~"True if and only if [x] is an instance of {name}"
       (and (list? x)
            (= (first x) ',fields)))
     (defmethod fields (x) (,(intern ~"{name}?") x)
       ',fields)
     (defmethod class (x) (,(intern ~"{name}?") x)
       ',name)
     ,@(map (lambda (f)
              `(defmacro/f ,f (obj)
                 ,~"Field [{f}] of an instance [obj] of [{name}]"
                 (list 'aref obj ,(1+ (index f fields)))))
            fields)
     ',name))
