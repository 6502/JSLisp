(defmacro defmethod (name args test &rest body)
  (let ((mvar (intern ~"*{name}-methods*"))
        (avar (intern ~"*{name}-arglist*")))
    (unless (symbol-value mvar)
      (setf (symbol-value mvar) `((true (error ,~"No matching method [{name}]")))))
    (let ((sa (str-value args false)))
      (if (symbol-value avar)
          (unless (= sa (symbol-value avar))
            (error ~"Method [{name}] arglist mismatch"))
          (setf (symbol-value avar) sa)))
    (push `(,test ,@body) (symbol-value mvar))
    `(defun ,name ,args
       (cond
         ,@(reverse (symbol-value mvar))))))

(defmethod fields (x) true null)
(defmethod class (x) true null)
(defmethod make-instance (class &rest args) true
  (error "Undefined class"))

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
     (defmethod make-instance (class &rest args) (= class ',name)
       (apply #',name args))
     (defun ,(intern ~"{name}?") (x)
       (and (list? x) (= (first x) ',fields)))
     (defmethod fields (x) (,(intern ~"{name}?") x)
       ',fields)
     (defmethod class (x) (,(intern ~"{name}?") x)
       ',name)
     ,@(map (lambda (f)
              (let ((ix (1+ (index f fields)))
                    (set (intern ~"set-{f}"))
                    (inc (intern ~"inc-{f}"))
                    (dec (intern ~"dec-{f}")))
                `(progn
                   (defmethod ,f (obj) (,(intern ~"{name}?") obj)
                              (aref obj ,ix))
                   (defmethod ,set (obj value) (,(intern ~"{name}?") obj)
                              (setf (aref obj ,ix) value))
                   (defmethod ,inc (obj inc) (,(intern ~"{name}?") obj)
                              (incf (aref obj ,ix) inc))
                   (defmethod ,dec (obj dec) (,(intern ~"{name}?") obj)
                              (decf (aref obj ,ix) dec)))))
            fields)
     ',name))
