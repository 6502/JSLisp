(defmacro defmethod (name args test &rest body)
  `(progn
     (defvar ,(intern ~"*{name}-methods*") '((true (error ,~"No matching method [{name}]"))))
     (defvar ,(intern ~"*{name}-methods-arglist*") ,(str-value args))
     (unless (= ,(str-value args) ,(intern ~"*{name}-methods-arglist*"))
       (error ,~"Method [{name}] arglist mismatch"))
     (push (append (list ',test) ',body) ,(intern ~"*{name}-methods*"))
     (defun ,name ,args
       (cond
         (,test ,@body)
         ,@(if (symbol-value (intern ~"*{name}-methods*"))
               (reverse (symbol-value (intern ~"*{name}-methods*")))
               `((true (error ,~"No matching methof [{name}]"))))))))

(defmethod fields (x) true null)
(defmethod class (x) true null)

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
