(defvar *transaction* (list))
(defvar *changelog* (list))
(defvar *tables* (list))
(defvar *no-transactions* false)

(defvar *logwrite* null)

(defun commit ()
  (when *logwrite*
    (funcall *logwrite* (str-value `(progn ,@*changelog*) false)))
  (setf *changelog* (list))
  (setf *transaction* (list)))

(defun rollback ()
  (setf *changelog* (list))
  (do () ((zero? (length *transaction*)))
    (funcall (pop *transaction*))))

(defun dump (write-function)
  (dotimes (pass 2)
    (dolist (table *tables*)
      (let* ((colnames (keys (symbol-value table)))
             (cols (map (lambda (n)
                          (aref (symbol-value table) n))
                        colnames)))
        (if (= pass 0)
            (funcall write-function
                     `(defrecord ,table ,(map #'intern colnames)))
            (dotimes (i (length (first cols)))
              (funcall write-function
                       `(,#"new-{table}" ,@(map (lambda (j)
                                                  (aref cols j i))
                                                (range (length colnames)))))))))))

(defmacro defrecord (name fields)
  `(progn
     (unless (= 0 (length *transaction*) (length *changelog*))
       (error "Record type definition is not allowed in a transaction"))
     (when *logwrite*
       (funcall *logwrite* (str-value `(defrecord ,',name ,',fields) false)))
     (push ',name *tables*)
     (defvar ,name #(,@(map (lambda (f)
                              `(,f (list)))
                        fields)))
     (defun ,#"new-{name}" ,fields
       (let ((id (length (. ,name ,(first fields)))))
         ,@(map (lambda (f)
                  `(push ,f (. ,name ,f)))
                fields)
         (unless *no-transactions*
           (push (lambda ()
                   ,@(map (lambda (f)
                            `(pop (. ,name ,f)))
                          fields))
                 *transaction*)
           (push (append (list ',#"new-{name}") (list ,@fields))
                 *changelog*))
         (list ',name id)))
     (defun ,name (record)
       (list ',name record))
     ,@(map (lambda (f)
              `(defmethod ,f (record) (= (first record) ',name)
                 (aref (. ,name ,f) (second record))))
            fields)
     ,@(map (lambda (f)
              `(defmethod ,#"set-{f}" (record value) (= (first record) ',name)
                 (let* ((r (second record))
                        (old-value (aref (. ,name ,f) r)))
                   (unless *no-transactions*
                     (push `(setf (aref (. ,',name ,',f) ,r) ',value)
                           *changelog*)
                     (push (lambda () (setf (aref (. ,name ,f) r) old-value))
                           *transaction*)))
                 (setf (aref (. ,name ,f) (second record)) value)))
            fields)
     (defmacro ,#"foreach-{name}" (var &rest body)
       (let ((rno (gensym)))
         `(dotimes (,rno (length (. ,',name ,',(first fields))))
            (let ((,var (list ',',name ,rno)))
              ,@body))))
     ',name))

(export *logwrite* *tables* *no-transactions*
        defrecord
        commit rollback
        dump)