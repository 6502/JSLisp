(defun sym (&rest args)
  ;; Interns a symbol by concatenating parts of the final
  ;; name where each part is either a symbol itself or a literal
  (let ((res ""))
    (dolist (x args)
      (incf res (if (symbol? x) (symbol-name x) x)))
    (intern res)))

(defmacro defrecord (name pkey &rest fields)
  ;;
  ;; Defines a new database record type
  ;; (defrecord <name> (<key1> <key2> ... <keyn>)
  ;;    [<field-1> | (<field-1> <validator-1>)]
  ;;    [<field-2> | (<field-2> <validator-2>)]
  ;;    ...
  ;;    [<field-n> | (<field-n> <validator-n>)])
  ;;
  ;; Defines
  ;;   - *<name>* as a map from key->record
  ;;   - (*<name>* key-1 key-2 ... key-n) as a record by-key accessor
  ;;   - (new-<name> value-1 value-2 ... value-n) as constructor
  ;;   - (field-1 record) as field accessor (including setter and pkey update handling)
  ;;
  (let ((fieldnames (map (lambda (f)
                           (if (symbol? f) f (first f)))
                         fields)))
    (eval `(defmacro ,(sym "*" name "*") (&rest key)
             (unless (= (length key) ,(length pkey))
               (error ,~"*{(symbol-name name)}*-accessor: key is {(map #'symbol-name pkey)}"))
             `(aref ,',(sym "*" name "*") (list ,@key))))
    `(progn
       (defstruct ,name ,@fieldnames)
       (defvar ,(sym "*" name "*") #())
       ,@(map (lambda (f)
                (if (symbol-function f)
                    `(setf #',f
                           (let ((,(sym "old-" f) #',f))
                             (lambda (,name)
                               (if (,(sym name "?") ,name)
                                   (,(sym name "-" f) ,name)
                                   (funcall ,(sym "old-" f) ,name)))))
                    `(defun ,f (,name) (,(sym name "-" f) ,name))))
              fieldnames)
       ,@(map (lambda (ff)
                (let* ((f (if (symbol? ff) ff (first ff)))
                       (tf (if (symbol? ff) undefined (second ff)))
                       (setter (if (/= -1 (index f pkey))
                                   `(progn
                                      (when (/= * (,f ,name))
                                        (if (,(sym "*" name "*")
                                              ,@(map (lambda (f1)
                                                       (if (= f f1)
                                                           `*
                                                           `(,f1 ,name)))
                                                     pkey))
                                            (error ,~"set-{(symbol-name f)}: primary key duplication")
                                            (progn
                                              (setf (,(sym "*" name "*")
                                                      ,@(map (lambda (f)
                                                               `(,f ,name))
                                                             pkey))
                                                    undefined)
                                              (setf (,(sym name "-" f) ,name) *)
                                              (setf (,(sym "*" name "*")
                                                      ,@(map (lambda (f)
                                                               `(,f ,name))
                                                             pkey))
                                                    ,name))))
                                      *)
                                   `(setf (,(sym name "-" f) ,name) *))))
                  (if (symbol-function (sym "set-" f))
                      `(setf #',(sym "set-" f)
                             (let ((,(sym "old-set-" f) #',(sym "set-" f)))
                               (lambda (,name *)
                                 (if (,(sym name "?") ,name)
                                     ,(if tf
                                          `(if (funcall ,tf *)
                                               ,setter
                                               (error ,~"set-{(symbol-name f)}: incompatible value"))
                                          setter)
                                     (funcall ,(sym "old-set-" f) ,name *)))))
                      `(defun ,(sym "set-" f) (,name *)
                         ,(if tf
                              `(if (funcall ,tf *)
                                   ,setter
                                   (error ,~"set-{(symbol-name f)}: incompatible value"))
                              setter)))))
              fields)
       (defun ,(sym "new-" name) ,fieldnames
         (when (,(sym "*" name "*") ,@pkey)
           (error ,~"new-{(symbol-name name)}: primary key duplication"))
         ,@(let ((res (list)))
                (dolist (f fields)
                  (when (list? f)
                    (push `(unless (funcall ,(second f) ,(first f))
                             (error ,~"Incompatible value for {(symbol-name (first f))}"))
                          res)))
                res)
         (let ((,name (,(sym "make-" name)
                        ,@(let ((res (list)))
                               (dolist (f fieldnames)
                                 (push (sym ":" f) res)
                                 (push f res))
                               res))))
           (setf (,(sym "*" name "*") ,@pkey)
                 ,name))))))
