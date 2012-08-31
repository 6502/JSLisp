;;
;; Database format is very simple: the file is a sequence of lines
;; where each line is either a json*-serialized named object or a
;; single number
;;
;; - if the line contains a named object then that object is the new
;;   version for the object with id [obj.id]
;;
;; - if the line contains a number then the number is the id of the
;;   object to be removed from the database
;;
;; Database is updated by appending lines that either put a new
;; version of an object or remove an existing object.
;;
;; - .%filename is the filename for persistence
;; - .%maxid is the maximum of all object IDs in memory
;; - .%all is the id->obj dictionary of all objects.
;; - .<classname> is an id->obj of objects of that class.
;;
;; When on disk a reference to another record is represented with an
;; instance of jdb:ref containing the id of the pointed record.
;;

(defobject database (%file %maxid %all))
(defobject ref (id))

(defun db-load (file)
  (let ((db (new-database file 0 #())))
    (dolist (L (if (list? file)
                   file
                   (split (replace (try (get-file file) "") "\r" "")
                          "\n")))
      (unless (= L "")
        (let ((obj (json-parse* L)))
          (if (number? obj)
              (let ((x (aref db.%all obj)))
                (remove-key db.%all obj)
                (remove-key (aref db (first obj.%class)) obj))
              (let ((id obj.id)
                    (class (first obj.%class)))
                (setf (aref db.%all id) obj)
                (when (> id db.%maxid)
                  (setf db.%maxid id))
                (setf (aref (or (aref db class)
                                (setf (aref db class) #()))
                            id) obj))))))
    (labels ((fix (x)
               (cond
                 ((or (object? x)
                      (and x x.%class))
                  (dolist (k (keys x))
                    (if (ref? (aref x k))
                        (setf (aref x k) (aref db.%all (aref x k).id))
                        (fix (aref x k)))))
                 ((list? x)
                  (dotimes (i (length x))
                    (if (ref? (aref x i))
                        (setf (aref x i) (aref db.%all (aref x i).id))
                        (fix (aref x i))))))))
      (fix db.%all))
    db))

(defun db-json* (db obj)
  (cond
    ((and obj obj.%class obj.id
          (= (aref db.%all obj.id) obj))
     ~"\\{\"%class\":\"jdb:ref\",\"id\":{obj.id}\\}")
    ((list? obj)
     (let ((res "[")
           (sep ""))
       (dolist (x obj)
         (incf res ~"{sep}{(db-json* db x)}")
         (setf sep ","))
       (+ res "]")))
    ((object? obj)
     (let ((res "{")
           (sep ""))
       (dolist (k (keys obj))
         (incf res ~"{sep}{(json k)}:{(db-json* db (aref obj k))}")
         (setf sep ","))
       (+ res "}")))
    ((and obj obj.%class)
     (let ((res ~"\\{\"%class\":{(json (first obj.%class))}"))
       (dolist (k (rest obj.%class))
         (incf res ~",{(json k)}:{(db-json* db (aref obj k))}"))
       (+ res "}")))
    (true (json obj))))

(defun db-json*-1 (db obj)
  (let ((res ~"\\{\"%class\":{(json (first obj.%class))}"))
    (dolist (k (rest obj.%class))
      (incf res ~",{(json k)}:{(db-json* db (aref obj k))}"))
    (+ res "}")))

(defun db-dump (db)
  (let ((L (map (lambda (id)
                  (db-json*-1 db (aref db.%all id)))
                (keys db.%all))))
    (if (list? db.%file)
        (setf db.%file L)
        (put-file db.%file (join L "\n"))))
  db.%file)

(defun db-get (db id)
  (aref db.%all id))

(defun db-okput? (db obj)
  true)

(defun db-okdelete? (db obj)
  true)

(defun db-put (db obj)
  (unless (and obj.%class (find "id" obj.%class))
    (error "First-level objects in database must have an [id] field"))
  (if (db-okput? db obj)
      (progn
        (unless obj.id
          (setf obj.id (incf db.%maxid)))
        (let ((id obj.id)
              (class (first obj.%class)))
          (when (> id db.%maxid)
            (setf db.%maxid id))
          (setf (aref db.%all id) obj)
          (setf (aref (or (aref db class)
                          (setf (aref db class) #()))
                      id) obj)
          (if (list? db.%file)
              (push (db-json*-1 db obj) db.%file)
              (append-file db.%filename (+ (db-json*-1 db obj) "\n")))
          obj))
      null))

(defun db-delete (db obj)
  (if (db-okdelete? db obj)
      (let ((id obj.id)
            (class (first obj.%class)))
        (remove-key db.%all id)
        (remove-key (aref db class) id)
        (if (list? db.%file)
            (push (json id) db.%file)
            (append-file db.%filename (+ (json id) "\n")))
        true)
      false))

(defmacro db-foreach ((var dbtable) &rest body)
  (unless (and (list? dbtable)
               (= (first dbtable '.))
               (symbol? (third dbtable)))
    (error "Syntax is (foreach (<var> <db>.<class>) ...)"))
  (let ((result '#.(gensym))
        (x '#.(gensym))
        (id '#.(gensym))
        (db '#.(gensym)))
    `(let ((,result (list))
           (,db ,(second dbtable)))
       (labels ((,#"collect" (,x) (push ,x ,result)))
         (dolist (,id (keys (aref ,db ,(symbol-full-name (third dbtable)))))
           (let ((,var (aref ,db "%all" ,id)))
             ,@body)))
       ,result)))

(defmacro defrecord (name fields)
  `(progn
     (defobject ,name (id ,@fields))
     (defun ,name ,fields
       (,#"new-{name}" null ,@fields))))

(export db-load db-dump
        db-get db-put db-delete
        db-okput? db-okdelete?
        db-foreach
        defrecord)
