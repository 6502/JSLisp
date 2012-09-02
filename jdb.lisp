;; LEVEL 0 - key/value store ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; Database format is very simple: the file is a sequence of lines
;; where each line is a JSON-serialized array with at least two elements.
;; The last element is the value and the previous elements are the key
;; path to reach the value.
;;
;; A value of null means that the key should be removed.
;;

(defobject database (file root))
(defvar *db*)

(defun db-load (file)
  (setf *db* (new-database file #()))
  (dolist (L (if (string? file)
                 (split (replace (try (get-file file) "") "\r" "") "\n")
                 file))
    (unless (= L "")
      (let* ((x (json-parse L))
             (n (length x))
             (value (aref x (1- n)))
             (key (aref x (- n 2)))
             (path (slice x 0 (- n 2)))
             (obj *db*.root))
        (dolist (k path)
          (setf obj (aref obj k)))
        (if (null? value)
            (remove-key obj key)
            (setf (aref obj key) value))))))

(defun db-dump ()
  (let ((L (map (lambda (k)
                  (json (list k (aref *db*.root k))))
                (keys *db*.root))))
    (if (list? *db*.file)
        (setf *db*.file L)
        (put-file *db*.file (+ (join L "\n") "\n")))
    (length L)))

(defun db-set (&rest x)
  (let* ((n (length x))
         (value (aref x (1- n)))
         (key (aref x (- n 2)))
         (path (slice x 0 (- n 2)))
         (obj *db*.root))
    (dolist (k path)
      (setf obj (aref obj k)))
    (if (null? value)
        (remove-key obj key)
        (setf (aref obj key) value))
    (if (list? *db*.file)
        (push (json x) *db*.file)
        (append-file *db*.file (+ (json x) "\n")))
    null))

(export *db* db-load db-dump db-set)

;; LEVEL 1 - table/record ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; db.tables is a map tablename -> table
;;
;; a table is a map with three values
;;
;; .fields   a list of field names
;; .maxid    the maximum id of all records in the table
;; .records  a map id -> record
;;
;; each record is a map with the fields listed in .fields and also
;; with a field .table with the name of the table and .id with the
;; record id.
;;

(defun db-new-table (name fields)
  (unless *db*.root.tables
    (db-set "tables" #()))
  (db-set "tables" name #((fields fields)
                          (maxid 0)
                          (records #()))))

(defun db-delete-table (name)
  (db-set "tables" name null))

(defun db-new-record (table values)
  (let* ((tab (aref *db*.root.tables table))
         (fields tab.fields)
         (id (1+ tab.maxid))
         (record #((table table)
                   (id id))))
    (unless (= (length values) (length fields))
      (error "Wrong number of values"))
    (dolist ((field value) (zip fields values))
      (setf (aref record field) value))
    (db-set "tables" table "records" id record)
    (db-set "tables" table "maxid" id)
    id))

(defun db-delete-record (obj)
  (db-set "tables" obj.table "records" obj.id null))

(defun db-eval (record expr)
  (cond
    ((or (number? expr)
         (null? expr)
         (string? expr)
         (bool? expr))
     expr)
    ((= expr 'null) null)
    ((= expr 'true) true)
    ((= expr 'false) false)
    ((symbol? expr)
     (aref record (symbol-name expr)))
    ((list? expr)
     (let ((op (first expr))
           (args (map (lambda (x) (db-eval record x)) (rest expr))))
       (cond
         ((find op '(< <= = >= > /=
                     find index last-index aref list slice append
                     + - * / %))
          (apply (symbol-function op) args))
         ((= op 'if)
          (if (first args) (second args) (third args)))
         ((= op 'and)
          (all (x args) x))
         ((= op 'or)
          (any (x args) x))
         ((= op 'regexp)
          ((regexp (first args)).exec (second args)))
         (true (error ~"Unsupported function {op}")))))
    (true (error "Unsupported expression"))))

(defun db-select (table expression condition)
  (let ((result (list))
        (records (aref *db*.root.tables table).records))
    (dolist (id (keys records))
      (let ((record (aref records id)))
        (when (db-eval record condition)
          (push (db-eval record expression) result))))
    result))

(defun db-update (table field expression condition)
  (unless (find field (aref *db*.root.tables table).fields)
    (error ~"Invalid field {field}"))
  (let ((count 0)
        (records (aref *db*.root.tables table).records))
    (dolist (id (keys records))
      (let ((record (aref records id)))
        (when (db-eval record condition)
          (incf count)
          (db-set "tables" table "records" id field
                  (db-eval record expression)))))
    count))

(defun db-delete (table condition)
  (let ((count 0)
        (records (aref *db*.root.tables table).records))
    (dolist (id (keys records))
      (let ((record (aref records id)))
        (when (db-eval record condition)
          (incf count)
          (db-delete-record record))))
    count))

(defun db-add-field (table field &optional (default null))
  (let ((fields (aref *db*.root.tables table).fields)
        (records (aref *db*.root.tables table).records)
        (count 0))
    (when (find field fields)
      (error ~"Field {field} already present"))
    (db-set "tables" table "fields" (length fields) field)
    (dolist (k (keys records))
      (let ((record (aref records k)))
        (incf count)
        (setf (aref record field) (db-eval record default))))
    count))

(defun db-remove-field (table field)
  (let ((fields (aref *db*.root.tables table).fields)
        (records (aref *db*.root.tables table).records)
        (count 0))
    (unless (find field fields)
      (error ~"Field {field} not present"))
    (db-set "tables" table "fields" (remove field fields))
    (dolist (k (keys records))
      (let ((record (aref records k)))
        (incf count)
        (setf (aref record field) null)))
    count))

(defmacro deftable (table fields)
  `(progn
     (unless (and (aref *db* "root" "tables")
                  (aref *db* "root" "tables" ,(symbol-name table)))
       (db-new-table ,(symbol-name table)
                     ',(map #'symbol-name fields)))
     (defun ,table (id)
       (aref (. *db* root tables ,table records) id))
     (defun ,#"new-{table}" (&key ,@fields)
       (db-new-record ,(symbol-name table)
                      (list ,@fields)))))

(export db-new-table db-delete-table db-add-field db-remove-field
        db-new-record db-delete-record
        db-select db-update db-delete
        deftable select-from)