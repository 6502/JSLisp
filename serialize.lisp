(defvar buf "")
(defvar p 0)
(defvar seen (list))

(defun backref (x)
  "Returns true if [x] has already been serialized and outputs the back reference, returns false otherwise
   also adding [x] to the list of already serialized objects"
  (let ((ix (index x seen)))
    (if (>= ix 0)
        (let ((six (+ ix "")))
          (incf buf (+ "r" (char (+ 64 (length six))) six))
          true)
        (progn
          (push x seen)
          false))))

(defun write (x)
  "Writes object [x] to current serialization buffer"
  (cond
    ((number? x)
     (let ((sx (+ x "")))
       (incf buf (+ "n" (char (+ 64 (length sx))) sx))))
    ((string? x)
     (let ((sx (+ (length x) "")))
       (incf buf (+ "s" (char (+ 64 (length sx))) sx x))))
    ((bool? x)
     (incf buf (if x "T" "F")))
    ((list? x)
     (unless (backref x)
       (let ((sx (+ (length x) "")))
         (incf buf (+ "l" (char (+ 64 (length sx))) sx))
         (dolist (item x) (write item)))))
    ((null? x) (incf buf "@"))
    ((undefined? x) (incf buf "?"))
    ((object? x)
     (unless (backref x)
       (let* ((keys (keys x))
              (sz (length keys))
              (ssz (+ sz "")))
         (incf buf (+ "O" (char (+ 64 (length ssz))) ssz))
         (dolist (k keys)
           (write k)
           (write (aref x k))))))
    (true (error ~"Unable to serialize {x}"))))

(defun tag (x)
  "Adds tag string [x] for a custom object"
  (incf buf (+ "t" (char (+ 64 (length x))) x)))

(defun next ()
  "Returns next character in buffer"
  (aref buf (1- (incf p))))

(defun size ()
  "Reads and decode a short size from buffer"
  (- (char-code (next)) 64))

(defun get (sz)
  "Reads next [sz] characters from buffer"
  (let ((p0 p))
    (slice buf p0 (incf p sz))))

(defun deserialize (tag)
  "Deserializes custom object associated to [tag]"
  (error ~"Unable to deserialize {tag}"))

(defun read ()
  "Reads an object from buffer"
  (case (next)
    ("n" (atof (get (size))))
    ("l" (let ((res (list)))
           (push res seen)
           (dotimes (i (atoi (get (size))))
             (push (read) res))
           res))
    ("s" (get (atoi (get (size)))))
    ("r" (aref seen (atoi (get (size)))))
    ("t" (deserialize (get (size))))
    ("?" undefined)
    ("@" null)
    ("T" true)
    ("F" false)
    ("O" (let ((res #()))
           (push res seen)
           (dotimes (i (atoi (get (size))))
             (let ((k (read))
                   (v (read)))
               (setf (aref res k) v)))
           res))
    (otherwise (error "Parse error"))))

(defmacro defobject* (name fields)
  "Defines an object like [defobject] also implementing serialization"
  (let ((x (gensym))
        (fnames (map (lambda (f)
                       (if (list? f)
                           (first f)
                           f))
                     fields)))
    `(progn
       (defobject ,name ,fields)
       (defmethod write (,x) (,#"{name}?" ,x)
                  (unless (backref ,x)
                    (tag ,(symbol-name name))
                    ,@(map (lambda (f)
                             `(write (. ,x ,f)))
                           fnames)))
       (defmethod deserialize (tag) (= tag ,(symbol-name name))
                  (let ((,x (,#"new-{name}")))
                    (push ,x seen)
                    ,@(map (lambda (f)
                             `(setf (. ,x ,f) (read)))
                           fnames)
                    ,x)))))

(defun to-buffer (x)
  "Serializes object [x] into a string. Supported objects are [null],
   [undefined], numbers, strings, lists of supported objects and objects
   supporting serialization where all fields contain supported objects
   ([defobject*] can be used to define an object with serialization support).
   Reference loops in lists and objects are supported and loops are rebuilt
   on deserialization. Note that symbols are NOT supported."
  (let ((buf "")
        (seen (list)))
    (write x)
    buf))

(defun from-buffer (x)
  "Deserializes an object from a string"
  (let ((buf x)
        (p 0)
        (seen (list)))
    (read)))

(export to-buffer from-buffer defobject* write read)
