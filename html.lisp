(setf (hash-reader "#")
      (lambda (src)
        (next-char src)
        `(document.getElementById ,(symbol-name (parse-symbol src)))))

(defmacro set-style (element &rest properties)
  "Allows settings multiple style properties for a DOM node, example:[[
     (set-style mynode
                position \"absolute\"
                px/left 0
                px/top  0
                px/width 200
                px/height 300)
]]
  The [px/] prefix means a CSS unit that will be appended to the expression."
  (let ((el '#.(gensym))
        (elstyle '#.(gensym)))
    `(let* ((,el ,element)
            (,elstyle (. ,el style)))
       ,@(map (lambda (i)
                (let* ((prop (aref properties i))
                       (value (aref properties (1+ i)))
                       (pname (symbol-name prop))
                       (um (index "/" pname)))
                  (if (= um -1)
                      `(setf (. ,elstyle ,prop) ,value)
                      `(setf (. ,elstyle ,(intern (slice pname (1+ um))))
                             (+ ,value ,(slice pname 0 um))))))
              (range 0 (length properties) 2))
       ,el)))

(setf (hash-reader ">")
      (lambda (src)
        (next-char src)
        (let ((stopmark "")
              (text ""))
          (do ((c (current-char src) (current-char src)))
            ((or (undefined? c) (= c "\n"))
             (when c
               (incf stopmark c)
               (next-char src)))
            (incf stopmark c)
            (next-char src))
          (do ((c (current-char src) (current-char src)))
            ((or (undefined? c)
                 (= (slice text (- (length stopmark))) stopmark))
             (slice text 0 (- (length stopmark))))
            (incf text c)
            (next-char src)))))

(defvar *css* #())

(defun scss (tag tx)
  (unless (aref *css* tag)
    (setf (aref *css* tag)
          (append-child document.head
                        (document.createElement "style")))
    (setf (aref *css* tag).type "text/css"))
  (setf (aref *css* tag).textContent (+ tag tx)))

(defmacro css (tag &rest attributes)
  (unless (and (even? (length attributes))
               (all (i (range 0 (length attributes) 2))
                 (keyword? (aref attributes i))))
    (error "syntax is (css <tag> <attr1>: <value1> ...)"))
  (let ((tx (list '+ "{"))
        (sep ""))
    (dolist (i (range 0 (length attributes) 2))
      (push (+ sep (slice (symbol-name (aref attributes i)) 0 -1) ":") tx)
      (push (aref attributes (1+ i)) tx)
      (setf sep ";"))
    (push (+ sep "}") tx)
    `(scss ,tag ,tx)))

(defun class (x)
  (x.className))

(defun set-class (x y)
  (setf x.className y))

(defun class-add (x &rest classes)
  (dolist (c classes)
    (x.classList.add c)))

(defun class-remove (x &rest classes)
  (dolist (c classes)
    (x.classList.remove c)))

(defmacro on (element event &rest body)
  `((. ,element addEventListener) ,(symbol-name event)
    (lambda (event)
      (declare (ignorable event))
      ,@body)))

(export css set-style class set-class class-add class-remove on)