(defun pattern-var? (s)
  (and (symbol? s)
       (= "<" (first (symbol-name s)))
       (= ">" (last (symbol-name s)))))

(defun match (pattern src &optional (vars #()))
  (cond
    ((pattern-var? pattern)
     (setf (aref vars pattern) src)
     vars)
    ((list? pattern)
     (when (and (list? src)
                (= (length src) (length pattern)))
       (dolist ((x y) (zip pattern src))
         (unless (match x y vars)
           (return-from match false)))
       vars))
    (true
     (when (= src pattern)
       vars))))

(defun vars (pattern)
  (let ((result (list)))
    (labels ((visit (x)
               (cond
                 ((pattern-var? x)
                  (push x result))
                 ((list? x)
                  (dolist (y x) (visit y))))))
      (visit pattern)
      result)))

(defvar *patterns* (list))

(defmacro defpattern (pattern &rest body)
  (let ((vars (sort (vars pattern))))
    `(progn
       (push (list ',pattern
                   ',vars
                   (lambda ,vars ,@body))
             *patterns*)
       ',pattern)))

(setf #'parse-value
      (let ((of #'parse-value))
        (lambda (src)
          (let ((x (funcall of src)))
            (when (list? x)
              (dolist (p *patterns*)
                (let ((m (match (first p) x)))
                  (when m
                    (setf x (apply (third p)
                                   (map (lambda (x) (aref m x))
                                        (second p))))))))
            x))))
