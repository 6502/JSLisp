(defvar *print-width* 70)

(defun pprint (obj)
  "Tries to format reasonably an object that contains JsLisp source code.
   This function is useful mostly for displaying the result of macro \
   expansion. See {{macroexpand-1}}[[
   (pprint '(labels((fact(x)(if(< x 2)1(*
            x(fact(1- x))))))(fact 10)))
   ;; produces as output
   (labels ((fact (x)
              (if (< x 2)
                  1
                  (* x (fact (1- x))))))
     (fact 10))
   ]]"
  (let ((result "")
        (col 0)
        (row 0)
        (indent (list)))
    (labels ((newline ()
               (incf result "\n")
               (dotimes (i (last indent))
                 (incf result " "))
               (incf row)
               (setf col (last indent)))
             (output (str)
               (when (and (= (first str) "\n")
                          (> (length str) 1))
                 (newline)
                 (setf str (slice str 1)))
               (case str
                 ("\n"
                    (newline))
                 ("("
                    (incf result str)
                    (incf col)
                    (push col indent))
                 (")"
                    (incf result str)
                    (incf col)
                    (pop indent))
                 (otherwise
                    (when (and (> col (last indent))
                               (> (+ col (length str)) *print-width*))
                      (newline))
                    (incf result str)
                    (incf col (length str)))))
             (sname (x)
               (if (symbol? x) (symbol-name x)))
             (sep (ppx px x i)
               (let ((nx (sname (first x)))
                     (npx (sname (first px)))
                     (nppx (sname (first ppx))))
                 (cond
                   ((and (list? (first x))
                         (/= '\. (first (first x)))) "\n")
                   ((= nx "progn") "\n ")
                   ((= nx "with-canvas") (if (>= i 1) "\n " " "))
                   ((= nx "do") (cond
                                        ((= i 0) " ")
                                        ((= i 1) "\n   ")
                                        (true "\n ")))
                   ((= nx "cond") "\n ")
                   ((= nx "labels") (if (>= i 1) "\n " " "))
                   ((= nx "macrolet") (if (>= i 1) "\n " " "))
                   ((= nx "case") (if (>= i 1) "\n " " "))
                   ((= nx "and") (if (>= i 1) "\n    " " "))
                   ((= nx "or") (if (>= i 1) "\n   " " "))
                   ((= nx "enumerate") (if (>= i 1) "\n " " "))
                   ((= nx "dotimes") (if (>= i 1) "\n " " "))
                   ((= nx "dolist") (if (>= i 1) "\n " " "))
                   ((= nx "set-handler") (if (>= i 2) "\n " " "))
                   ((= nx "set-style") (if (% i 2) "\n          " " "))
                   ((= nx "defun") (if (>= i 2) "\n " " "))
                   ((= nx "defmethod") (if (>= i 3) "\n " " "))
                   ((and (= nx "setf")
                         (= i 1)
                         (list? (aref x 2))
                         (= (first (aref x 2)) "lambda")) "\n     ")
                   ((= nx "lambda") (if (>= i 1) "\n " " "))
                   ((= nx "defmacro") (if (>= i 2) "\n " " "))
                   ((= nx "if") (if (>= i 1) "\n   " " "))
                   ((= nx "let") (if (>= i 1) "\n " " "))
                   ((= nx "let*") (if (>= i 1) "\n " " "))
                   ((= nx "let**") (if (>= i 1) "\n " " "))
                   ((= nx "with-window") (if (>= i 1) "\n " " "))
                   ((and (= npx "with-window")
                         (= (index x px) 1))
                    (if (>= i 1) "\n  " " "))
                   ((= npx "cond") "\n")
                   ((= npx "case") "\n")
                   ((= nppx "labels") (if (>= i 1) "\n " " "))
                   ((= nppx "macrolet") (if (>= i 1) "\n " " "))
                   ((= nx "when") (if (>= i 1) "\n " " "))
                   ((= nx "unless") (if (>= i 1) "\n " " "))
                   (true " "))))
             (dumplist (ppx px x)
               (enumerate (j y x)
                 (dump px x y)
                 (when (< j (1- (length x)))
                   (output (sep ppx px x j)))))
             (dump (ppx px x)
               (cond
                 ((list? x)
                  (cond
                    ((and (= (first x) '\.)
                          (= (length x) 3)
                          (or (string? (third x))
                              (symbol? (third x))))
                     (dump px x (second x))
                     (output ".")
                     (dump px x (third x)))
                    ((and (= (first x) '\`) (= (length x) 2))
                     (output "`")
                     (dump px x (second x)))
                    ((and (= (first x) '\,) (= (length x) 2))
                     (output ",")
                     (dump px x (second x)))
                    ((and (= (first x) '\,@) (= (length x) 2))
                     (output ",@")
                     (dump px x (second x)))
                    ((and (= (first x) 'quote) (= (length x) 2))
                     (output "'")
                     (dump px x (second x)))
                    ((and (= (first x) 'function) (= (length x) 2))
                     (output "#'")
                     (dump px x (second x)))
                    (true
                     (output "(")
                     (dumplist ppx px x)
                     (output ")"))))
                 ((symbol? x)
                  (output (symbol-name x)))
                 (true
                  (output (json x))))
               (when (and (list? x)
                          (find (first x) '(defun defmacro defmethod defvar)))
                 (newline))))
      (dump (list) (list) obj)
      (display result)
      null)))

(export pprint *print-width*)
