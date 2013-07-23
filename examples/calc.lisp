(defobject binop (precedence associativity f))

(defun val (e x)
  (cond
    ((callable? x) (val e (funcall x e)))   ; Formula
    ((list? x) (val e (aref e (first x))))  ; Unboxing
    (true x)))

(macrolet ((bop (precedence associativity f)
                `(symbol-macrolet ((vx (val e x))
                                   (vy (val e y)))
                   (new-binop ,precedence ',associativity (lambda (x y) (lambda (e) ,f))))))
  (defvar binops
          #(("**"  (bop  1 R (expt vx vy)))
            ("*"   (bop  2 L (* vx vy)))
            ("/"   (bop  2 L (/ vx vy)))
            ("%"   (bop  2 L (% vx vy)))
            ("+"   (bop  3 L (+ vx vy)))
            ("-"   (bop  3 L (- vx vy)))
            ("<<"  (bop  4 L (ash vx vy)))
            (">>"  (bop  4 L (ash vx (- vy))))
            ("&"   (bop  5 L (logand vx vy)))
            ("|"   (bop  5 L (logior vx vy)))
            ("^"   (bop  5 L (logxor vx vy)))
            ("<"   (bop  6 L (< vx vy)))
            ("<="  (bop  6 L (<= vx vy)))
            (">"   (bop  6 L (> vx vy)))
            (">="  (bop  6 L (>= vx vy)))
            ("=="  (bop  6 L (= vx vy)))
            ("!="  (bop  6 L (/= vx vy)))
            ("and" (bop  7 L (and vx vy)))
            ("or"  (bop  8 L (or vx vy)))
            ("="   (bop  9 R (setf (aref e (first x)) vy)))
            (":="  (bop 10 R (progn (setf (aref e (first x)) y) vy))))))

(defvar expr_level (apply #'max (map (lambda (k) (aref binops k).precedence)
                                     (keys binops))))

(defvar number "[0-9]+(?:\\.[0-9]*)?(?:[Ee][-+]?[0-9]+)?")
(defvar string "\"(?:[^\"\\\\]|\\\\.)*\"")
(defvar var "[A-Za-z_][A-Za-z0-9_]*")

(defvar tkexpr (+ (join (map #'regexp-escape
                             (sort (keys binops)
                                   (lambda (x y) (> (length x) (length y))))) "|")
                  ~"|[()]\
                    |{number}\
                    |{string}\
                    |True|False\
                    |{var}\
                    |[^ ]"))

(defvar string_esc #(("n" "\n")
                     ("t" "\t")
                     ("f" "\f")
                     ("v" "\v")))

(defun calc (e s)
  (let** ((tk (s.match (regexp tkexpr "g")))
          (i 0)
          (#'expr (level)
            (when (= i (length tk))
              (error "Expression expected"))
            (if (= level 0)
                (case (aref tk (1- (incf i)))
                      ("-" (let ((x (expr 1)))
                             (lambda (e) (- (val e x)))))
                      ("True" true)
                      ("False" false)
                      ("(" (let ((x (expr expr_level)))
                             (unless (= (aref tk i) ")")
                               (error "')' expected"))
                             (incf i)
                             x))
                      (otherwise
                        (let ((x (aref tk (1- i))))
                          (cond
                            ((x.match (regexp var)) (list x))
                            ((= (first x) "\"") (replace (slice x 1 -1)
                                                         "\\\\."
                                                         (lambda (x)
                                                           (or (aref string_esc (second x))
                                                               (second x)))))
                            (true (atof x))))))
                (let ((x (expr (1- level)))
                      (op (aref binops (aref tk i))))
                  (do () ((or (not op) (/= op.precedence level)) x)
                    (incf i)
                    (let ((y (expr (if (= op.associativity 'R) level (1- level)))))
                      (setf x (op.f x y))
                      (setf op (aref binops (aref tk i))))))))
          (x (expr expr_level)))
    (unless (= i (length tk))
      (error "Extra tokens at end of expression"))
    (val e x)))

(defun main ()
  (let ((input (append-child document.body (create-element "input")))
        (env #()))
    (set-timeout (lambda () (input.focus)) 10)
    (setf input.style.width "100%")
    (setf input.onkeydown
          (lambda (event)
            (if (= event.which 13)
                (progn
                  (event.preventDefault)
                  (event.stopPropagation)
                  (let ((cmd (append-child document.body (create-element "div"))))
                    (setf cmd.innerText input.value))
                  (let ((res (append-child document.body (create-element "div"))))
                    (setf res.style.color "#F00")
                    (setf res.innerText (+ "==> " (try
                                                    (calc env input.value)
                                                    *exception*))))
                  (append-child document.body input)
                  (setf input.value "")
                  (set-timeout (lambda () (input.focus)) 10)))))))

(main)