"deploy.lisp is JsLisp treeshaking/optimizer module.
Given a source lisp file that contains a (main ...) function call it
will generate minimal and minimized javascript code that executes the
main function call.

The code will contains only functions that are actually being
referenced explicitly in #'main code or in functions referenced
explicitly in them and so on recursively.

Output will also contain all variables, symbols and quoted forms
explicitly referenced in the included code. Initial variable value
will be the current content at macroexpansion time of (main ...)
call.  Note that closures currently cannot be correctly stringified
and therefore any variable containing a closure will end up containing
a regular function, the value of closed over variable will be lost and
the contained function will access instead a global javascript
variable.

Compiled Javascript code is also minified by changing all names to
shorter versions ('$' followed by a base-62 azAZ09 progressive number)
and by removing code metainformation not normally needed at runtime.
The minifier makes strong assumptions about what kind of javascript is
being fed, so it can possibly break inlined general javascript code.

In other words anything fancy most probably will not work.

The program works by defining a (main &rest args) macro that when
macroexpanded will output the compiled program instead of executing
the main call. It can be used directly from the command line using
node:

    node jslisp.js deploy.lisp myprogram.lisp > myprogram.js

The compiled code allows faster loading and startup and some source
code protection; no macros and no readable names will be present
making the resulting javascript close to stripped machine code from a
reverse engineering point of view.
"

(defun stringify (x)
  (if (callable? x)
      (+ x "")
      (try
       (js-code "JSON.stringify(d$$x)")
       "'**UNABLE-TO-STRINGIFY**'")))

(defun literal (k names)
  (let ((x (js-code "lisp_literals[parseInt(d$$k.substr(1))]")))
    (if (or names (not (symbol? x)))
        (stringify x)
        "[]")))

(defun variable (k names)
  (let ((x (js-code "glob['d'+d$$k]")))
    (if (or names (not (symbol? x)))
        (stringify x)
        "[]")))

(defun symbol (k names)
  (let ((x (js-code "glob[d$$k]")))
    (if (or names (not (symbol? x)))
        (stringify x)
        "[]")))

(defun fclosure (x)
  (do ((result (js-object))
       (vars (js-object))
       (todo (list x))
       (litout false))
      ((empty todo)
         (let ((output (list))
               (names (aref result "f$$symbol_name")))
           (dolist (k (sort (keys vars)))
             (cond
               ((= (aref k 0) "q")
                (unless litout
                  (setf litout true)
                  (push "lisp_literals=[];" output))
                (push ~"lisp_literals[{(slice k 1)}]={(literal k names)};" output))
               ((= (aref k 0) "s")
                (push ~"{k}={(symbol k names)};" output))
               ((not (undefined? (js-code "glob['d'+d$$k]")))
                (push ~"d{k}={(variable k names)};" output))))
           (dolist (k (sort (keys result)))
             (push ~"f{k}={(aref result k)};" output))
           output))
    (let ((new-todo (list)))
      (dolist (x todo)
        (setf (aref result (. x name))
              (or (and (symbol-function x)
                       (+ (symbol-function x) ""))
                  "**N/A**"))
        (dolist (v (or (and (symbol-function x)
                            (. (symbol-function x) usedglobs))
                       (list)))
          (setf (aref vars v) true))
        (dolist (fn (or (and (symbol-function x)
                             (. (symbol-function x) outcalls))
                        (list)))
          (unless (aref result fn)
            (let* ((i (index "$$" fn))
                   (y (intern (demangle fn) (slice fn 0 i))))
              (push y new-todo)))))
      (setf todo new-todo))))

(defun minimize (s)
  (let ((seen (js-object))
        (lits (js-object))
        (rlits (js-object))
        (litcount 0)
        (count 0))
    (labels ((nstr (x)
               (if (< x 62)
                   (aref "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789" x)
                   (+ (nstr (floor (/ x 62)))
                      (nstr (% x 62)))))
             (newlit (x)
               (or (aref lits x)
                   (let ((n (+ "_" (nstr (1- (incf litcount))))))
                     (setf (aref rlits n) x)
                     (setf (aref lits x) n))))
             (newname (x)
               (or (aref seen x)
                   (setf (aref seen x)
                         (+ "$" (nstr (1- (incf count))))))))
      ; String literals collection
      (setf s (replace s "\"([^\"\\\\]|\\\\.)*\"" #'newlit))
      (setf s (replace s "'([^'\\\\]|\\\\.)*'" #'newlit))
      ; Name shortening
      (setf s (replace s "[a-zA-Z0-9]*\\$\\$[a-zA-Z0-9_$]*" #'newname))
      ; Lisp literals renaming
      (setf s (replace s "lisp_literals\\[[0-9]+\\]*" #'newname))
      (setf s (replace s "lisp_literals=\\[\\];" ""))
      ; Code metainformation removal
      (setf s (replace s "f\\.usedglobs=\\[[^\\]]*\\];f\\.outcalls=\\[[^\\]]*\\];" ""))
      ; Extra parenthesis removal
      (dotimes (i 5)
        (setf s (replace s "([-+*/<>=!~&|\\(])\\((-?[0-9]+\\.?[0-9]*|[a-zA-Z_\\$][a-zA-Z_0-9\\$]*)\\)" "$1$2")))
      (setf s (replace s "\\[\\(([^\\]()]+)\\)\\]" "[$1]"))
      (dotimes (i 5)
        (setf s (replace s "(\\(|,)\\(([^(),]+)\\)(\\)|,)" "$1$2$3")))
      (setf s (replace s "([^-])(--)+([^-])" "$1+$3"))
      (setf s (replace s "([^-])-(--)+([^-])" "$1-$3"))
      (setf s (replace s "([^-+])\\+-([^-+])" "$1-$2"))
      ; Unneeded semicolon and space removal
      (setf s (replace s " *([-:?&|;(){}=+*/,]) *" "$1"))
      (setf s (replace s ";}" "}"))
      ; Restore object-creation literals
      (setf s (replace s "{_[a-zA-Z0-9]+:" (lambda (x) ~"\\{{(aref rlits (slice x 1 (1- (length x))))}:")))
      (setf s (replace s ",_[a-zA-Z0-9]+:" (lambda (x) ~",{(aref rlits (slice x 1 (1- (length x))))}:")))
      ; Remove unneeded quotes in object field ids
      (setf s (replace s "{\"([a-zA-Z_$][a-zA-Z_$0-9]*)\":" "{$1:"))
      (setf s (replace s ",\"([a-zA-Z_$][a-zA-Z_$0-9]*)\":" ",$1:"))
      ; "function(" packing
      (when (or (find "\"" s) (find "\\" s) (find "'" s))
        (error "Internal error: Unexpected quote or backslash"))
      (setf s (+ "eval(\""
                 (replace s "function\\(" "'")
                 "\".replace(/'/g,\"function(\"))"))
      ; Add used literals definitions
      (let ((ldef ""))
        (dolist (key (keys rlits))
          (when (find key s)
            (setf ldef ~"{key}={(aref rlits key)};{ldef}")))
        (setf s (+ ldef s)))
      (+ s "//JsLisp"))))

(setf (symbol-macro (intern "main" ""))
      (lambda (&rest args)
        `(progn
           (let ((res ""))
             (dolist (x (fclosure ',#main))
               (incf res x))
             (incf res (js-compile '(funcall #',#main ,@args)))
             (display (minimize res)))
           null)))
