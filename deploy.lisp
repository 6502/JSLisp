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
a regular function, the value of closed over variables will be lost and
the contained function will access instead global javascript
variables.

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
(defun minimize (s)
  (let ((seen #())
        (lits #())
        (rlits #())
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
      ;; String literals collection
      (setf s (replace s "\"([^\"\\\\]|\\\\.)*\"" #'newlit))
      (setf s (replace s "'([^'\\\\]|\\\\.)*'" #'newlit))
      ;; Code metainformation removal
      (setf s (replace s "f\\.usedglobs=\\[[^\\]]*\\];f\\.outcalls=\\[[^\\]]*\\];f\\.arglist=lisp_literals\\[[^\\]]*\\];" ""))
      ;; Name shortening
      (setf s (replace s "([a-zA-Z0-9_]|\\$[0-9]+_)+\\$\\$[a-zA-Z0-9_$]*" #'newname))
      ;; Lisp literals renaming
      (setf s (replace s "lisp_literals\\[[0-9]+\\]*" #'newname))
      (setf s (replace s "lisp_literals=\\[\\];" ""))
      ;; Extra parenthesis removal
      (dotimes (i 5)
        (setf s (replace s "([-+*/<>=!~&|\\(])\\((-?[0-9]+\\.?[0-9]*|[a-zA-Z_\\$][a-zA-Z_0-9\\$]*)\\)" "$1$2")))
      (setf s (replace s "\\[\\(([^\\]()]+)\\)\\]" "[$1]"))
      (dotimes (i 5)
        (setf s (replace s "(\\(|,)\\(([^(),]+)\\)(\\)|,)" "$1$2$3")))
      (setf s (replace s "([^-])(--)+([^-])" "$1+$3"))
      (setf s (replace s "([^-])-(--)+([^-])" "$1-$3"))
      (setf s (replace s "([^-+])\\+-([^-+])" "$1-$2"))
      ;; Unneeded semicolon and space removal
      (setf s (replace s "\\n" " "))
      (setf s (replace s " *([-:?&|;(){}=+*/,]) *" "$1"))
      (setf s (replace s ";}" "}"))
      ;; Restore object-creation literals
      (setf s (replace s "{_[a-zA-Z0-9]+:" (lambda (x) ~"\\{{(aref rlits (slice x 1 (1- (length x))))}:")))
      (setf s (replace s ",_[a-zA-Z0-9]+:" (lambda (x) ~",{(aref rlits (slice x 1 (1- (length x))))}:")))
      ;; Remove unneeded quotes in object field ids
      (setf s (replace s "{\"([a-zA-Z_$][a-zA-Z_$0-9]*)\":" "{$1:"))
      (setf s (replace s ",\"([a-zA-Z_$][a-zA-Z_$0-9]*)\":" ",$1:"))
      ;; "function(" packing
      (when (find "'" s)
        (error "Internal error: Unexpected single quote"))
      (setf s (+ "eval(\""
                 (replace (replace (replace s "function\\(" "'")
                                   "\\\\" "\\\\")
                          "\"" "\\\"")
                 "\".replace(/'/g,\"function(\"))"))
      ;; Add used literals definitions
      (let ((ldef ""))
        (dolist (key (keys rlits))
          (when (find key s)
            (setf ldef ~"{key}={(aref rlits key)};{ldef}")))
        (setf s (+ ldef s)))
      (+ s "//JsLisp"))))

(defvar *repo* (list))

(defvar *repcode* "lisp_literals=[];\
                   d$$$42_exception$42_=null;\
                   Symbol=function(n){this.name=n};\
                   s=function(n){return new Symbol(n)};\
                   a=function(x,L){x.push.apply(x,L)};")

(defvar *globs* #())

(if node-js
    (setf #'warning
          (lambda (x) (js-code "process.stderr.write(\"WARNING: \"+d$$x+\"\\n\")"))))

(defun rep (x)
  (cond
    ((undefined? x) "undefined")
    ((null? x) "null")
    ((infinity? x) "Infinity")
    ((-infinity? x) "-Infinity")
    ((number? x) (json x))
    ((bool? x) (json x))
    ((string? x)
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (incf *repcode* ~"r{ix}={(json x)};"))
       ~"r{ix}"))
    ((symbol? x)
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (incf *repcode* ~"r{ix}=s({(json (symbol-full-name x))});"))
       ~"r{ix}"))
    ((list? x)
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (incf *repcode* ~"r{ix}=[];")
         (let ((L "[")
               (sep ""))
           (dolist (y x)
             (incf L sep)
             (incf L (rep y))
             (setf sep ","))
           (incf L "]")
           (unless (= L "[]")
             (incf *repcode* ~"a(r{ix},{L});"))))
       ~"r{ix}"))
    ((object? x)
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (incf *repcode* ~"r{ix}=\\{\\};")
         (dolist (k (keys x))
           (let ((ii (if ((regexp "^[a-zA-Z_$][a-zA-Z_0-9$]*$").exec k)
                         ~"r{ix}.{k}={(rep (aref x k))};"
                         ~"r{ix}[{(rep k)}]={(rep (aref x k))};")))
             (incf *repcode* ii))))
       ~"r{ix}"))
    ((and x x."%class")
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (let ((constructor (rep (symbol-function (intern (+ (first x.%class) "-constructor"))))))
           (incf *repcode* ~"r{ix}=new {constructor}();")
           (dolist (k (rest x.%class))
             (let ((ii (if ((regexp "^[a-zA-Z_$][a-zA-Z_0-9$]*$").exec k)
                           ~"r{ix}.{k}={(rep (aref x k))};"
                           ~"r{ix}[{(rep k)}]={(rep (aref x k))};")))
               (incf *repcode* ii)))))
       ~"r{ix}"))
    ((callable? x)
     (let ((ix (index x *repo*)))
       (when (= ix -1)
         (setf ix (length *repo*))
         (push x *repo*)
         (let ((ii (replace (+ "" x) "\r" "")))
           (setf ii (replace ii "f.usedglobs=\\[[^\\]]*\\];f.outcalls=\\[[^\\]]*\\];f.arglist=[^;]*;" ""))
           (incf *repcode* ~"r{ix}={ii};"))
         (when x.usedglobs
           (dolist (y x.usedglobs)
             (unless (aref *globs* y)
               (setf (aref *globs* y) true)
               (cond
                 ((= (slice y 0 2) "q#")
                  (let ((v (aref (js-code "lisp_literals") (slice y 2))))
                    (let ((ii ~"lisp_literals[{(slice y 2)}]={(rep v)};"))
                      (incf *repcode* ii))))
                 ((= (slice y 0 2) "s#")
                  (let ((v (aref (js-code "glob") (+ "s" (slice y 2)))))
                    (let ((ii ~"s{(slice y 2)}={(rep v)};"))
                      (incf *repcode* ii))))
                 (true
                  (let ((v (aref (js-code "glob") (+ "d" y))))
                    (let ((ii ~"d{y}={(rep v)};"))
                      (incf *repcode* ii))))))))
         (when x.outcalls
           (dolist (y x.outcalls)
             (setf y (+ "f" y))
             (unless (aref *globs* y)
               (setf (aref *globs* y) true)
               (let ((v (aref (js-code "glob") y)))
                 (if v
                     (progn
                       (unless v.usedglobs
                         (warning ~"Reaching outer function {y}"))
                       (let ((ii ~"{y}={(rep v)};"))
                         (incf *repcode* ii)))
                     (warning ~"Undefined function {y}"))))))
         (when x.prototype.%class
           (let ((ii (rep x.prototype.%class)))
             (incf *repcode* ~"Object.defineProperty(r{ix}.prototype,\
                               \"%class\",\
                               \\{enumerable:false,value:{ii}\\});"))
           (dolist ((name getter setter) x.prototype.%properties)
             (let ((i0 (rep getter))
                   (i1 (rep setter)))
               (incf *repcode* ~"Object.defineProperty(r{ix}.prototype,{(json name)},\\{get:{i0},set:{i1}\\});")))
           (let ((ii (rep x.prototype.%copy)))
             (incf *repcode* ~"Object.defineProperty(r{ix}.prototype,\
                               \"%copy\",\
                               \\{enumerable:false,value:{ii}\\});"))))
       ~"r{ix}"))
    (true (error ~"Unable to deploy value {x}"))))

(setf *deploy-prefix* "")
(setf *deploy-suffix* "")

(setf (symbol-macro (intern "main" ""))
      (lambda (&rest args)
        `(progn
           (let ((m (rep (lambda ,args (funcall #',#"main" ,@args)))))
             (display (+ *deploy-prefix*
                         (minimize (+ *repcode* m "()"))
                         *deploy-suffix*)))
           null)))
