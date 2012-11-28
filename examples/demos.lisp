(import * from gui)

(defvar *menu* null)

(defun make-menu ()
  (setf *menu* (set-style (create-element "div")
                          position "absolute"
                          px/left 0
                          px/right 0
                          px/top 0
                          px/height 35
                          backgroundColor "#E0E0E0"))
  (append-child document.body *menu*))

(defun add-menu-option (name cback)
  (let ((btn (button name cback)))
    (let ((prev *menu*.lastChild))
      (append-child *menu* btn)
      (set-style btn
                 position "absolute"
                 px/left (+ (if prev (+ prev.offsetLeft prev.offsetWidth) 0) 2)
                 px/top 2
                 px/bottom 2))))

(defvar *main-calls* (list))

(defmacro import-nomain (module)
  (setf (symbol-macro (intern "main" (symbol-name module)))
        (lambda (&rest args)
          `(progn
             (push (list *current-module*
                         (lambda ()
                           (funcall #',#"main" ,@args)))
                   *main-calls*)
             'deferred)))
  `(import ,module))

(import-nomain examples/3dboard)
(import-nomain examples/mandelbrot)
(import-nomain examples/life)
(import-nomain examples/piechart)
(import-nomain examples/hexa)
(import-nomain examples/guitest)
(import-nomain examples/3d)
(import-nomain examples/mypaint)
(import-nomain examples/flood-fill)
(import-nomain examples/asteroids)

(defun main ()
  (make-menu)
  (dolist ((name f) *main-calls*)
    (add-menu-option (slice name 9) f)))

(main)
