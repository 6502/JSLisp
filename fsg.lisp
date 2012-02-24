(defvar *screen* (create-element "div"))
(setf (. *screen* style position) "absolute")
(setf (. *screen* style left) "0px")
(setf (. *screen* style right) "0px")
(setf (. *screen* style top) "0px")
(setf (. *screen* style bottom) "0px")
(setf (. *screen* style overflow) "hidden")
(setf (. *screen* style pointerEvents) "none")
(append-child (. document body) *screen*)
(defvar *canvas* (create-element "canvas"))
(append-child *screen* *canvas*)
(setf (. *canvas* style pointerEvents) "none")
(defvar *dc* (funcall (. *canvas* getContext) "2d"))

(defun clear ()
  "Clears all graphics"
  (setf (. *canvas* width) (. *screen* offsetWidth))
  (setf (. *canvas* height) (. *screen* offsetHeight))
  null)

(defun fill-style (s)
  "Sets current fill style"
  (setf (. *dc* fillStyle) s))

(defun stroke-style (s)
  "Sets current stroke style"
  (setf (. *dc* strokeStyle) s))

(defun line-width (s)
  "Sets current line width"
  (setf (. *dc* lineWidth) s))

(defmacro defgfx (lisp-name doc js-name &rest args)
  `(defun ,lisp-name ,args ,doc (funcall (. *dc* ,js-name) ,@args)))

(defgfx begin-path
    "Starts a new path"
  beginPath)

(defgfx close-path
    "Closes current path by connecting current point to start point"
  closePath)

(defgfx move-to
    "Moves the current point to (x, y)"
  moveTo x y)

(defgfx line-to
    "Draws a line from current point to (x, y)"
  lineTo x y)

(defgfx bez2-to
    "Draws a quadratic bezier from current point, using control point (x1, y1) and arriving to (x2, y2)"
  quadraticCurveTo x1 y1 x2 y2)

(defgfx bez3-to
    "Draws a cubic bezier from current point, using control points (x1, y1), (x2, y2) and arriving to (x3, y3)"
    bezierCurveTo x1 y1 x2 y2 x3 y3)

(defgfx arc-to
    "?"
  arcTo x1 y1 x2 y2 radius)

(defgfx arc
    "Draws and arc with center (x, y), radius r, starting angle a0 and
     ending angle a1. Parameter ccw decides if the arc should be
     drawn counterclockwise (true) or clockwise (false)."
    arc x y r a0 a1 ccw)

(defgfx fill
    "Fills current path (eventually by first closing it) with current fill style"
  fill)

(defgfx stroke
    "Draws the boundary of current path with current stroke style and line width"
    stroke)

(defgfx clip
    "Clips subsequent draw operations to current path"
    clip)

(clear)