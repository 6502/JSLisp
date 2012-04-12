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

(defgfx fill-rect
    "Fills a rectangle with current fill style"
    fillRect x0 y0 w h)

(defun random-color ()
  "Returns a random color"
  (let ((red (+ 128 (random-int 64)))
        (green (+ 128 (random-int 64)))
        (blue (+ 128 (random-int 64))))
    ~"rgb({red},{green},{blue})"))

(defun font (x)
  (setf (. *dc* font) x))

(defgfx fill-text
    "Draws the specified text using current fill style"
    fillText text x y)

(defgfx stroke-text
    "Draws the specified text using current stroke style"
    strokeText text x y)

(defun text-width (text)
    "Width of a text line"
    (. (funcall (. *dc* measureText) text) width))

(defun save ()
  "Saves graphic context status"
  (funcall (. *dc* save)))

(defun restore ()
  "Restores graphic context status"
  (funcall (. *dc* restore)))

(defun shadow (color dx dy blur)
  (setf (. *dc* shadowColor) color)
  (setf (. *dc* shadowOffsetX) dx)
  (setf (. *dc* shadowOffsetY) dy)
  (setf (. *dc* shadowBlur) blur))

(clear)

(export *screen* *canvas* *dc*
        clear fill-style stroke-style line-width
        begin-path close-path
        move-to line-to bez-2 bez-3 arc-to arc
        fill stroke clip
        fill-rect
        random-color
        font fill-text stroke-text text-width
        save restore
        shadow)