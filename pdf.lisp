(defvar *pdf* null)

(defvar PDFDocument (js-code "require('pdfkit')"))

(defun new-pdf (&optional (size "a4") (layout "portrait"))
  (js-code "(new dpdf$$PDFDocument({size:d$$size, layout:dpdf$$layout}))"))

(defmacro pdf (options &rest body)
  `(let ((*pdf* (new-pdf ,@options)))
     ,@body
     *pdf*))

(defun new-page (&optional size layout)
  (if size
      (if layout
          (*pdf*.addPage (js-object (size size)
                                    (layout layout)))
          (*pdf*.addPage (js-object (size size))))
      (*pdf*.addPage)))

(defun text (text &optional x y)
  (*pdf*.text text x y))

(defun font (name &optional style)
  (*pdf*.font name style))

(defun font-size (size)
  (*pdf*.fontSize size))

(defun down (step)
  (*pdf*.moveDown step))

(defun line-width (size)
  (*pdf*.lineWidth size))

(defun stroke-color (color)
  (*pdf*.strokeColor color))

(defun fill-color (color)
  (*pdf*.fillColor color))

(defun stroke-opacity (opacity)
  (*pdf*.strokeOpacity opacity))

(defun fill-opacity (opacity)
  (*pdf*.fillOpacity opacity))

(defun opacity (opacity)
  (*pdf*.opacity opacity))

(defun move-to (x y)
  (*pdf*.moveTo x y))

(defun line-to (x y)
  (*pdf*.lineTo x y))

(defun quadratic-curve-to (x1 y1 x2 y2)
  (*pdf*.quadraticCurveTo x1 y1 x2 y2))

(defun bezier-curve-to (x1 y1 x2 y2 x3 y3)
  (*pdf*.bezierCurveTo x1 y1 x2 y2 x3 y3))

(defun stroke (&optional color)
  (*pdf*.stroke color))

(defun fill (&optional color)
  (*pdf*.fill color))

(defun fill-and-stroke (&optional fill-color stroke-color)
  (*pdf*.fillAndStroke fill-color stroke-color))

(defun save ()
  (*pdf*.save))

(defun restore ()
  (*pdf*.restore))

(export pdf new-page text font font-size down
        line-width
        stroke-color fill-color
        stroke-opacity fill-opacity
        opacity
        move-to line-to quadratic-curve-to bezier-curve-to
        stroke fill fill-and-stroke)
