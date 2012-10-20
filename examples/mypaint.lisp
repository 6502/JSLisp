(import * from gui)
(import * from graphics)
(import * from examples/raster)
(import * from layout)

(defun color-button (parent color callback)
  (let ((div (create-element "div")))
    (set-style div
               position "absolute"
               border ~"solid 1px #000000"
               backgroundColor ~"rgb({(first color)},{(second color)},{(third color)})")
    (append-child parent div)

    (set-handler div oncontextmenu
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation)))

    (set-handler div onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (funcall callback color (. event button) div))
    div))

(defun luma (color)
  (+ (first color) (* 2 (second color)) (* 0.9 (third color))))

(defun palette (parent rows w h callback)
  (declare (ignorable rows))
  (let ((colors (list))
        (layout (H border: 4 (flow spacing: 4))))
    (dotimes (r 2)
      (dotimes (g 2)
        (dotimes (b 2)
          (push (list (* r 255) (* g 255) (* b 255) 255) colors)
          (push (list (+ 64 (* r 128)) (+ 64 (* g 128)) (+ 64 (* b 128)) 255) colors))))
    (dolist (c (sort colors (lambda (a b) (> (luma a) (luma b)))))
      (add-element (first layout.elements).element
                   width: w height: h (dom (color-button parent c callback))))
    layout))

(defobject paint
  (frame
   pic
   data
   old-data
   start-data
   (fullredraw false)
   (commands (list))
   (undone (list))
   (zoom 1)
   (bg (list 255 255 255 255))
   (fg (list   0   0   0 255))
   (pen 4)
   (selection null)
   (selection-div null)))

(defun copy-pixels (dst src)
  "Copies all pixels from ImageData [src] to ImageData [dst] (they must have the same size)"
  (let ((width (. dst width))
        (height (. dst height))
        (src-data (. src data))
        (dst-data (. dst data)))
    (dotimes (i (* width height 4))
      (setf (aref dst-data i) (aref src-data i)))))

(defvar *tools* (list))

(defmacro deftool (name &rest body)
  `(progn
     (defun ,name (pw)
       (setf pw.selection null)
       ,@body)
     (push ',name *tools*)))

(defmacro exec (vars &rest body)
  `(let (,@(map (lambda (x) `(,x ,x)) vars))
     (push (lambda () ,@body) pw.commands)
     (funcall (last pw.commands))))

(deftool Pen
    (let ((pts (list)))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (push null pw.commands)
           (copy-pixels pw.old-data pw.data)
           (setf pts (list p)))
          ((= msg 'move)
           (let* ((sz pw.pen)
                  (hsz (ash sz -1))
                  (color (if (= btn 2) pw.bg pw.fg)))
             (copy-pixels pw.data pw.old-data)
             (pop pw.commands)
             (push p pts)
             (exec (pts sz hsz color)
                   (dotimes (i (1- (length pts)))
                     (let ((p0 (aref pts i))
                           (p (aref pts (1+ i))))
                       (line pw.data
                             (- (first p0) hsz) (- (second p0) hsz)
                             (- (first p) hsz) (- (second p) hsz)
                             sz sz color))))))))))

(deftool Fill
    (lambda (msg p btn)
      (when (= msg 'down)
        (let ((color (if (= btn 2) pw.bg pw.fg)))
          (exec (p color)
                (fill pw.data (first p) (second p) color))))))

(deftool Line
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (copy-pixels pw.old-data pw.data)
           (push null pw.commands)
           (setf p0 p))
          ((= msg 'move)
           (let* ((sz pw.pen)
                  (hsz (ash sz -1))
                  (color (if (= btn 2) pw.bg pw.fg)))
             (copy-pixels pw.data pw.old-data)
             (pop pw.commands)
             (exec (hsz sz p0 p color)
                   (line pw.data
                         (- (first p0) hsz) (- (second p0) hsz)
                         (- (first p) hsz) (- (second p) hsz)
                         sz sz
                         color))))))))

(deftool Curve
    (let ((pts (make-array 4))
          (n 0))
          (lambda (msg p btn)
            (cond
              ((= msg 'down)
               (cond
                 ((or (= n 0) (= n 4))
                  (copy-pixels pw.old-data pw.data)
                  (setf (aref pts 0) p)
                  (setf (aref pts 1) p)
                  (setf (aref pts 2) p)
                  (setf (aref pts 3) p)
                  (setf n 2)
                  (push null pw.commands))
                 ((= n 2)
                  (setf (aref pts 1) p)
                  (setf (aref pts 2) p)
                  (setf n 3))
                 (true
                  (setf (aref pts 2) p)
                  (setf n 4))))
              ((= msg 'move)
               (cond
                 ((= n 2)
                  (setf (aref pts 2) p)
                  (setf (aref pts 3) p))
                 ((= n 3)
                  (setf (aref pts 1) p)
                  (setf (aref pts 2) p))
                 ((= n 4)
                  (setf (aref pts 2) p)))
               (copy-pixels pw.data pw.old-data)
               (pop pw.commands)
               (let ((color (if (= btn 2) pw.bg pw.fg))
                     (p0 (aref pts 0))
                     (p1 (aref pts 1))
                     (p2 (aref pts 2))
                     (p3 (aref pts 3))
                     (sz pw.pen))
                 (exec (color p0 p1 p2 p3)
                       (bezier pw.data p0 p1 p2 p3 sz sz color))))))))

(deftool Box
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (copy-pixels pw.old-data pw.data)
           (push null pw.commands)
           (setf p0 p))
          ((= msg 'move)
           (copy-pixels pw.data pw.old-data)
           (pop pw.commands)
           (let ((color (if (= btn 2) pw.bg pw.fg)))
             (exec (p0 p color)
                   (box pw.data
                        (min (first p0) (first p))
                        (min (second p0) (second p))
                        (max (first p0) (first p))
                        (max (second p0) (second p))
                        color))))))))

(deftool Box*
  (let ((p0 null))
    (lambda (msg p btn)
      (cond
        ((= msg 'down)
         (copy-pixels pw.old-data pw.data)
         (push null pw.commands)
         (setf p0 p))
        ((= msg 'move)
         (copy-pixels pw.data pw.old-data)
         (pop pw.commands)
         (let ((color (if (= btn 2) pw.bg pw.fg))
               (sz pw.pen))
           (exec (p0 p color sz)
                 (frame pw.data
                        (min (first p0) (first p))
                        (min (second p0) (second p))
                        (max (first p0) (first p))
                        (max (second p0) (second p))
                        sz sz
                        color))))))))

(deftool Ellipse
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (setf p0 p)
           (copy-pixels pw.old-data pw.data)
           (push null pw.commands))
          ((= msg 'move)
           (let ((color (if (= btn 2) pw.bg pw.fg)))
             (copy-pixels pw.data pw.old-data)
             (pop pw.commands)
             (exec (p0 p color)
                   (let ((x0 (min (first p0) (first p)))
                         (x1 (max (first p0) (first p)))
                         (y0 (min (second p0) (second p)))
                         (y1 (max (second p0) (second p))))
                     (ellipse pw.data x0 y0 x1 y1 color)))))))))

(deftool Ellipse*
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (setf p0 p)
           (copy-pixels pw.old-data pw.data)
           (push null pw.commands))
          ((= msg 'move)
           (let ((color (if (= btn 2) pw.bg pw.fg))
                 (sz pw.pen))
             (copy-pixels pw.data pw.old-data)
             (pop pw.commands)
             (exec (p0 p color sz)
                   (let ((x0 (min (first p0) (first p)))
                         (x1 (max (first p0) (first p)))
                         (y0 (min (second p0) (second p)))
                         (y1 (max (second p0) (second p))))
                     (ellipse-frame pw.data x0 y0 x1 y1 sz sz color)))))))))

(deftool Select
    (let ((p0 null))
      (lambda (msg p btn)
        (declare (ignorable btn))
        (cond
          ((= msg 'down)
           (setf p0 p))
          ((= msg 'move)
           (setf pw.selection
                 (list p0 p null)))))))

(deftool Zoom+
    (incf pw.zoom)
    (setf pw.fullredraw true))

(deftool Zoom-
    (when (> pw.zoom 1)
      (decf pw.zoom)
      (setf pw.fullredraw true)))

(deftool Undo
    (when (length pw.commands)
      (push (pop pw.commands) pw.undone)
      (copy-pixels pw.data pw.start-data)
      (dolist (f pw.commands) (funcall f))))

(deftool Redo
    (when (length pw.undone)
      (push (pop pw.undone) pw.commands)
      (funcall (last pw.commands))))

(deftool Save
    (let* ((pic pw.pic)
           (ctx (funcall (. pic getContext) "2d")))
      (funcall (. ctx putImageData) pw.data 0 0)
      (let ((s (funcall (. pic toDataURL) "image/png")))
        (when (= (slice s 0 14) "data:image/png")
          (setf s (+ "data:image/octect-stream" (slice s 14)))
          (js-code "document.location.href=d$$s")))))

(defmacro deftoolbar ()
  `(defun toolbar (parent cols callback)
     "Builds a toolbar with all self-registered tool functions"
     (let (,@(map (lambda (x)
                    `(,x (button ,(symbol-name x)
                                 (lambda () (funcall callback #',x)))))
               *tools*))
       ,@(map (lambda (x)
                `(append-child parent ,x))
              *tools*)
       (let ((layout (V spacing: 4))
             (crow null))
         (dolist (x (list ,@*tools*))
           (unless crow
             (setf crow (H spacing: 4))
             (add-element layout crow))
           (add-element crow (dom x))
           (when (= (length crow.elements) cols)
             (setf crow null)))
         layout))))

(deftoolbar)

(defun update (view pw)
  (let ((view-width (. view offsetWidth))
        (view-height (. view offsetHeight))
        (zoom pw.zoom)
        (full pw.fullredraw))
    (setf pw.fullredraw false)
    (when (or (/= (. view width) view-width)
              (/= (. view height) view-height))
      (setf (. view width) view-width)
      (setf (. view height) view-height))
    (if (= zoom 1)
        ;; 1:1, just blit
        (let ((ctx (funcall (. view getContext) "2d")))
          (setf (. ctx fillStyle) "#DDDDDD")
          (funcall (. ctx fillRect) 0 0 view-width view-height)
          (funcall (. ctx putImageData) pw.data 0 0))

        ;; Zoomed. Unfortunately there's no way to turn off reilably antialiasing
        ;; Draw by hand one pixel at a time.
        (let* ((ctx (funcall (. view getContext) "2d"))
               (view-data (progn
                            (when full
                              (setf (. ctx fillStyle) "#DDDDDD")
                              (funcall (. ctx fillRect) 0 0 view-width view-height))
                            (funcall (. ctx getImageData) 0 0 view-width view-height)))
               (dst (. view-data data))
               (src (. pw.data data))
               (pic-width (. pw.pic width))
               (pic-height (. pw.pic height))
               (maxy (min pic-height (floor (/ view-height zoom))))
               (maxx (min pic-width (floor (/ view-width zoom))))
               (zoom4 (* zoom 4))
               (up (* view-width zoom 4)))
          (dotimes (i maxy)
            (let ((wp (* i view-width zoom 4))
                  (rp (* i pic-width 4))
                  (next (* (- view-width zoom) 4)))
              (repeat maxx
                (let ((r (aref src rp))
                      (g (aref src (+ rp 1)))
                      (b (aref src (+ rp 2)))
                      (a (aref src (+ rp 3))))
                  (when (or full
                            (/= r (aref dst wp))
                            (/= g (aref dst (+ wp 1)))
                            (/= b (aref dst (+ wp 2)))
                            (/= a (aref dst (+ wp 3))))
                    (repeat zoom
                      (repeat zoom
                        (setf (aref dst wp) r)
                        (setf (aref dst (+ wp 1)) g)
                        (setf (aref dst (+ wp 2)) b)
                        (setf (aref dst (+ wp 3)) a)
                        (incf wp 4))
                      (incf wp next))
                    (decf wp up))
                  (incf wp zoom4)
                  (incf rp 4)))))
          (funcall (. ctx putImageData) view-data 0 0))))
  (if pw.selection
      (let* ((p0 (first pw.selection))
             (p1 (second pw.selection))
             (x0 (min (first p0) (first p1)))
             (y0 (min (second p0) (second p1)))
             (x1 (max (first p0) (first p1)))
             (y1 (max (second p0) (second p1)))
             (zoom pw.zoom))
        (set-style pw.selection-div
                   display "block"
                   px/left (1- (* zoom x0))
                   px/top (1- (* zoom y0))
                   px/width (* zoom (- x1 x0))
                   px/height (* zoom (- y1 y0))))
      (set-style pw.selection-div
                 display "none")))

(defun paint (x y w h title pic)
  (let* ((frame (window x y w h title: title))
         (view-div (create-element "div"))
         (view (create-element "canvas"))
         (current-tool null)
         (pw (make-paint frame: w
                         pic: pic
                         selection-div: (create-element "div")))
         (palette (palette frame.client 2 30 30
                           (lambda (color button div)
                             (declare (ignorable button div))
                             (if (= button 2)
                                 (setf pw.bg color)
                                 (setf pw.fg color)))))
         (toolbar (toolbar frame.client 2
                           (lambda (f)
                             (let ((x (funcall f pw)))
                               (when x
                                 (setf current-tool x))
                               (update view pw)))))
         (layout (V spacing: 8 border: 8
                    (H  size: 140 (H toolbar)
                        size: undefined (dom view-div))
                    size: 68
                    palette)))

    (set-style view-div
               position "absolute")

    (set-style pw.selection-div
               zIndex 999
               position "absolute"
               border "solid 1px #FF0000"
               backgroundColor "rgba(255,0,0,0.125)"
               display "none"
               px/top 0
               px/left 0
               px/width 0
               px/height 0)

    (setf pw.zoom 1)
    (setf pw.pen 1)

    (set-style view
               position "absolute"
               cursor "crosshair")

    (set-style frame.client
               backgroundColor "#CCCCCC"
               overflow "hidden")

    (set-style frame.frame
               backgroundColor "#CCCCCC")

    (append-child view-div view)
    (append-child view-div pw.selection-div)

    (append-child frame.client view-div)

    (let ((ctx (funcall (. pic getContext) "2d"))
          (width (. pic width))
          (height (. pic height)))

      (setf pw.data (funcall (. ctx getImageData) 0 0 width height))
      (setf pw.old-data (funcall (. ctx getImageData) 0 0 width height))
      (setf pw.start-data (funcall (. ctx getImageData) 0 0 width height)))

    (set-handler view oncontextmenu
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation)))
    (set-handler view onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (let* ((p (event-pos event))
                        (p0 (element-pos view))
                        (x (- (first p) (first p0)))
                        (y (- (second p) (second p0)))
                        (z pw.zoom))
                   (when current-tool
                     (funcall current-tool 'down (list (floor (/ x z)) (floor (/ y z))) (. event button))
                     (funcall current-tool 'move (list (floor (/ x z)) (floor (/ y z))) (. event button))
                     (update view pw)
                     (tracking (lambda (xx yy)
                                 (let ((x (- xx (first p0)))
                                       (y (- yy (second p0))))
                                   (funcall current-tool 'move (list (floor (/ x z)) (floor (/ y z))) (. event button))
                                   (update view pw)))
                               (lambda (xx yy)
                                 (let ((x (- xx (first p0)))
                                       (y (- yy (second p0))))
                                   (funcall current-tool 'up (list (floor (/ x z)) (floor (/ y z))) (. event button))
                                   (update view pw)))
                               "crosshair"))))
    (setf frame.resize-cback
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))
            (set-style view
                       px/width view-div.offsetWidth
                       px/height view-div.offsetHeight)
            (update view pw)))
    (show-window frame)
    pw))

(defun main (w h)
  (let ((pic (create-element "canvas")))
    (setf (. pic width) w)
    (setf (. pic height) h)
    (let* ((ctx (funcall (. pic getContext) "2d"))
           (data (funcall (. ctx getImageData) 0 0 w h)))
      (clear data (list 255 255 255 255))
      (funcall (. ctx putImageData) data 0 0))
    (paint 100 100 640 480 "MyPaint" pic)))

(main 320 200)
