(import * from gui)
(import * from graphics)
(import * from examples/raster)

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
  (let ((colors (list))
        (layout (:H :spacing 4))
        (crow null))
    (dotimes (r 2)
      (dotimes (g 2)
        (dotimes (b 2)
          (push (list (* r 255) (* g 255) (* b 255) 255) colors)
          (push (list (+ 64 (* r 128)) (+ 64 (* g 128)) (+ 64 (* b 128)) 255) colors))))
    (dolist (c (sort colors (lambda (a b) (> (luma a) (luma b)))))
      (when (null? crow)
        (setf crow (:V :size w :spacing 4 (:V)))
        (push crow (layout-node-children layout)))
      (push (:Vdiv (color-button parent c callback) :size w)
            (layout-node-children crow))
      (when (= (length (layout-node-children crow)) (1+ rows))
        (setf crow null)))
    (:H :border 4
        (:V layout)
        (:H))))

(defstruct paint
  frame
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
  (selection null))

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
     (defun ,name (pw) ,@body)
     (push ',name *tools*)))

(defmacro exec (vars &rest body)
  `(let (,@(map (lambda (x) `(,x ,x)) vars))
     (push (lambda () ,@body) (paint-commands pw))
     (funcall (last (paint-commands pw)))))

(deftool Pen
    (let ((pts (list)))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (push null (paint-commands pw))
           (copy-pixels (paint-old-data pw) (paint-data pw))
           (setf pts (list p)))
          ((= msg 'move)
           (let* ((sz (paint-pen pw))
                  (hsz (ash sz -1))
                  (color (if (= btn 2) (paint-bg pw) (paint-fg pw))))
             (copy-pixels (paint-data pw) (paint-old-data pw))
             (pop (paint-commands pw))
             (push p pts)
             (exec (pts sz hsz color)
                   (dotimes (i (1- (length pts)))
                     (let ((p0 (aref pts i))
                           (p (aref pts (1+ i))))
                       (line (paint-data pw)
                             (- (first p0) hsz) (- (second p0) hsz)
                             (- (first p) hsz) (- (second p) hsz)
                             sz sz color))))))))))

(deftool Fill
    (lambda (msg p btn)
      (when (= msg 'down)
        (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw))))
          (exec (p color)
                (fill (paint-data pw) (first p) (second p) color))))))

(deftool Line
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (copy-pixels (paint-old-data pw) (paint-data pw))
           (push null (paint-commands pw))
           (setf p0 p))
          ((= msg 'move)
           (let* ((sz (paint-pen pw))
                  (hsz (ash sz -1))
                  (color (if (= btn 2) (paint-bg pw) (paint-fg pw))))
             (copy-pixels (paint-data pw) (paint-old-data pw))
             (pop (paint-commands pw))
             (exec (hsz sz p0 p color)
                   (line (paint-data pw)
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
                  (copy-pixels (paint-old-data pw) (paint-data pw))
                  (setf (aref pts 0) p)
                  (setf (aref pts 1) p)
                  (setf (aref pts 2) p)
                  (setf (aref pts 3) p)
                  (setf n 2)
                  (push null (paint-commands pw)))
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
               (copy-pixels (paint-data pw) (paint-old-data pw))
               (pop (paint-commands pw))
               (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw)))
                     (p0 (aref pts 0))
                     (p1 (aref pts 1))
                     (p2 (aref pts 2))
                     (p3 (aref pts 3))
                     (sz (paint-pen pw)))
                 (exec (color p0 p1 p2 p3)
                       (bezier (paint-data pw) p0 p1 p2 p3 sz sz color))))))))

(deftool Box
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (copy-pixels (paint-old-data pw) (paint-data pw))
           (push null (paint-commands pw))
           (setf p0 p))
          ((= msg 'move)
           (copy-pixels (paint-data pw) (paint-old-data pw))
           (pop (paint-commands pw))
           (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw))))
             (exec (p0 p color)
                   (box (paint-data pw)
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
         (copy-pixels (paint-old-data pw) (paint-data pw))
         (push null (paint-commands pw))
         (setf p0 p))
        ((= msg 'move)
         (copy-pixels (paint-data pw) (paint-old-data pw))
         (pop (paint-commands pw))
         (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw)))
               (sz (paint-pen pw)))
           (exec (p0 p color sz)
                 (frame (paint-data pw)
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
           (copy-pixels (paint-old-data pw) (paint-data pw))
           (push null (paint-commands pw)))
          ((= msg 'move)
           (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw))))
             (copy-pixels (paint-data pw) (paint-old-data pw))
             (pop (paint-commands pw))
             (exec (p0 p color)
                   (let ((x0 (min (first p0) (first p)))
                         (x1 (max (first p0) (first p)))
                         (y0 (min (second p0) (second p)))
                         (y1 (max (second p0) (second p))))
                     (ellipse (paint-data pw) x0 y0 x1 y1 color)))))))))

(deftool Ellipse*
    (let ((p0 null))
      (lambda (msg p btn)
        (cond
          ((= msg 'down)
           (setf p0 p)
           (copy-pixels (paint-old-data pw) (paint-data pw))
           (push null (paint-commands pw)))
          ((= msg 'move)
           (let ((color (if (= btn 2) (paint-bg pw) (paint-fg pw)))
                 (sz (paint-pen pw)))
             (copy-pixels (paint-data pw) (paint-old-data pw))
             (pop (paint-commands pw))
             (exec (p0 p color sz)
                   (let ((x0 (min (first p0) (first p)))
                         (x1 (max (first p0) (first p)))
                         (y0 (min (second p0) (second p)))
                         (y1 (max (second p0) (second p))))
                     (ellipse-frame (paint-data pw) x0 y0 x1 y1 sz sz color)))))))))

(deftool Zoom+
    (incf (paint-zoom pw))
    (setf (paint-fullredraw pw) true))

(deftool Zoom-
    (when (> (paint-zoom pw) 1)
      (decf (paint-zoom pw))
      (setf (paint-fullredraw pw) true)))

(deftool Undo
    (when (length (paint-commands pw))
      (push (pop (paint-commands pw)) (paint-undone pw))
      (copy-pixels (paint-data pw) (paint-start-data pw))
      (dolist (f (paint-commands pw)) (funcall f))))

(deftool Redo
    (when (length (paint-undone pw))
      (push (pop (paint-undone pw)) (paint-commands pw))
      (funcall (last (paint-commands pw)))))

(deftool Save
    (let* ((pic (paint-pic pw))
           (ctx (funcall (. pic getContext) "2d")))
      (funcall (. ctx putImageData) (paint-data pw) 0 0)
      (let ((s (funcall (. pic toDataURL) "image/png")))
        (when (= (slice s 0 14) "data:image/png")
          (setf s (+ "data:image/octect-stream" (slice s 14)))
          (js-code "document.location.href=d$$s")))))

(defmacro deftoolbar ()
  `(defun toolbar (parent cols w h callback)
     "Builds a toolbar with all self-registered tool functions"
     (let (,@(map (lambda (x)
                    `(,x (button ,(symbol-name x)
                                 (lambda () (funcall callback #',x)))))
               *tools*))
       ,@(map (lambda (x)
                `(append-child parent ,x))
              *tools*)
       (let ((layout (:V :spacing 4))
             (crow null))
         (dolist (x (list ,@*tools*))
           (unless crow
             (setf crow (:H :spacing 4))
             (push crow (layout-node-children layout)))
           (push (:Hdiv x) (layout-node-children crow))
           (when (= (length (layout-node-children crow)) cols)
             (setf crow null)))
         (:H layout)))))

(deftoolbar)

(defun update (view pw)
  (let ((view-width (. view offsetWidth))
        (view-height (. view offsetHeight))
        (zoom (paint-zoom pw))
        (full (paint-fullredraw pw)))
    (setf (paint-fullredraw pw) false)
    (when (or (/= (. view width) view-width)
              (/= (. view height) view-height))
      (setf (. view width) view-width)
      (setf (. view height) view-height))
    (if (= zoom 1)
        ;; 1:1, just blit
        (let ((ctx (funcall (. view getContext) "2d")))
          (setf (. ctx fillStyle) "#DDDDDD")
          (funcall (. ctx fillRect) 0 0 view-width view-height)
          (funcall (. ctx putImageData) (paint-data pw) 0 0))

        ;; Zoomed. Unfortunately there's no way to turn off reilably antialiasing
        ;; Draw by hand one pixel at a time.
        (let* ((ctx (funcall (. view getContext) "2d"))
               (view-data (progn
                            (when full
                              (setf (. ctx fillStyle) "#DDDDDD")
                              (funcall (. ctx fillRect) 0 0 view-width view-height))
                            (funcall (. ctx getImageData) 0 0 view-width view-height)))
               (dst (. view-data data))
               (src (. (paint-data pw) data))
               (pic-width (. (paint-pic pw) width))
               (pic-height (. (paint-pic pw) height))
               (maxy (min pic-height (floor (/ view-height zoom))))
               (maxx (min pic-width (floor (/ view-width zoom))))
               (zoom4 (* zoom 4))
               (up (* view-width zoom 4)))
          (dotimes (i maxy)
            (let ((wp (* i view-width zoom 4))
                  (rp (* i pic-width 4))
                  (next (* (- view-width zoom) 4)))
              (dotimes (j maxx)
                (let ((r (aref src rp))
                      (g (aref src (+ rp 1)))
                      (b (aref src (+ rp 2)))
                      (a (aref src (+ rp 3))))
                  (when (or full
                            (/= r (aref dst wp))
                            (/= g (aref dst (+ wp 1)))
                            (/= b (aref dst (+ wp 2)))
                            (/= a (aref dst (+ wp 3))))
                    (dotimes (ii zoom)
                      (dotimes (jj zoom)
                        (setf (aref dst wp) r)
                        (setf (aref dst (+ wp 1)) g)
                        (setf (aref dst (+ wp 2)) b)
                        (setf (aref dst (+ wp 3)) a)
                        (incf wp 4))
                      (incf wp next))
                    (decf wp up))
                  (incf wp zoom4)
                  (incf rp 4)))))
          (funcall (. ctx putImageData) view-data 0 0)))))

(defun paint (x y w h title pic)
  (let* ((frame (window x y w h :title title))
         (view (create-element "canvas"))
         (current-tool null)
         (pw (make-paint :frame w
                         :pic pic))
         (palette (palette (window-client frame) 2 30 30
                           (lambda (color button div)
                             (if (= button 2)
                                 (setf (paint-bg pw) color)
                                 (setf (paint-fg pw) color)))))
         (toolbar (toolbar (window-client frame) 2 70 30
                           (lambda (f)
                             (let ((x (funcall f pw)))
                               (when x
                                 (setf current-tool x))
                               (update view pw)))))
         (layout (:V :spacing 8 :border 8
                     (:H :spacing 8
                         (:H :size 140 toolbar)
                         (:Hdiv view))
                     (:V :size 68 palette))))

    (setf (paint-zoom pw) 1)
    (setf (paint-pen pw) 1)

    (set-style view
               position "absolute"
               cursor "crosshair")

    (set-style (window-client frame)
               backgroundColor "#CCCCCC"
               overflow "hidden")

    (set-style (window-frame frame)
               backgroundColor "#CCCCCC")

    (append-child (window-client frame) view)

    (let ((ctx (funcall (. pic getContext) "2d"))
          (width (. pic width))
          (height (. pic height)))

      (setf (paint-data pw) (funcall (. ctx getImageData) 0 0 width height))
      (setf (paint-old-data pw) (funcall (. ctx getImageData) 0 0 width height))
      (setf (paint-start-data pw) (funcall (. ctx getImageData) 0 0 width height)))

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
                        (z (paint-zoom pw)))
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
    (setf (window-resize-cback frame)
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))
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
