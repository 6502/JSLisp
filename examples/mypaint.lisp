(import * from gui)
(import * from graphics)

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

(defun hline (data x0 x1 y r g b a)
  "Draws an horizontal line on canvas image [data] from [x0] to [x1] at height [y] with color [r g b a]"
  (let ((width (. data width))
        (height (. data height))
        (pixels (. data data)))
    (do ((p (* (+ (* y width) x0) 4) (+ p 4))
         (count (- x1 x0) (1- count)))
        ((= count 0))
      (setf (aref pixels p)       r)
      (setf (aref pixels (+ p 1)) g)
      (setf (aref pixels (+ p 2)) b)
      (setf (aref pixels (+ p 3)) a))))

(defun box (data x0 y0 x1 y1 color)
  "Fills a box on canvas image [data] from [x0 y0] to [x1 y1] with [color]"
  (let ((width (. data width))
        (height (. data height)))
    (when (< x0 0) (setf x0 0))
    (when (> x1 width) (setf x1 width))
    (when (< y0 0) (setf y0 0))
    (when (> y1 height) (setf y1 height))
    (when (< x0 x1)
      (do ((y y0 (1+ y))
           (r (first color))
           (g (second color))
           (b (third color))
           (a (fourth color)))
          ((>= y y1))
        (hline data x0 x1 y r g b a)))))

(defun clear (data color)
  "Clears the whole canvas image [data] with specified [color]"
  (box data 0 0 (. data width) (. data height) color))

(defun frame (data x0 y0 x1 y1 pw ph color)
  "Draws a rectangular frame with specified pen size [pw ph] and [color]"
  (let ((width (. data width))
        (height (. data height)))
    (if (or (>= (+ x0 pw) (- x1 pw))
            (>= (+ y0 ph) (- y1 ph)))
        (box data x0 y0 x1 y1 color)
        (progn
          (box data x0 y0 x1 (+ y0 ph) color)
          (box data x0 (+ y0 ph) (+ x0 pw) (- y1 ph) color)
          (box data (- x1 pw) (+ y0 ph) x1 (- y1 ph) color)
          (box data x0 (- y1 ph) x1 y1 color)))))

(defun line (data x0 y0 x1 y1 pw ph color)
  "Draws a line fro [x0 y0] to [x1 y1] with specified pen size [pw ph] and [color]"
  (let ((width (. data width))
        (height (. data height)))
    (when (> y0 y1)
      (swap y0 y1)
      (swap x0 x1))
    (let ((xa (max 0 (min x0 x1)))
          (xb (min width (+ pw (max x0 x1)))))
      (if (or (= x0 x1) (= y0 y1))
          (box data xa y0 xb (+ y1 ph) color)
          (let* ((k (/ (- x1 x0) (- y1 y0)))
                 (dx (abs (* ph k))))
            (do ((r (first color))
                 (g (second color))
                 (b (third color))
                 (a (fourth color))
                 (y y0 (1+ y))
                 (yend (+ y1 ph))
                 (left (+ (/ k 2) 0.5 (if (< x0 x1) (- x0 dx) x0)) (+ left k))
                 (right (+ (/ k 2) 0.5 (if (< x0 x1) (+ x0 pw) (+ x0 pw dx))) (+ right k)))
                ((>= y yend))
              (let ((x0 (max (floor left) xa))
                    (x1 (min (floor right) xb)))
                (when (< x0 x1)
                  (hline data x0 x1 y r g b a)))))))))

(defstruct paint
  frame
  pic
  data
  old-data
  (zoom 1)
  (bg (list 255 255 255 255))
  (fg (list   0   0   0 255))
  (pen 2)
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

(deftool Pen
  (let ((p0 null))
    (lambda (msg p btn)
      (cond
        ((= msg 'down)
         (setf p0 p))
        ((= msg 'up)
         (setf p0 null))
        ((= msg 'move)
         (when p0
           (let* ((sz (paint-pen pw))
                  (hsz (ash sz -1)))
             (line (paint-data pw)
                   (- (first p0) hsz) (- (second p0) hsz)
                   (- (first p) hsz) (- (second p) hsz)
                   sz sz
                   (if (= btn 2) (paint-bg pw) (paint-fg pw))))
           (setf p0 p)))))))

(deftool Line
  (let ((p0 null))
    (lambda (msg p btn)
      (cond
        ((= msg 'down)
         (setf p0 p))
        ((= msg 'up)
         (setf p0 null))
        ((= msg 'move)
         (when p0
           (let* ((sz (paint-pen pw))
                  (hsz (ash sz -1)))
             (copy-pixels (paint-data pw) (paint-old-data pw))
             (line (paint-data pw)
                   (- (first p0) hsz) (- (second p0) hsz)
                   (- (first p) hsz) (- (second p) hsz)
                   sz sz
                   (if (= btn 2) (paint-bg pw) (paint-fg pw))))))))))

(deftool Box
  (let ((p0 null))
    (lambda (msg p btn)
      (cond
        ((= msg 'down)
         (setf p0 p))
        ((= msg 'up)
         (setf p0 null))
        ((= msg 'move)
         (when p0
           (copy-pixels (paint-data pw) (paint-old-data pw))
           (box (paint-data pw)
                (min (first p0) (first p))
                (min (second p0) (second p))
                (max (first p0) (first p))
                (max (second p0) (second p))
                (if (= btn 2) (paint-bg pw) (paint-fg pw)))))))))

(deftool Frame
  (let ((p0 null))
    (lambda (msg p btn)
      (cond
        ((= msg 'down)
         (setf p0 p))
        ((= msg 'up)
         (setf p0 null))
        ((= msg 'move)
         (when p0
           (copy-pixels (paint-data pw) (paint-old-data pw))
           (frame (paint-data pw)
                  (min (first p0) (first p))
                  (min (second p0) (second p))
                  (max (first p0) (first p))
                  (max (second p0) (second p))
                  (paint-pen pw) (paint-pen pw)
                  (if (= btn 2) (paint-bg pw) (paint-fg pw)))))))))

(deftool Fill
  (lambda (msg p btn)
    (when (= msg 'down)
      (let* ((src (. (paint-old-data pw) data))
             (width (. (paint-data pw) width))
             (height (. (paint-data pw) height))
             (color (if (= btn 2) (paint-bg pw) (paint-fg pw)))
             (r (first color))
             (g (second color))
             (b (third color))
             (a (fourth color)))
        (labels ((get-pixel (x y)
                   (let ((i (ash (+ (* y width) x) 2)))
                     (+ (aref src i)
                        (ash (aref src (+ i 1)) 8)
                        (ash (aref src (+ i 2)) 16)
                        (ash (aref src (+ i 3)) 24)))))
          (let* ((test-color (get-pixel (first p) (second p)))
                 (diff-color (logxor test-color 1))
                 (r1 (logand diff-color 255)))
            (do ((todo (list p)))
                ((= 0 (length todo)))
              (let* ((p (pop todo))
                     (x (first p))
                     (y (second p)))
                (when (= test-color (get-pixel x y))
                  ;; Move to left if you can
                  (do () ((or (= x 0) (/= test-color (get-pixel (1- x) y)))) (decf x))
                  ;; Horizontal line fill
                  (do ((look-above true)
                       (look-below true)
                       (x0 x))
                      ((or (= x width) (/= test-color (get-pixel x y)))
                       (hline (paint-data pw) x0 x y r g b a)
                       (hline (paint-old-data pw) x0 x y r1 g b a))
                    (when (> y 0)
                      (if look-above
                          (when (= test-color (get-pixel x (1- y)))
                            (push (list x (1- y)) todo)
                            (setf look-above false))
                          (when (/= test-color (get-pixel x (1- y)))
                            (setf look-above true))))
                    (when (< y (1- height))
                      (if look-below
                          (when (= test-color (get-pixel x (1+ y)))
                            (push (list x (1+ y)) todo)
                            (setf look-below false))
                          (when (/= test-color (get-pixel x (1+ y)))
                            (setf look-below true))))
                    (incf x)))))))))))

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
       (:H (:V :spacing 4 :size w
               ,@(map (lambda (x)
                        `(:Hdiv ,x :size h))
                      *tools*))
           (:H)))))

(deftoolbar)

(defun paint (x y w h title)
  (let* ((frame (window x y w h :title title))
         (pic (create-element "canvas"))
         (ctool null)
         (pw (make-paint :frame w
                         :pic pic))
         (palette (palette (window-client frame) 2 30 30
                           (lambda (color button div)
                             (if (= button 2)
                                 (setf (paint-bg pw) color)
                                 (setf (paint-fg pw) color)))))
         (toolbar (toolbar (window-client frame) 1 80 30
                           (lambda (f)
                             (setf ctool (funcall f pw))))))
    (setf (. pic width) 520)
    (setf (. pic height) 340)
    (set-style pic
               position "absolute"
               cursor "crosshair")
    (set-style (window-client frame)
               backgroundColor "#CCCCCC")
    (set-style (window-frame frame)
               backgroundColor "#CCCCCC")
    (append-child (window-client frame) pic)

    (let* ((width (. pic width))
           (height (. pic height))
           (ctx (funcall (. pic getContext) "2d"))
           (data (funcall (. ctx getImageData) 0 0 width height))
           (old-data (funcall (. ctx getImageData) 0 0 width height)))

      (setf (window-resize-cback frame)
            (lambda (x0 y0 x1 y1)
              (set-coords palette 8 8 (- x1 x0 8) (- y1 y0 8))
              (set-coords toolbar 8 8 (- x1 x0 8) (- y1 y0 8))
              (set-style pic
                         px/left 96
                         px/top 8
                         px/width (* (paint-zoom pw) (. pic width))
                         px/height (* (paint-zoom pw) (. pic height)))))

      (setf (paint-data pw) data)
      (setf (paint-old-data pw) old-data)

      (labels ((update ()
                 (funcall (. ctx putImageData) data 0 0)))

        (set-handler pic oncontextmenu
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation)))

        (set-handler pic onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (copy-pixels (paint-old-data pw) (paint-data pw))
                     (let* ((p (event-pos event))
                            (p0 (element-pos pic))
                            (x (- (first p) (first p0)))
                            (y (- (second p) (second p0)))
                            (z (paint-zoom pw)))
                       (when ctool
                         (funcall ctool 'down (list (floor (/ x z)) (floor (/ y z))) (. event button))
                         (funcall ctool 'move (list (floor (/ x z)) (floor (/ y z))) (. event button))
                         (update)
                         (tracking (lambda (xx yy)
                                     (let ((x (- xx (first p0)))
                                           (y (- yy (second p0))))
                                       (funcall ctool 'move (list (floor (/ x z)) (floor (/ y z))) (. event button))
                                       (update)))
                                   (lambda (xx yy)
                                     (let ((x (- xx (first p0)))
                                           (y (- yy (second p0))))
                                       (funcall ctool 'up (list (floor (/ x z)) (floor (/ y z))) (. event button))
                                       (update)))
                                   "crosshair"))))

        (clear data (list 255 255 255 255))
        (update)
        (show-window frame)
        pw))))

(defun main ()
  (paint 100 100 640 480 "MyPaint"))

(main)