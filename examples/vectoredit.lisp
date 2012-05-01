(import * from graphics)
(import * from gui)
(import * from objects)

;; A bidimensional point
(defobject p2d (x y))

(defun dist (a b)
  (let ((dx (- (x b) (x a)))
        (dy (- (y b) (y a))))
    (sqrt (+ (* dx dx) (* dy dy)))))

;; A line built with quadratic curves
(defobject line (pts
                 width
                 color))

;; Part of a line
(defobject boundary (L t0 t1))

;; A closed area
(defobject area (boundaries
                 color))

;; A vector shape
(defobject shape (objs))

(defun lerp (a b t)
  "Interpolated point between [a] and [b]"
  (p2d (+ (p2d-x a) (* t (- (p2d-x b) (p2d-x a))))
       (+ (p2d-y a) (* t (- (p2d-y b) (p2d-y a))))))

(defun avg (a b)
  "Middle point between [a] and [b]"
  (lerp a b 0.5))

(defun pt (pts t)
  "Returns a point on a curve"
  ;;
  ;;       |  arc-1  |  arc-2  |  arc-3  |         |  last   |
  ;;  x----|----x----|----x----|----x----|- - - - -|----x----|----x
  ;;  0    .5   1    .5   2    .5   3    .5            n-2   .5  n-1
  ;;
  (let ((n (length pts)))
    (macrolet ((p (ix) `(aref pts ,ix)))
      (cond
        ((< t 0.5)
         (lerp (p 0) (avg (p 0) (p 1)) (* t 2)))
        ((> t (- n 1.5))
         (lerp (avg (p (- n 2)) (p (1- n))) (p (1- n))
               (* 2 (- t (- n 1.5)))))
        (true
         (let* ((i (floor (+ t 0.5)))
                (s (+ 0.5 (- t i)))
                (p0 (avg (p (1- i)) (p i)))
                (p1 (p i))
                (p2 (avg (p i) (p (1+ i))))
                (p01 (lerp p0 p1 s))
                (p12 (lerp p1 p2 s)))
           (lerp p01 p12 s)))))))

(defun draw (canvas obj)
  "Draw [obj] on the provided [canvas]")

(defmethod draw (canvas obj) (line? obj)
  (with-canvas canvas
    (begin-path)
    (let ((first true)
          (n (length (line-pts obj))))
      (dolist (t (fp-range 0 (1- n) (* 10 n)))
        (let ((p (pt (line-pts obj) t)))
          (if first
              (move-to (p2d-x p) (p2d-y p))
              (line-to (p2d-x p) (p2d-y p)))
          (setf first false))))
    (stroke-style (line-color obj))
    (line-width (width obj))
    (stroke)))

;; Interactor methods
(defun hit (shape obj p btn)
  "The [hit] method should return either a callable handling drag/up or a false value"
  false)

(defun prompt (obj)
  "The top-most 'true' prompt message gets displayed in the status"
  false)

;; Utility mouse callback... just waits mouseup
(defun wait-up (phase p))

;; Line-edit interactor
(defobject line-editor (shape entity))

(defmethod prompt (obj) (line-editor? obj)
  "[1]=edit position or add point, [2]=delete point")

(defmethod draw (canvas obj) (line-editor? obj)
  (with-canvas canvas
    (with-line (line-editor-entity obj)
      (begin-path)
      (let ((first true)
            (n (length pts)))
        (dolist (p pts)
          (if first
              (move-to (p2d-x p) (p2d-y p))
              (line-to (p2d-x p) (p2d-y p)))
          (setf first false))
        (line-width 1)
        (stroke-style "#C0C0C0")
        (stroke))
      (let ((last null))
        (stroke-style "#FF0000")
        (line-width 2)
        (dolist (p pts)
          (begin-path)
          (arc (p2d-x p) (p2d-y p) 4 0 (* 2 pi) true)
          (close-path)
          (stroke)
          (when last
            (let ((mp (lerp p last 0.5)))
              (begin-path)
              (arc (p2d-x mp) (p2d-y mp) 2 0 (* 2 pi) true)
              (close-path)
              (stroke)))
          (setf last p))))))

(defmethod hit (shape obj p btn) (line-editor? obj)
  (with-line (line-editor-entity obj)
    (let ((best null)
          (best-dist 16))
      (dolist (pp pts)
        (let ((dist (dist p pp)))
          (when (< dist best-dist)
            (setf best-dist dist)
            (setf best pp))))
      (if best
          ;; Hit: edit or delete
          (if (= btn 2)
              (progn
                (if (= (length pts) 2)
                    (progn
                      (nremove-first (line-editor-entity obj) (objs shape))
                      (nremove-first obj (objs shape)))
                    (nremove-first best pts))
                #'wait-up)
              (lambda (phase p)
                (setf (p2d-x best) (p2d-x p))
                (setf (p2d-y best) (p2d-y p))))
          ;; Miss: check for point addition
          (progn
            (dotimes (i (1- (length pts)))
              (let* ((pp (lerp (aref pts i) (aref pts (1+ i)) 0.5))
                     (dist (dist p pp)))
                (when (< dist best-dist)
                  (setf best-dist dist)
                  (setf best (list pp (1+ i))))))
            (if best
                (progn
                  ;; Add point
                  (insert pts (second best) (first best))
                  (lambda (phase p)
                    (setf (x (first best)) (x p))
                    (setf (y (first best)) (y p))))
                (progn
                  ;; Miss: remove interactor
                  (nremove-first obj (objs shape))
                  #'wait-up)))))))

; Click on line activates an editor
(defmethod hit (shape obj p btn) (line? obj)
  (let ((n (length (line-pts obj))))
    (dolist (t (fp-range 0 (1- n) (* 10 n)))
      (let ((pp (pt (line-pts obj) t)))
        (when (< (dist p pp) 16)
          (push (line-editor shape obj)
                (objs shape))
          (return-from hit #'wait-up))))))

; Line creation interactor
(defobject line-drawing (shape (pts (list))))

(defmethod prompt (obj) (line-drawing? obj)
  (if (length (line-drawing-pts obj))
      "[1]=add another point, [2]=restart"
      "[1]=add first point, [2]=quit"))

(defmethod hit (shape obj p btn) (line-drawing? obj)
  (if (= btn 2)
      (progn
        (if (length (line-drawing-pts obj))
            (progn
              (setf (line-drawing-pts obj) (list))
              (nremove-first obj (objs shape))
              (push obj (objs shape)))
            (nremove-first obj (objs shape)))
        #'wait-up)
      (progn
        (push p (line-drawing-pts obj))
        (when (> (length (line-drawing-pts obj)) 1)
          (push (line (line-drawing-pts obj) 3.0 "#000000") (objs shape)))
        (lambda (phase pp)
          (setf (p2d-x p) (p2d-x pp))
          (setf (p2d-y p) (p2d-y pp))))))

(defmethod draw (canvas obj) (line-drawing? obj)
  (when (= (length (line-drawing-pts obj)) 1)
    (with-canvas canvas
      (with-p2d (first (line-drawing-pts obj))
        (begin-path)
        (move-to (- x 3) (- y 3))
        (line-to (+ x 3) (+ y 3))
        (move-to (- x 3) (+ y 3))
        (line-to (+ x 3) (- y 3))
        (stroke-style "#000000")
        (line-width 1)
        (stroke)))))

(defun editor (shape)
  (let* ((w (window 100 100 600 400
                    :title "vector editor"))
         (canvas (create-element "canvas"))
         (prompt (create-element "div"))
         (interactor null))
    (set-style prompt
               position "absolute"
               px/fontSize 16
               fontWeight "bold"
               backgroundColor "#CCCCCC")
    (set-style canvas
               position "absolute")
    (labels ((repaint ()
               (setf (. canvas width) (. canvas offsetWidth))
               (setf (. canvas height) (. canvas offsetHeight))
               (with-canvas canvas
                 (fill-style "#EEEEEE")
                 (rect 0 0 (. canvas width) (. canvas height))
                 (fill))
               (let ((msg "Select a command..."))
                 (dolist (x (objs shape))
                   (draw canvas x)
                   (setf msg (or (prompt x) msg)))
                 (setf (. prompt textContent) msg)))
             (set-interactor (i)
               (setf (objs shape)
                     (filter (lambda (x) (not (prompt x))) (objs shape)))
               (when i (push i (objs shape)))
               (repaint)))
      (let* ((clear (button "Clear"
                            (lambda ()
                              (setf (objs shape) (list))
                              (repaint))))
             (draw-line (button "+line"
                                (lambda ()
                                  (set-interactor (line-drawing shape)))))
             (layout (:H :spacing 4 :border 4
                         (:V :spacing 4 :size 60
                             (:V (:Vdiv draw-line :size 30)
                                 (:Vdiv clear :size 30)
                                 (:V)))
                         (:V :spacing 4 (:Hdiv canvas)
                             (:Hdiv prompt :size 20)))))

        (set-style (window-client w)
                   overflow "hidden")

        (dolist (x (list canvas prompt
                         clear draw-line))
          (append-child (window-client w) x))

        (setf (window-resize-cback w)
              (lambda (x0 y0 x1 y1)
                (set-coords layout 0 1 (- x1 0) (- y1 y0 -1))
                (repaint)))

        (set-handler canvas oncontextmenu
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation)))

        (set-handler canvas onmousemove
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (when interactor
                       (let ((p (event-pos event))
                             (p0 (element-pos canvas)))
                         (funcall interactor 1 (p2d (- (first p) (first p0))
                                                    (- (second p) (second p0))))
                         (repaint))))

        (set-handler canvas onmouseup
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (when interactor
                       (let ((p (event-pos event))
                             (p0 (element-pos canvas)))
                         (funcall interactor 2 (p2d (- (first p) (first p0))
                                                    (- (second p) (second p0)))))
                       (setf interactor null))
                     (repaint))

        (set-handler canvas onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (let ((p (event-pos event))
                           (p0 (element-pos canvas)))
                       (let ((pp (p2d (- (first p) (first p0))
                                      (- (second p) (second p0))))
                             (btn (if (= (. event button) 2) 2 1)))
                         (dolist (x (reverse (objs shape)))
                           (unless interactor
                             (setf interactor (hit shape x pp btn))))
                         (when interactor
                           (funcall interactor 0 pp))))
                     (repaint))))
    (show-window w)))

(defun main ()
  (editor (shape (list))))

(main)