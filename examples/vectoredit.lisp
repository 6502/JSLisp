(import * from graphics)
(import * from gui)

;; A bidimensional point
(defobject p2d (x y))
(defun p2d (x y) (new-p2d x y))

(defun dist (a b)
  (let ((dx (- b.x a.x))
        (dy (- b.y a.y)))
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
  (p2d (+ a.x (* t (- b.x a.x)))
       (+ a.y (* t (- b.y a.y)))))

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
          (n (length obj.pts)))
      (dolist (t (fp-range 0 (1- n) (* 10 n)))
        (let ((p (pt obj.pts t)))
          (if first
              (move-to p.x p.y)
              (line-to p.x p.y))
          (setf first false))))
    (stroke-style obj.color)
    (line-width obj.width)
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
    (begin-path)
    (let ((first true)
          (n (length obj.entity.pts)))
      (dolist (p obj.entity.pts)
        (if first
            (move-to p.x p.y)
            (line-to p.x p.y))
        (setf first false))
      (line-width 1)
      (stroke-style "#C0C0C0")
      (stroke))
    (let ((last null))
      (stroke-style "#FF0000")
      (line-width 2)
      (dolist (p obj.entity.pts)
        (begin-path)
        (arc p.x p.y 4 0 (* 2 pi) true)
        (close-path)
        (stroke)
        (when last
          (let ((mp (lerp p last 0.5)))
            (begin-path)
            (arc mp.x mp.y 2 0 (* 2 pi) true)
            (close-path)
            (stroke)))
        (setf last p)))))

(defmethod hit (shape obj p btn) (line-editor? obj)
  (let ((best null)
        (best-dist 16))
    (dolist (pp obj.entity.pts)
      (let ((dist (dist p pp)))
        (when (< dist best-dist)
          (setf best-dist dist)
          (setf best pp))))
    (if best
        ;; Hit: edit or delete
        (if (= btn 2)
            (progn
              (if (= (length obj.entity.pts) 2)
                  (progn
                    (nremove-first obj.entity shape.objs)
                    (nremove-first obj shape.objs))
                  (nremove-first best obj.entity.pts))
              #'wait-up)
            (lambda (phase p)
              (setf best.x p.x)
              (setf best.y p.y)))
        ;; Miss: check for point addition
        (progn
          (dotimes (i (1- (length obj.entity.pts)))
            (let* ((pp (lerp (aref obj.entity.pts i) (aref obj.entity.pts (1+ i)) 0.5))
                   (dist (dist p pp)))
              (when (< dist best-dist)
                (setf best-dist dist)
                (setf best (list pp (1+ i))))))
          (if best
              (progn
                ;; Add point
                (insert obj.entity.pts (second best) (first best))
                (lambda (phase p)
                  (setf (first best).x p.x)
                  (setf (first best).y p.y)))
              (progn
                ;; Miss: remove interactor
                (nremove-first obj shape.objs)
                #'wait-up))))))

; Click on line activates an editor
(defmethod hit (shape obj p btn) (line? obj)
  (let ((n (length obj.pts)))
    (dolist (t (fp-range 0 (1- n) (* 10 n)))
      (let ((pp (pt obj.pts t)))
        (when (< (dist p pp) 16)
          (push (new-line-editor shape obj) shape.objs)
          (return-from hit #'wait-up))))))

; Line creation interactor
(defobject line-drawing (shape (pts (list))))

(defmethod prompt (obj) (line-drawing? obj)
  (if (length obj.pts)
      "[1]=add another point, [2]=restart"
      "[1]=add first point, [2]=quit"))

(defmethod hit (shape obj p btn) (line-drawing? obj)
  (if (= btn 2)
      (progn
        (if (length obj.pts)
            (progn
              (setf obj.pts (list))
              (nremove-first obj shape.objs)
              (push obj shape.objs))
            (nremove-first obj shape.objs))
        #'wait-up)
      (progn
        (push p obj.pts)
        (when (= (length obj.pts) 2)
          (push (new-line obj.pts 3.0 "#000000") shape.objs))
        (lambda (phase pp)
          (setf p.x pp.x)
          (setf p.y pp.y)))))

(defmethod draw (canvas obj) (line-drawing? obj)
  (when (= (length obj.pts) 1)
    (with-canvas canvas
      (let ((p (first obj.pts)))
        (begin-path)
        (move-to (- p.x 3) (- p.y 3))
        (line-to (+ p.x 3) (+ p.y 3))
        (move-to (- p.x 3) (+ p.y 3))
        (line-to (+ p.x 3) (- p.y 3))
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
                 (dolist (x shape.objs)
                   (draw canvas x)
                   (setf msg (or (prompt x) msg)))
                 (setf (. prompt textContent) msg)))
             (set-interactor (i)
               (setf shape.objs
                     (filter (lambda (x) (not (prompt x))) shape.objs))
               (when i (push i shape.objs))
               (repaint)))
      (let* ((clear (button "Clear"
                            (lambda ()
                              (setf shape.objs (list))
                              (repaint))))
             (draw-line (button "+line"
                                (lambda ()
                                  (set-interactor (new-line-drawing shape)))))
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
                         (dolist (x (reverse shape.objs))
                           (unless interactor
                             (setf interactor (hit shape x pp btn))))
                         (when interactor
                           (funcall interactor 0 pp))))
                     (repaint))))
    (show-window w)))

(defun main ()
  (editor (new-shape (list))))

(main)