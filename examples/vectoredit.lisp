(import * from layout)
(import * from gui)
(import * from xy)

(setf #'warning #'error)

;; a graphic world

(defobject view ((editors (list))
                 (canvas null)
                 (ctx null)
                 (zero (xy 0 0))
                 (scale 1.0)))

(defobject editor ((objects (list))
                   (mousedown-listeners (list))
                   (mousemove-listeners (list))
                   (mouseup-listeners (list))
                   (mousewheel-listeners (list))))

(defvar *view* null)
(defvar *editor* null)

(defun xform (p)
  (xy+ *view*.zero (xy* p *view*.scale)))

(defun rev-xform (p)
  (xy/ (xy- p *view*.zero) *view*.scale))

;; drawing primitives

(defun move-to (p)
  (setf p (xform p))
  (*view*.ctx.moveTo p.x p.y))

(defun line-to (p)
  (setf p (xform p))
  (*view*.ctx.lineTo p.x p.y))

(defun bez2-to (c p)
  (setf c (xform c))
  (setf p (xform p))
  (*view*.ctx.quadraticCurveTo c.x c.y p.x p.y))

(defun bez3-to (c1 c2 p)
  (setf c1 (xform c1))
  (setf c2 (xform c2))
  (setf p (xform p))
  (*view*.ctx.bezierCurveTo c1.x c1.y c2.x c2.y p.x p.y))

(defun polyline (pts)
  (when (and (= (length pts) 1)
             (list? (first pts)))
    (setf pts (first pts)))
  (when (> (length pts) 0)
    (move-to (first pts))
    (dotimes (i (1- (length pts)))
      (line-to (aref pts (1+ i))))))

(defun dot (center pxradius)
  (setf center (xform center))
  (*view*.ctx.arc center.x center.y pxradius 0 (* 2 pi) false))

(defun stroke (color width)
  (setf *view*.ctx.strokeStyle color)
  (setf *view*.ctx.lineWidth
        (if (< width 0)
            (- width)
            (* width *view*.scale)))
  (*view*.ctx.stroke))

(defun fill (color)
  (setf *view*.ctx.fillStyle color)
  (*view*.ctx.fill))

;;

(defun draw (obj)
  (declare (ignorable obj)))

(defun hit (obj p)
  (declare (ignorable obj p)))

(defun view (editors)
  (let** ((w (window 0 0 640 480 title: "Vector editor"))
          (canvas (add-widget w (create-element "canvas")))
          (ctx (canvas.getContext "2d"))
          (view (make-view editors: editors
                           canvas: canvas
                           ctx: ctx))
          (dirty false)

          (#'update ()
            (unless dirty
              (setf dirty true)
              (set-timeout #'redraw 0)))

          (#'redraw ()
            (let ((*view* view))
              (setf dirty false)
              (setf canvas.width canvas.offsetWidth)
              (setf canvas.height canvas.offsetHeight)
              (dolist (*editor* editors)
                (setf ctx.fillStyle "rgba(0,0,0,0.15)")
                (ctx.fillRect 0 0 canvas.width canvas.height)
                (dolist (o *editor*.objects)
                  (ctx.beginPath)
                  (draw o)))))

          (#'mousedown (event)
            (let ((*view* view))
              (event.preventDefault)
              (event.stopPropagation)
              (when (> (length editors) 0)
                (let ((p (rev-xform (apply #'xy (relative-pos event canvas))))
                      (*editor* (last editors)))
                  (dolist (ml (reverse *editor*.mousedown-listeners))
                    (when (funcall ml p)
                      (update)
                      (return-from mousedown)))
                  (dolist (o (reverse *editor*.objects))
                    (when (hit o p)
                      (update)
                      (return-from mousedown)))))))

          (#'mousemove (event)
            (let ((*view* view))
              (event.preventDefault)
              (event.stopPropagation)
              (when (> (length editors) 0)
                (let ((p (rev-xform (apply #'xy (relative-pos event canvas))))
                      (*editor* (last editors)))
                  (dolist (ml (reverse *editor*.mousemove-listeners))
                    (when (funcall ml p)
                      (update)
                      (return-from mousemove)))))))

          (#'mouseup (event)
            (let ((*view* view))
              (event.preventDefault)
              (event.stopPropagation)
              (when (> (length editors) 0)
                (let ((p (rev-xform (apply #'xy (relative-pos event canvas))))
                      (*editor* (last editors)))
                  (dolist (ml (reverse *editor*.mouseup-listeners))
                    (when (funcall ml p)
                      (update)
                      (return-from mouseup)))))))

          (#'mousewheel (event)
            (let* ((*view* view)
                   (p (rev-xform (apply #'xy (relative-pos event canvas)))))
              (event.preventDefault)
              (event.stopPropagation)
              (when (> (length editors) 0)
                (let ((*editor* (last editors)))
                  (dolist (ml (reverse *editor*.mousewheel-listeners))
                    (when (funcall ml p event.wheelDeltaY)
                      (update)
                      (return-from mousewheel)))))
              (let ((k (/ event.wheelDeltaY 120))
                    (oldscale view.scale))
                (setf view.scale (* view.scale (exp (* (log 1.1) k))))
                (setf view.scale (max (min view.scale 1000) 0.001))
                (setf view.zero (xy+ view.zero (xy* p (- oldscale view.scale))))
                (update)))))

    (setf canvas.onmousedown #'mousedown)
    (setf canvas.onmousemove #'mousemove)
    (setf canvas.onmouseup #'mouseup)
    (setf canvas.onmousewheel #'mousewheel)

    (setf canvas."data-resize" #'redraw)
    (set-layout w (V border: 8 (dom canvas)))
    (show-window w center: true)))

;;

(defmacro track (var &rest body)
  `(let** ((#'move (,@var)
             ,@body)
           (#'up (,@var)
             (declare (ignorable ,@var))
             (nremove #'move *editor*.mousemove-listeners)
             (nremove #'up *editor*.mouseup-listeners)))
     (push #'move *editor*.mousemove-listeners)
     (push #'up *editor*.mouseup-listeners)))

(defobject keypoint (pt cback))

(defmethod draw (obj) (keypoint? obj)
  (dot obj.pt 4)
  (fill "#FF0000"))

(defmethod hit (obj p) (keypoint? obj)
  (when (< (xy-dist obj.pt p) 8)
    (track (p)
      (setf obj.pt p)
      (funcall obj.cback p))
    true))

(defobject rel-keypoint (base delta cback))

(defmethod draw (obj) (rel-keypoint? obj)
  (dot (xy+ obj.base.pt obj.delta) 3)
  (fill "#FF00FF"))

(defmethod hit (obj p) (rel-keypoint? obj)
  (when (< (xy-dist (xy+ obj.base.pt obj.delta) p) 8)
    (track (p)
      (setf obj.delta (xy- p obj.base.pt))
      (funcall obj.cback obj.delta))
    true))

(defobject construction (f))

(defmethod draw (obj) (construction? obj)
  (funcall obj.f))

(defobject editor-closer ())

(defmethod hit (obj p) (editor-closer? obj)
  (declare (ignorable p))
  (pop *view*.editors))

(defun sub-editor (objects)
  (push (new-editor (append (new-editor-closer) objects))
        *view*.editors))

(defun edit-rect (x0 y0 x1 y1 f)
  (let** ((xm (/ (+ x0 x1) 2))
          (ym (/ (+ y0 y1) 2))
          (kp1 (make-keypoint pt: (xy x0 y0)
                              cback: (lambda (p)
                                       (setf x0 (min p.x (- x1 (/ 10 *view*.scale))))
                                       (setf y0 (min p.y (- y1 (/ 10 *view*.scale))))
                                       (fix))))
          (kp2 (make-keypoint pt: (xy xm y0)
                              cback: (lambda (p)
                                       (setf y0 (min p.y (- y1 (/ 10 *view*.scale))))
                                       (fix))))
          (kp3 (make-keypoint pt: (xy x1 y0)
                              cback: (lambda (p)
                                       (setf x1 (max p.x (+ x0 (/ 10 *view*.scale))))
                                       (setf y0 (min p.y (- y1 (/ 10 *view*.scale))))
                                       (fix))))
          (kp4 (make-keypoint pt: (xy x0 ym)
                              cback: (lambda (p)
                                       (setf x0 (min p.x (- x1 (/ 10 *view*.scale))))
                                       (fix))))
          (kp5 (make-keypoint pt: (xy x1 ym)
                              cback: (lambda (p)
                                       (setf x1 (max p.x (+ x0 (/ 10 *view*.scale))))
                                       (fix))))
          (kp6 (make-keypoint pt: (xy x0 y1)
                              cback: (lambda (p)
                                       (setf x0 (min p.x (- x1 (/ 10 *view*.scale))))
                                       (setf y1 (max p.y (+ y0 (/ 10 *view*.scale))))
                                       (fix))))
          (kp7 (make-keypoint pt: (xy xm y1)
                              cback: (lambda (p)
                                       (setf y1 (max p.y (+ y0 (/ 10 *view*.scale))))
                                       (fix))))
          (kp8 (make-keypoint pt: (xy x1 y1)
                              cback: (lambda (p)
                                       (setf x1 (max p.x (+ x0 (/ 10 *view*.scale))))
                                       (setf y1 (max p.y (+ y0 (/ 10 *view*.scale))))
                                       (fix))))
          (kp9 (make-keypoint pt: (xy xm ym)
                              cback: (lambda (p)
                                       (let ((dx (- p.x xm))
                                             (dy (- p.y ym)))
                                         (incf x0 dx) (incf x1 dx)
                                         (incf y0 dy) (incf y1 dy)
                                         (fix)))))
          (cc (new-construction (lambda ()
                                  (move-to (xy x0 y0))
                                  (line-to (xy x1 y0))
                                  (line-to (xy x1 y1))
                                  (line-to (xy x0 y1))
                                  (line-to (xy x0 y0))
                                  (stroke "#FFFFFF" -1))))
          (#'fix ()
                 (funcall f x0 y0 x1 y1)
                 (setf xm (/ (+ x0 x1) 2))
                 (setf ym (/ (+ y0 y1) 2))
                 (setf kp1.pt (xy x0 y0))
                 (setf kp2.pt (xy xm y0))
                 (setf kp3.pt (xy x1 y0))
                 (setf kp4.pt (xy x0 ym))
                 (setf kp5.pt (xy x1 ym))
                 (setf kp6.pt (xy x0 y1))
                 (setf kp7.pt (xy xm y1))
                 (setf kp8.pt (xy x1 y1))
                 (setf kp9.pt (xy xm ym))))
    (sub-editor (list cc kp1 kp2 kp3 kp4 kp5 kp6 kp7 kp8 kp9))))

(defobject circle (center radius fill-color stroke-color stroke-width))

(defmethod draw (obj) (circle? obj)
  (let ((c (xform obj.center))
        (r (* obj.radius *view*.scale)))
    (*view*.ctx.arc c.x c.y r 0 (* 2 pi) true)
    (when obj.fill-color (fill obj.fill-color))
    (when obj.stroke-color (stroke obj.stroke-color obj.stroke-width))))

(defmethod hit (obj p) (circle? obj)
  (when (< (xy-dist p obj.center) obj.radius)
    (let** ((kp1 (make-keypoint pt: obj.center
                                cback: (lambda (p)
                                         (setf obj.center p))))
            (kp2 (make-rel-keypoint base: kp1
                                    delta: (xy obj.radius 0)
                                    cback: (lambda (delta)
                                             (setf obj.radius (xy-abs delta)))))
            (cc (new-construction (lambda ()
                                    (move-to kp1.pt) (line-to (xy+ kp1.pt kp2.delta))
                                    (stroke "#FFFFFF" -1)))))
      (sub-editor (list cc kp1 kp2)))))

(defobject bez3 (p1 p2 p3 p4 stroke-color stroke-width))

(defmethod draw (obj) (bez3? obj)
  (move-to obj.p1)
  (bez3-to obj.p2 obj.p3 obj.p4)
  (stroke obj.stroke-color obj.stroke-width))

(defmethod hit (obj p) (bez3? obj)
  (labels ((bez3geo (p1 p2 p3 p4)
             (let ((L (+ (xy-dist p1 p2) (xy-dist p2 p3) (xy-dist p3 p4))))
               (if (< L (/ 8 *view*.scale))
                   (list p1 p4)
                   (let* ((p12 (xy-avg p1 p2))
                          (p23 (xy-avg p2 p3))
                          (p34 (xy-avg p3 p4))
                          (p123 (xy-avg p12 p23))
                          (p234 (xy-avg p23 p34))
                          (p1234 (xy-avg p123 p234)))
                     (append (bez3geo p1 p12 p123 p1234)
                             (slice (bez3geo p1234 p234 p34 p4) 1)))))))
    (when (< (apply #'min (map (lambda (px) (xy-dist p px))
                               (bez3geo obj.p1 obj.p2 obj.p3 obj.p4)))
             (/ 8 *view*.scale))
      (let** ((kp1 (make-keypoint pt: obj.p1
                                  cback: (lambda (p)
                                           (setf obj.p1 p)
                                           (setf obj.p2 (xy+ p kp2.delta)))))
              (kp4 (make-keypoint pt: obj.p4
                                  cback: (lambda (p)
                                           (setf obj.p4 p)
                                           (setf obj.p3 (xy+ p kp3.delta)))))
              (kp2 (make-rel-keypoint base: kp1
                                      delta: (xy- obj.p2 obj.p1)
                                      cback: (lambda (delta)
                                               (setf obj.p2 (xy+ obj.p1 delta)))))
              (kp3 (make-rel-keypoint base: kp4
                                      delta: (xy- obj.p3 obj.p4)
                                      cback: (lambda (delta)
                                               (setf obj.p3 (xy+ obj.p4 delta)))))
              (cc (new-construction (lambda ()
                                      (move-to kp1.pt) (line-to (xy+ kp1.pt kp2.delta))
                                      (move-to kp4.pt) (line-to (xy+ kp4.pt kp3.delta))
                                      (stroke "#FFFFFF" -1)))))
        (sub-editor (list cc kp1 kp2 kp3 kp4))))))

(defobject image (img p1 p2))

(defmethod draw (obj) (image? obj)
  (let (((x0 y0) (xform obj.p1))
        ((x1 y1) (xform obj.p2)))
    (*view*.ctx.drawImage obj.img x0 y0 (- x1 x0) (- y1 y0))))

(defmethod hit (obj p) (image? obj)
  (when (and (<= obj.p1.x p.x obj.p2.x)
             (<= obj.p1.y p.y obj.p2.y))
    (edit-rect obj.p1.x obj.p1.y obj.p2.x obj.p2.y
               (lambda (x0 y0 x1 y1)
                 (setf obj.p1 (xy x0 y0))
                 (setf obj.p2 (xy x1 y1))))))

(defobject rect (p1 p2 fill-color stroke-color stroke-width))

(defmethod draw (obj) (rect? obj)
  (move-to obj.p1)
  (line-to (xy obj.p2.x obj.p1.y))
  (line-to obj.p2)
  (line-to (xy obj.p1.x obj.p2.y))
  (line-to obj.p1)
  (when obj.fill-color (fill obj.fill-color))
  (when obj.stroke-color (stroke obj.stroke-color obj.stroke-width)))

(defmethod hit (obj p) (rect? obj)
  (when (and (<= obj.p1.x p.x obj.p2.x)
             (<= obj.p1.y p.y obj.p2.y))
    (edit-rect obj.p1.x obj.p1.y obj.p2.x obj.p2.y
               (lambda (x0 y0 x1 y1)
                 (setf obj.p1 (xy x0 y0))
                 (setf obj.p2 (xy x1 y1))))))

(defobject polygon (pts fill-color stroke-color stroke-width))

(defmethod draw (obj) (polygon? obj)
  (move-to (first obj.pts))
  (dotimes (i (1- (length obj.pts)))
    (line-to (aref obj.pts (1+ i))))
  (*view*.ctx.closePath)
  (when obj.fill-color (fill obj.fill-color))
  (when obj.stroke-color (stroke obj.stroke-color obj.stroke-width)))

(defmethod hit (obj p) (polygon? obj)
  (when (or (and obj.fill-color
                 (xy-inside p obj.pts))
            (< (apply #'min (map (lambda (x) (xy-dist p x)) obj.pts))
               (/ 8 *view*.scale)))
    (let** ((kpts (map (lambda (i)
                         (make-keypoint pt: (aref obj.pts i)
                                        cback: (lambda (p)
                                                 (setf (aref obj.pts i) p)
                                                 (setf (aref kpts (length obj.pts)).pt
                                                       (apply #'xy-avg obj.pts)))))
                       (range (length obj.pts)))))
      (push (make-keypoint pt: (apply #'xy-avg obj.pts)
                           cback: (lambda (p)
                                    (let ((delta (xy- p (apply #'xy-avg obj.pts))))
                                      (enumerate (i x obj.pts)
                                        (setf (aref obj.pts i)
                                              (xy+ x delta))
                                        (setf (aref kpts i).pt
                                              (aref obj.pts i)))
                                      true)))
            kpts)
      (sub-editor kpts))))

(defun main ()
  (let ((e (new-editor)))
    (push (make-circle center: (xy 50 50)
                       radius: 30
                       fill-color: "#FFFF00"
                       stroke-color: "#008000"
                       stroke-width: -4)
          e.objects)

    (push (make-circle center: (xy 150 50)
                       radius: 40
                       fill-color: "#FF0000"
                       stroke-color: "#000000"
                       stroke-width: -2)
          e.objects)

    (push (make-bez3 p1: (xy 200 100)
                     p2: (xy 300 100)
                     p3: (xy 300 200)
                     p4: (xy 200 200)
                     stroke-color: "#000080"
                     stroke-width: -5)
          e.objects)

    (let ((img (create-element "img")))
      (setf img.src "examples/img/wq.png")
      (push (make-image img: img
                        p1: (xy 100 200)
                        p2: (xy 150 250))
            e.objects))

    (push (make-rect p1: (xy 200 200)
                     p2: (xy 250 280)
                     fill-color: "#CCCCFF"
                     stroke-color: "#0000C0"
                     stroke-width: -4)
          e.objects)

    (push (make-polygon pts: (map (lambda (i)
                                    (xy+ (xy 300 200)
                                         (xy-from-polar 50 (* i 2 pi (/ 8)))))
                                  (range 8))
                        fill-color: "#FF0080"
                        stroke-color: "#FFFFFF"
                        stroke-width: 3)
          e.objects)

    (view (list e))))

(main)