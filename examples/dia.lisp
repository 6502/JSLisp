(import * from gui)
(import * from layout)

(defobject node
           ((text "<new node>")
            (fields (list))
            (color "#FFFFFF")
            (x 100)
            (y 100)
            (width 80)
            (height 30)
            (outgoing (list))
            (incoming (list))
            (list (error "Container is required"))))

(defobject link ((from (error "From table is required"))
                 (to (error "Target table is required"))
                 (ym 0.5)
                 (list from.list)))

(defun link (from to &optional (ym 0.5))
  (let ((L (new-link from to ym)))
    (push L from.outgoing)
    (push L to.incoming)
    L))

(defun draw (e ctx editor)
  (declare (ignorable e ctx editor)))

(defun hit (e x y editor)
  (declare (ignorable e x y editor)))

(defun hit2 (e x y editor)
  (declare (ignorable e x y editor)))

(defmethod destroy (e) (node? e)
  (do () ((= 0 (length e.incoming)))
    (destroy (first e.incoming)))
  (do () ((= 0 (length e.outgoing)))
    (destroy (first e.outgoing)))
  (nremove e e.list))

(defmethod draw (e ctx editor) (node? e)
  (declare (ignorable editor))
  (setf ctx.fillStyle e.color)
  (setf ctx.strokeStyle "#000000")
  (setf ctx.lineWidth 1)
  (ctx.beginPath)
  (ctx.moveTo (- e.x 0.5) (- e.y 0.5))
  (ctx.lineTo (+ e.x e.width 0.5) (- e.y 0.5))
  (ctx.lineTo (+ e.x e.width 0.5) (+ e.y e.height 0.5))
  (ctx.lineTo (- e.x 0.5) (+ e.y e.height 0.5))
  (ctx.closePath)
  (ctx.fill)
  (ctx.stroke)
  (setf ctx.font "bold 16px Arial")
  (setf ctx.fillStyle "#000000")
  (setf ctx.textBaseline "top")
  (let ((w (ctx.measureText e.text).width))
    (ctx.fillText e.text
                  (+ e.x (/ (- e.width w) 2))
                  (+ e.y (/ (- e.height 16) 2)))))

(defmethod hit (e x y editor) (node? e)
  (declare (ignorable editor))
  (when (and (<= e.x x (+ e.x e.width))
             (<= e.y y (+ e.y e.height)))
    (lambda (xx yy)
      (incf e.x (- xx x))
      (incf e.y (- yy y))
      (setf x xx)
      (setf y yy))))

(defmethod edit (e editor) (node? e)
  (let** ((w (window 0 0 400 300 title: "Table properties"))
          (text (add-widget w (input "text")))
          (color (add-widget w (css-color-input "preferred color")))
          (fields (add-widget w (text-area "fields")))
          (ok (add-widget w (button "OK" #'ok)))
          (cancel (add-widget w (button "Cancel" #'cancel)))
          (delete (add-widget w (button "Delete" #'delete)))
          (#'ok ()
            (setf e.fields (filter (lambda (f) (/= f ""))
                                   (split (text fields) "\n")))
            (setf e.color (text color))
            (setf e.text (text text))
            (editor.repaint)
            (hide-window w))
          (#'cancel ()
            (hide-window w))
          (#'delete ()
            (destroy e)
            (editor.repaint)
            (hide-window w)))
    (set-layout w (V border: 8 spacing: 8
                     size: 40
                     (H (dom text) size: 110 (dom color))
                     size: undefined
                     (dom fields)
                     size: 30
                     (H :filler: size: 80 (dom ok) (dom cancel) (dom delete) :filler:)))
    (setf (text text) e.text)
    (setf (text color) e.color)
    (setf (text fields) (join e.fields "\n"))
    (show-window w center: true)
    (focus text)))

(defmethod hit2 (e x y editor) (node? e)
  (when (and (<= e.x x (+ e.x e.width))
             (<= e.y y (+ e.y e.height)))
    (lambda (xx yy phase)
      (when (= phase 'up)
        (let ((e2 (any (e2 e.list)
                    (and (<= e2.x xx (+ e2.x e2.width))
                         (<= e2.y yy (+ e2.y e2.height))
                         e2))))
          (cond
            ((= e e2) (edit e editor))
            (e2 (push (link e e2) e.list))))))))

(defmethod destroy (e) (link? e)
  (nremove e e.from.outgoing)
  (nremove e e.to.incoming)
  (nremove e e.list))

(defun link-geometry (e)
  (let* ((y0 (+ e.from.y e.from.height))
         (y1 e.to.y)
         (x0 (floor (+ 0.5 e.from.x (* e.from.width (/ (1+ (index e e.from.outgoing))
                                                       (1+ (length e.from.outgoing)))))))
         (x1 (floor (+ 0.5 e.to.x (* e.to.width (/ (1+ (index e e.to.incoming))
                                                   (1+ (length e.to.incoming)))))))
         (ym (floor (+ 0.5 y0 (* e.ym (- y1 y0))))))
    (list (list x0 y0)
          (list x0 ym)
          (list x1 ym)
          (list x1 y1))))

(defmethod draw (e ctx editor) (link? e)
  (declare (ignorable editor))
  (setf ctx.strokeStyle "#000000")
  (setf ctx.lineWidth 2)
  (ctx.beginPath)
  (enumerate (i (x y) (link-geometry e))
    (if (= i 0)
        (ctx.moveTo x y)
        (ctx.lineTo x y)))
  (ctx.stroke))

(defmethod hit (e x y editor) (link? e)
  (declare (ignorable editor))
  (let ((((x0 y0) (x1 y1)) (slice (link-geometry e) 1 3)))
    (declare (ignorable y1))
    (when (and (or (<= x1 x x0) (<= x0 x x1))
               (<= (- y0 5) y (+ y0 5)))
      (lambda (xx yy)
        (declare (ignorable xx))
        (setf e.ym (/ (- yy (+ e.from.y e.from.height))
                      (- e.to.y e.from.y e.from.height)))))))

(defun main ()
  (let** ((editor (set-style (create-element "div")
                             position "absolute"
                             px/left 0
                             px/top 0
                             px/right 0
                             px/bottom 0))
          (canvas (append-child editor (set-style (create-element "canvas")
                                                  position "absolute"
                                                  px/left 0
                                                  px/top 0)))
          (entities (list))
          (#'repaint ()
            (let ((w editor.offsetWidth)
                  (h editor.offsetHeight)
                  (ctx (canvas.getContext "2d")))
              (set-style canvas
                         px/width w
                         px/height h)
              (setf canvas.width w)
              (setf canvas.height h)
              (setf ctx.fillStyle "#CCCCCC")
              (ctx.fillRect 0 0 w h)
              (dolist (e entities)
                (draw e ctx editor))))
          (#'mousedown (event hitf)
            (event.stopPropagation)
            (event.preventDefault)
            (let* (((x y) (relative-pos event canvas))
                   (handler (any (e (reverse entities)) (funcall hitf e x y editor))))
              (if handler
                  (progn
                    (funcall handler x y 'down)
                    (tracking (lambda (xx yy)
                                (let (((x0 y0) (element-pos canvas)))
                                  (decf xx x0)
                                  (decf yy y0)
                                  (funcall handler xx yy 'move)
                                  (repaint)))
                              (lambda (xx yy)
                                (let (((x0 y0) (element-pos canvas)))
                                  (decf xx x0)
                                  (decf yy y0)
                                  (funcall handler xx yy 'up)
                                  (repaint)))))
                  (when (= hitf #'hit2)
                    (push (make-node x: (- x 60)
                                     y: (- y 15)
                                     width: 120
                                     height: 30
                                     list: entities)
                          entities)
                    (repaint))))))
    (set-interval (lambda ()
                    (when (or (/= canvas.width editor.offsetWidth)
                              (/= canvas.height editor.offsetHeight))
                      (repaint)))
                  10)
    (setf canvas.onmousedown
          (lambda (event)
            (when (= event.button 0)
              (mousedown event #'hit))))
    (setf canvas.oncontextmenu
          (lambda (event)
            (mousedown event #'hit2)))
    (append-child document.body editor)
    (setf editor.repaint #'repaint)
    (repaint)))

(main)