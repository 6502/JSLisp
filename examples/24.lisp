(setf *deploy-prefix*
      (replace *deploy-prefix* "</head>"
               "<meta name=\"viewport\" content=\"user-scalable=no, width=device-width, initial-scale=1.0, maximum-scale=1.0\"/>\
                <meta name=\"apple-mobile-web-app-capable\" content=\"yes\" />\
                <meta name=\"apple-mobile-web-app-status-bar-style\" content=\"black\" />\
                </head>"))

(import * from gui)
(import * from layout)

(defvar R 30)
(defvar R2 80)

(defobject digit (x y n xx yy))
(defobject operator (x y op f entities a b xx yy))

(defmethod draw (ctx e selected) (digit? e)
  (ctx.beginPath)
  (ctx.moveTo (- e.x R) (- e.y R))
  (ctx.lineTo (+ e.x R) (- e.y R))
  (ctx.lineTo (+ e.x R) (+ e.y R))
  (ctx.lineTo (- e.x R) (+ e.y R))
  (ctx.closePath)
  (when (find e selected)
    (setf ctx.strokeStyle "#F00")
    (setf ctx.lineWidth 8)
    (ctx.stroke))
  (setf ctx.fillStyle "#EEE")
  (setf ctx.strokeStyle "#000")
  (setf ctx.lineWidth 2)
  (ctx.fill)
  (ctx.stroke)
  (setf ctx.fillStyle "#008")
  (setf ctx.font ~"bold {(* R 0.9)}px monospace")
  (setf ctx.textAlign "center")
  (ctx.fillText e.n e.x (+ e.y (* R 0.4))))

(defmethod draw (ctx e selected) (operator? e)
  (ctx.beginPath)
  (ctx.arc e.x e.y R 0 (* 2 pi) true)
  (ctx.closePath)
  (when (find e selected)
    (setf ctx.strokeStyle "#F00")
    (setf ctx.lineWidth 8)
    (ctx.stroke))
  (setf ctx.fillStyle (if e.entities "#CFC" "#FFC"))
  (setf ctx.strokeStyle "#000")
  (setf ctx.lineWidth 2)
  (ctx.fill)
  (ctx.stroke)
  (setf ctx.fillStyle "#800")
  (setf ctx.font ~"bold {(* R 0.9)}px monospace")
  (setf ctx.textAlign "center")
  (ctx.fillText e.op e.x (+ e.y (* R 0.4))))

(defun draw-links (ctx e)
  (declare (ignorable ctx e)))

(defmethod draw-links (ctx e) (operator? e)
  (setf ctx.strokeStyle "#00F")
  (setf ctx.lineWidth 4)
  (ctx.beginPath)
  (when e.a
    (ctx.moveTo e.x e.y)
    (ctx.lineTo e.a.x e.a.y))
  (when e.b
    (ctx.moveTo e.x e.y)
    (ctx.lineTo e.b.x e.b.y))
  (ctx.stroke))

(defmethod node-height (e) (digit? e)
  (declare (ignorable e))
  R2)

(defmethod node-height (e) (operator? e)
  (+ R2
     (max (if e.a (node-height e.a) 0)
          (if e.b (node-height e.b) 0))))

(defmethod node-width (e) (digit? e)
  (declare (ignorable e))
  R2)

(defmethod node-width (e) (operator? e)
  (+ (if e.a (node-width e.a) 0)
     (if e.b (node-width e.b) 0)))

(defmethod set-pos (x0 y0 x1 y1 e) (digit? e)
  (setf e.xx (/ (+ x0 x1) 2))
  (setf e.yy y1))

(defmethod set-pos (x0 y0 x1 y1 e) (operator? e)
  (setf e.xx (/ (+ x0 x1) 2))
  (setf e.yy y1)
  (when e.a
    (set-pos x0 y0 (+ x0 (node-width e.a)) (- y1 R2) e.a))
  (when e.b
    (set-pos (+ x0 (if e.a (node-width e.a) 0)) y0 x1 (- y1 R2) e.b)))

(defun place (entities x0 x1 y)
  (let** ((free (slice entities))
          (#'visit (x)
            (when (operator? x)
              (when x.a
                (nremove x.a free)
                (visit x.a))
              (when x.b
                (nremove x.b free)
                (visit x.b)))))
    (dolist (x entities)
      (visit x))
    (let ((w (map #'node-width free))
          (h (map #'node-height free)))
      (incf x0 (/ (- x1 x0 (reduce #'+ w)) 2))
      (incf y (apply #'max h))
      (dolist ((e nw nh) (zip free w h))
        (set-pos x0 (- y nh) (+ x0 nw) y e)
        (incf x0 nw)))
    free))

(defmethod value-of (e) (digit? e)
  (atoi e.n))

(defmethod value-of (e) (operator? e)
  (funcall e.f (value-of e.a) (value-of e.b)))

(defun game ()
  (set-style document.body
             overflow "hidden")
  (let** ((digits (list 1 2 3 4))
          (target 10)
          (timerstart (clock))
          (elapsed 0)
          (score 0)
          (running false)
          (count-down 5)
          (rounds 5)
          (ith 0)
          (selected (list))
          (moving true)

          (container (append-child document.body
                                   (set-style (create-element "div")
                                              position "absolute"
                                              px/left 0
                                              px/top 0
                                              px/right 0
                                              px/bottom 0)))
          (#'restart ()
            (incf ith)
            (setf digits (repeat-collect 4 (random-choice (range 1 10))))
            (setf target (do ((t 0))
                           ((<= 1 t 99) t)
                           (setf t (do ((r (slice digits)))
                                     ((= (length r) 1) (first r))
                                     (let** ((a (random-choice r))
                                             (b (random-choice (remove-first a r)))
                                             (x (funcall (random-choice (list #'+ #'- #'* #'/)) a b)))
                                       (when (< (abs (- x (round x))) 0.0001)
                                         (nremove-first a r)
                                         (nremove-first b r)
                                         (push x r)))))))
            (let ((i 0))
              (dolist (e (splice entities))
                (when (digit? e)
                  (setf e.n (aref digits i))
                  (setf e.x (* (+ i 2) R2))
                  (setf e.y (* R2 2))
                  (incf i))
                (when (or (digit? e) e.entities)
                  (push e entities))))
            (setf count-down 5)
            (setf timerstart (clock))
            (repaint))
          (canvas (append-child container
                                (set-style (create-element "canvas")
                                           position "absolute"
                                           px/left 0
                                           px/top 0)))
          (ctx (canvas.getContext "2d"))
          (entities (list))
          (timerdiv (append-child container
                                  (set-style (create-element "div")
                                             position "absolute"
                                             px/right 0
                                             px/bottom 0
                                             px/fontSize R
                                             fontWeight "bold"
                                             fontFamily "monospace"
                                             color "#888")))
          (scorediv (append-child container
                                  (set-style (create-element "div")
                                             position "absolute"
                                             px/bottom 0
                                             px/left 0
                                             px/fontSize R
                                             fontWeight "bold"
                                             fontFamily "monospace"
                                             color "#008")))
          (#'add-score (x)
            (incf score x)
            (setf scorediv.textContent ~"Score: {score}")
            (setf timerdiv.textContent "")
            (set-style scorediv
                       display "none")
            (set-timeout #'restart 2000))

          (#'update-timer ()
            (when running
              (setf elapsed (- (clock) timerstart))
              (setf timerdiv.textContent ~"({ith}/{rounds}) : {(to-fixed (/ elapsed 1000) 3)}")))

          (#'free ()
            (place (filter (lambda (e)
                             (or (digit? e)
                                 (not e.entities)))
                           entities)
                   R2 (* 6 R2) R2))

          (#'animate ()
            (setf moving false)
            (dolist (e entities)
              (let** ((dx (- (or e.xx e.x) e.x))
                      (dy (- (or e.yy e.y) e.y))
                      (d2 (+ (* dx dx) (* dy dy))))
                (if (< d2 1)
                    (progn
                      (setf e.x (or e.xx e.x))
                      (setf e.y (or e.yy e.y)))
                    (progn
                      (incf e.x (* dx 0.1))
                      (incf e.y (* dy 0.1))
                      (setf moving true)))))
            (when moving
              (set-timeout #'repaint 10)))

          (#'repaint ()
            (set-style canvas
                       px/width container.offsetWidth
                       px/height container.offsetHeight)
            (setf R2 (/ canvas.offsetWidth 7))
            (setf R (* R2 0.45))
            (set-style scorediv px/fontSize R)
            (set-style timerdiv px/fontSize R)
            (let ((i 0))
              (dolist (e entities)
                (when (and (operator? e) e.entities)
                  (setf e.x (* (+ i 2) R2))
                  (setf e.y R2)
                  (incf i))))
            (let ((w canvas.offsetWidth)
                  (h canvas.offsetHeight)
                  (result (free)))
              (when moving (animate))
              (setf canvas.width w)
              (setf canvas.height h)
              (setf ctx.font ~"bold {(* R2 3.5)}px monospace")
              (setf ctx.textAlign "center")
              (setf ctx.fillStyle "#EEE")
              (ctx.fillRect 0 0 w h)

              (if count-down
                  (progn
                    (setf ctx.fillStyle "#888")
                    (ctx.fillText count-down (* R2 3.5) (* R2 5.5)))
                  (progn
                    (setf ctx.strokeStyle "#888")
                    (setf ctx.lineWidth 1)
                    (ctx.beginPath)
                    (ctx.moveTo (* R2 0.5) (* R2 1.5))
                    (ctx.lineTo (* R2 6.5) (* R2 1.5))
                    (ctx.lineTo (* R2 6.5) (* R2 7.5))
                    (ctx.lineTo (* R2 0.5) (* R2 7.5))
                    (ctx.closePath)
                    (when (= (length result) 1)
                      (setf ctx.fillStyle
                            (if (< (abs (- (value-of (first result)) target))
                                   0.00001)
                                (progn
                                  (when running
                                    (setf running false)
                                    (add-score (floor (/ (* 100 11000) (+ 1000 elapsed)))))
                                  "#0F0")
                                "#F00"))
                      (ctx.fill))
                    (ctx.stroke)
                    (setf ctx.fillStyle "#FFF")
                    (ctx.fillText target (* R2 3.5) (* R2 6.5))
                    (dolist (e entities)
                      (draw-links ctx e))
                    (dolist (e (reverse entities))
                      (draw ctx e selected))))))

          (#'down (x y)
            (let* (((x0 y0) (element-pos canvas))
                   (free (free)))
              (decf x x0)
              (decf y y0)
              (dolist (e entities)
                (when (and (< (abs (- x e.x)) R)
                           (< (abs (- y e.y)) R))
                  (cond
                    ((find e selected)
                     (nremove e selected))
                    ((find e free)
                     (when (< (length (filter (lambda (e) (find e free)) selected)) 2)
                       (push e selected)))
                    ((and (operator? e) e.entities)
                     (setf selected (filter (lambda (e) (not e.entities)) selected))
                     (push e selected)))
                  (when (= (length selected) 3)
                    (let ((op (first (filter (lambda (e) e.entities) selected))))
                      (nremove op selected)
                      (let ((a (first selected))
                            (b (second selected)))
                        (push (new-operator R2 R2 op.op op.f null a b) entities)))
                    (splice selected))
                  (setf moving true)
                  (repaint)
                  (return-from down))))
            (if (length selected)
                (setf selected (list))
                (dolist (e (filter (lambda (e)
                                     (or (digit? e)
                                         e.entities))
                                   (splice entities)))
                  (push e entities)))
            (setf moving true)
            (repaint)))

    (push (new-operator (* R2 2) R2 "+" #'+ entities) entities)
    (push (new-operator (* R2 3) R2 "-" #'- entities) entities)
    (push (new-operator (* R2 4) R2 "*" #'* entities) entities)
    (push (new-operator (* R2 5) R2 "/" #'/ entities) entities)
    (dolist ((x n) (zip (list (* R2 2) (* R2 3) (* R2 4) (* R2 5)) digits))
      (push (new-digit x (* R2 2) n) entities))

    (set-interval (lambda ()
                    (update-timer)
                    (when (or (/= canvas.width container.offsetWidth)
                              (/= canvas.height container.offsetHeight)
                              (and count-down (> (clock) (+ 1000 timerstart))))
                      (when count-down
                        (decf count-down)
                        (setf timerstart (clock))
                        (unless count-down
                          (set-style scorediv
                                     display "block")
                          (setf running true)))
                      (repaint)))
                  53)
    (set-handler canvas onmousedown
      (event.preventDefault)
      (event.stopPropagation)
      (apply #'down (event-pos event)))

    (set-handler canvas ontouchstart
      (event.preventDefault)
      (event.stopPropagation)
      (let (((bx by) (element-pos canvas))
            (ex (first event.touches).pageX)
            (ey (first event.touches).pageY))
        (down (- ex bx) (- ey by))))

    (restart)))

(defun main ()
  (game))

(main)
