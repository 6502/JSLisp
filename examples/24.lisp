(import * from gui)
(import * from layout)

(defconstant R 30)
(defconstant R2 80)
(defconstant R3 200)

(defobject digit (x y n))
(defobject operator (x y op f entities a b))

(defmethod draw (ctx e) (digit? e)
  (ctx.beginPath)
  (ctx.moveTo (- e.x R) (- e.y R))
  (ctx.lineTo (+ e.x R) (- e.y R))
  (ctx.lineTo (+ e.x R) (+ e.y R))
  (ctx.lineTo (- e.x R) (+ e.y R))
  (ctx.closePath)
  (setf ctx.fillStyle "#EEE")
  (setf ctx.strokeStyle "#000")
  (setf ctx.lineWidth 2)
  (ctx.fill)
  (ctx.stroke)
  (setf ctx.fillStyle "#008")
  (setf ctx.font ~"bold {(* R 0.9)}px monospace")
  (setf ctx.textAlign "center")
  (ctx.fillText e.n e.x (+ e.y (* R 0.4))))

(defun xlate (e dx dy)
  (incf e.x dx)
  (incf e.y dy))

(defmethod hit (e x y) (digit? e)
  (when (and (< (abs (- e.x x)) R)
             (< (abs (- e.y y)) R))
    (lambda (xx yy)
      (xlate e (- xx x) (- yy y))
      (setf x xx)
      (setf y yy))))

(defmethod draw (ctx e) (operator? e)
  (ctx.beginPath)
  (ctx.arc e.x e.y R 0 (* 2 pi) true)
  (ctx.closePath)
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

(defun dist (a b)
  (sqrt (+ (expt (- a.x b.x) 2)
           (expt (- a.y b.y) 2))))

(defun above (ee side max-dist valid-entities)
  (let ((best null))
    (dolist (e valid-entities)
      (when (and (> (* side (- e.x ee.x)) 0)
                 (< e.y (- ee.y R)))
        (let ((dist (dist e ee)))
          (when (and (< dist max-dist)
                     (or (null? best)
                         (< dist (first best))))
            (setf best (list dist e))))))
    (and best (second best))))

(defun autoconnect (entities)
  (let ((valid-entities (filter #'digit? entities))
        (todo (filter #'operator? entities)))
    (dolist (ee todo)
      (setf ee.a undefined)
      (setf ee.b undefined))
    (do () ((not todo) valid-entities)
      (let ((best null))
        (dolist (ee todo)
          (let ((a (above ee -1 R3 valid-entities)))
            (when a
              (let ((b (above ee 1 R3 valid-entities)))
                (when b
                  (let ((score (+ (dist a ee) (dist b ee))))
                    (when (or (null? best)
                              (< score (first best)))
                      (setf best (list score ee a b)))))))))
        (if best
            (let (((score ee a b) best))
              (declare (ignorable score))
              (setf ee.a a)
              (setf ee.b b)
              (nremove ee todo)
              (nremove a valid-entities)
              (nremove b valid-entities)
              (push ee valid-entities))
            (return-from autoconnect valid-entities))))))

(defmethod hit (e x y) (operator? e)
  (when (and (< (abs (- e.x x)) R)
             (< (abs (- e.y y)) R))
    (when e.entities
      (let ((ee (new-operator e.x e.y e.op e.f null)))
        (push ee e.entities)
        (setf e ee)))
    (lambda (xx yy)
      (xlate e (- xx x) (- yy y))
      (setf x xx)
      (setf y yy))))

(defmethod value-of (e) (digit? e)
  (atoi e.n))

(defmethod value-of (e) (operator? e)
  (funcall e.f (value-of e.a) (value-of e.b)))

(defun game ()
  (let** ((digits (list 1 2 3 4))
          (target 10)
          (timerstart (clock))
          (elapsed 0)
          (score 0)
          (running false)
          (count-down 5)

          (container (append-child document.body
                                   (set-style (create-element "div")
                                              position "absolute"
                                              px/left 0
                                              px/top 0
                                              px/right 0
                                              px/bottom 0)))
          (#'restart ()
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
            (set-timeout #'restart 1000))
          (#'update-timer ()
            (when running
              (setf elapsed (- (clock) timerstart))
              (setf timerdiv.textContent ~"{(to-fixed (/ elapsed 1000) 3)}")))
          (#'repaint ()
            (set-style canvas
                       px/width container.offsetWidth
                       px/height container.offsetHeight)
            (let ((w canvas.offsetWidth)
                  (h canvas.offsetHeight)
                  (result (autoconnect entities)))
              (setf canvas.width w)
              (setf canvas.height h)

              (setf ctx.fillStyle "#EEE")
              (ctx.fillRect 0 0 w h)
              (setf ctx.fillStyle "#FFF")
              (setf ctx.font ~"bold {(* R2 3.5)}px monospace")
              (setf ctx.textAlign "center")
              (ctx.fillText (+ "" (or count-down target))
                            (* R2 3.5)
                            (* R2 (if count-down 5.5 6.5)))
              (unless count-down
                (setf ctx.strokeStyle "#888")
                (setf ctx.lineWidth 1)
                (ctx.beginPath)
                (ctx.moveTo (* R2 0.5) (* R2 1.5))
                (ctx.lineTo (* R2 6.5) (* R2 1.5))
                (ctx.lineTo (* R2 6.5) (* R2 7.5))
                (ctx.lineTo (* R2 0.5) (* R2 7.5))
                (ctx.closePath)
                (ctx.stroke)

                (dolist (e entities)
                  (draw-links ctx e))
                (dolist (e (reverse entities))
                  (draw ctx e))
                (when (= (length result) 1)
                  (let** ((x0 w)
                          (y0 h)
                          (x1 0)
                          (y1 0)
                          (#'visit (e)
                            (when (< e.x x0) (setf x0 e.x))
                            (when (> e.x x1) (setf x1 e.x))
                            (when (< e.y y0) (setf y0 e.y))
                            (when (> e.y y1) (setf y1 e.y))
                            (when e.a (visit e.a))
                            (when e.b (visit e.b))))
                    (visit (first result))
                    (setf ctx.strokeStyle
                          (if (< (abs (- (value-of (first result)) target))
                                 0.00001)
                              (progn
                                (when running
                                  (setf running false)
                                  (add-score (floor (/ (* 100 11000) (+ 1000 elapsed)))))
                                "#0F0")
                              "#F00"))
                    (setf ctx.lineWidth 6)
                    (ctx.beginPath)
                    (ctx.moveTo (- x0 R2) (- y0 R2))
                    (ctx.lineTo (+ x1 R2) (- y0 R2))
                    (ctx.lineTo (+ x1 R2) (+ y1 R2))
                    (ctx.lineTo (- x0 R2) (+ y1 R2))
                    (ctx.closePath)
                    (ctx.stroke))))))
          (#'down (x y)
            (let* (((x0 y0) (element-pos canvas))
                   (cb (any (e entities) (hit e (- x x0) (- y y0)))))
              (when cb
                (tracking
                  (lambda (x y)
                    (funcall cb (- x x0) (- y y0))
                    (repaint))
                  (lambda (x y)
                    (declare (ignorable x y))
                    (dolist (e (filter (lambda (e)
                                         (or (digit? e)
                                             e.entities
                                             (and (<= (* R2 0.5) e.x (* R2 6.5))
                                                  (<= (* R2 1.5) e.y (* R2 7.5)))))
                                       (splice entities)))
                      (push e entities))
                    (repaint))
                  "move")))))

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
      (apply #'down (event-pos event)))))

(defun main ()
  (game))

(main)
