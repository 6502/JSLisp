(import * from gui)
(import * from layout)

(defun prime? (x)
  (cond
    ((= x 2) true)
    ((= x 3) true)
    ((< x 2) false)
    ((= 0 (% x 2)) false)
    (true
      (do ((q 3 (+ q 2)))
        ((or (= 0 (% x q))
             (> (* q q) x))
         (> (* q q) x))))))

(defun primegraph ()
  (let** ((w (window 0 0 0.75 0.75 title: "Prime graph"))
          (canvas (add-widget w (create-element "canvas")))
          (#'resize ()
            (setf canvas.width canvas.offsetWidth)
            (setf canvas.height canvas.offsetHeight)
            (repaint))
          (#'repaint ()
            (let** ((ctx (canvas.getContext "2d"))
                    (w canvas.width)
                    (h canvas.height)
                    (cx (ash w -1))
                    (cy (ash h -1))
                    (i 1)
                    (#'line (n dx dy)
                      (repeat n
                        (when (and (< -1 cy h)
                                   (< -1 cx w)
                                   (prime? i))
                          (ctx.fillRect cx cy 1 1))
                        (incf i)
                        (incf cx dx)
                        (incf cy dy))))
              (setf ctx.fillStyle "#000")
              (ctx.fillRect 0 0 w h)
              (setf ctx.fillStyle "#FFF")
              (dolist (s (range 1 (max w h) 2))
                (line s 1 0)
                (line s 0 1)
                (line (1+ s) -1 0)
                (line (1+ s) 0 -1)))))
    (set-layout w (V border: 8 spacing: 8
                     (dom canvas)))
    (setf canvas.data-resize #'resize)
    (show-window w center: true)))

(defun main ()
  (primegraph))

(main)