(import * from gui)
(import * from layout)

(defun simple-graph ()
  (let** ((w (window 0 0 0.75 0.75 title: "Simple graph"))
          (data (add-widget w (text-area "data")))
          (ok (add-widget w (button "OK" #'ok)))
          (#'ok ()
            (let** ((w (window 0 0 0.75 0.75 title: "Graph"))
                    (values (filter (lambda (x) (not (NaN? x)))
                                    (map #'atof (split (text data) "\n"))))
                    (canvas (add-widget w (create-element "canvas")))
                    (ctx (canvas.getContext "2d"))
                    (#'repaint ()
                      (let** ((min (apply #'min values))
                              (max (apply #'max values))
                              (delta (- max min))
                              (w canvas.offsetWidth)
                              (h canvas.offsetHeight))
                        (decf min (/ delta 10))
                        (incf max (/ delta 10))
                        (setf delta (- max min))
                        (setf canvas.width w)
                        (setf canvas.height h)
                        (ctx.beginPath)
                        (enumerate (x y values)
                          (let ((xx (* (+ 0.5 x) w (/ (length values))))
                                (yy (* (- max y) h (/ delta))))
                            (if (= x 0)
                                (ctx.moveTo xx yy)
                                (ctx.lineTo xx yy))))
                        (setf ctx.lineWidth 2)
                        (setf ctx.strokeStyle "#F00")
                        (ctx.stroke)
                        (setf ctx.lineWidth 1)
                        (setf ctx.strokeStyle "#CCC")
                        (setf ctx.fillStyle "#000")
                        (let ((step 1))
                          (do () ((< (* 10 step) delta))
                            (setf step (* step 0.5)))
                          (do () ((> (* 10 step) delta))
                            (setf step (* step 2)))
                          (dolist (iy (range (1- (floor (/ min step)))
                                             (+ 2 (floor (/ max step)))))
                            (let* ((y (* iy step))
                                   (yy (* (- max y) h (/ delta))))
                              (ctx.beginPath)
                              (ctx.moveTo 0 yy)
                              (ctx.lineTo w yy)
                              (ctx.stroke)
                              (ctx.fillText (json y) 0 yy)))))))
              (set-layout w (V border: 8 spacing: 8
                               (dom canvas)))
              (setf canvas.data-resize #'repaint)
              (show-window w center: true))))
    (set-layout w (V border: 8 spacing: 8
                     (dom data)
                     size: 30
                     (H :filler: size: 80 (dom ok) :filler:)))
    (focus data)
    (show-window w center: true)))

(defun main ()
  (simple-graph))

(main)
