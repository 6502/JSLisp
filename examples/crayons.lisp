(import * from gui)
(import * from layout)

(defun crayons ()
  (let** ((w (window 0 0 0.75 0.75 title: "Crayons"))
          (area (add-widget w (create-element "div")))
          (canvas (append-child area (set-style (create-element "canvas")
                                                position "absolute"
                                                px/left 0
                                                px/top 0)))
          (ctx (canvas.getContext "2d"))
          (img null)
          (sketch (add-widget w (input "sketch url" autofocus: true)))
          (loadbtn (add-widget w (button "Load" #'load-sketch)))
          (#'load-sketch ()
            (when img
              (remove-child area img))
            (setf img (create-element "img"))
            (set-handler img onload
              (setf canvas.width img.width)
              (setf canvas.height img.height))
            (set-style img
                       position "absolute"
                       px/left 0
                       px/top 0)
            (append-child area img)
            (setf img.src (text sketch)))
          (current-color "#808080")
          (current-crayon null)
          (current-radius 5)
          (prad (list 5 10 15 18))
          (tools (let ((res (add-widget w (create-element "div"))))
                   (dotimes (i 8)
                     (let ((c (create-element "div"))
                           (r (if (logand i 1) 255 128))
                           (g (if (logand i 2) 255 128))
                           (b (if (logand i 4) 255 128)))
                       (set-style c
                                  position "absolute"
                                  backgroundColor ~"rgb({r},{g},{b})"
                                  cursor "pointer"
                                  boxShadow "1px 1px 1px 1px rgba(0,0,0,0.5)"
                                  px/borderRadius 8
                                  px/left (* i 48)
                                  px/top 0
                                  px/width 40
                                  px/height 40)
                       (set-handler c onmousedown
                         (let* ((pat (create-element "canvas"))
                                (cp (pat.getContext "2d")))
                           (setf pat.width 100)
                           (setf pat.height 100)
                           (setf cp.fillStyle ~"rgba({r},{g},{b},0.15)")
                           (dolist (ix (range 0 100 10))
                             (dolist (iy (range 0 100 10))
                               (let ((x (+ ix (* (random) 6) -3))
                                     (y (+ iy (* (random) 6) -3)))
                                 (dolist (xx (list (- x 100) x (+ x 100)))
                                   (dolist (yy (list (- y 100) y (+ y 100)))
                                     (cp.beginPath)
                                     (cp.arc xx yy 12 0 (* 2 pi) true)
                                     (cp.fill)))))
                             (setf current-color (ctx.createPattern pat "repeat"))))
                         (when current-crayon
                           (set-style current-crayon
                                      border "none"
                                      px/left (1+ current-crayon.offsetLeft)
                                      px/top 0))
                         (setf current-crayon c)
                         (set-style c
                                    border "solid 2px #000"
                                    px/left (1- c.offsetLeft)
                                    px/top -1))
                       (append-child res c)))
                   (dotimes (i 4)
                     (let* ((p (set-style (create-element "canvas")
                                          position "absolute"
                                          border "solid 1px #000"
                                          px/borderRadius 8
                                          boxShadow "1px 1px 1px 1px rgba(0,0,0,0.5)"
                                          px/left (* 48 (+ i 8))
                                          px/top 0
                                          px/width 40
                                          px/height 40))
                            (cp (p.getContext "2d")))
                       (setf p.width 40)
                       (setf p.height 40)
                       (setf cp.fillStyle "#000")
                       (cp.beginPath)
                       (cp.arc 20 20 (aref prad i) 0 (* 2 pi) true)
                       (cp.fill)
                       (set-handler p onmousedown
                         (setf current-radius (aref prad i)))
                       (append-child res p)))
                   res)))
    (set-layout w (V spacing: 8 border: 8
                     (dom area)
                     size: 40
                     (H size: (* 12 48)
                        (dom tools)
                        size: undefined
                        (dom sketch)
                        size: 80
                        (dom loadbtn))))
    (set-handler w.frame onmousedown
      (event.preventDefault)
      (event.stopPropagation))
    (set-handler area onmousedown
      (let (((cx cy) (relative-pos event area)))
        (tracking (lambda (x y)
                    (ctx.beginPath)
                    (setf ctx.strokeStyle current-color)
                    (setf ctx.lineWidth (* current-radius 2))
                    (setf ctx.lineCap "round")
                    (ctx.moveTo cx cy)
                    (ctx.lineTo x y)
                    (ctx.stroke)
                    (setf cx x)
                    (setf cy y))
                  undefined
                  undefined
                  (element-pos area))))
    (show-window w center: true)))

(defun main ()
  (crayons))

(main)