(defun main ()
  (let** ((canvas (create-element "canvas"))
          (ctx (canvas.getContext "2d"))
          (#'repaint ()
            (let** ((w canvas.offsetWidth)
                    (h canvas.offsetHeight)
                    (cx (/ w 2))
                    (cy (/ h 2))
                    (colors (list "#14FFA5"
                                  "#FF00FF"
                                  "#FF00FF"
                                  "#FF00FF"
                                  "#FF9921"
                                  "#FF9921"
                                  "#14FFA5"
                                  "#FF9921")))
              (setf canvas.width w)
              (setf canvas.height h)
              (setf ctx.fillStyle "#000000")
              (dotimes (y h)
                (dotimes (x w)
                  (let** ((dx (- x cx))
                          (dy (- y cy))
                          (a (atan2 dy dx))
                          (r (sqrt (+ (* dx dx) (* dy dy))))
                          (c1 (logand 1 (floor (+ (* 32 (log r))
                                                  (* a (/ 24 2 pi))))))
                          (c2 (logand 3 (floor (+ (* 4 (log r))
                                                  (* a (/ -12 2 pi)))))))
                    (setf ctx.fillStyle (aref colors (+ (* 4 c1) c2)))
                    (ctx.fillRect x y 1 1)))))))
    (setf canvas.style.width "1024px")
    (setf canvas.style.height "1024px")
    (setf document.body.innerHTML "")
    (append-child document.body canvas)
    (repaint)))

(main)