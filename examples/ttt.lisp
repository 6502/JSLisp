(import * from layout)
(import * from gui)

(defun ttt ()
  (let** ((w (window 0 0 400 400 title: "T-T-T"))
          (board (map (lambda (i)
                        (map (lambda (j)
                               (cell i j))
                             (range 3)))
                      (range 3)))
          (color 0)
          (markers "OX")
          (grid (add-widget w (table board)))
          (#'cell (i j)
            (declare (ignorable i j))
            (let ((div (create-element "div")))
              (set-style div
                         position "absolute"
                         textAlign "center"
                         backgroundColor "#FFFFFF")
              (setf div."data-resize"
                    (lambda (x0 y0 x1 y1)
                      (declare (ignorable x0 x1))
                      (setf div.style.fontSize
                            (+ (* (- y1 y0) 0.9) "px"))))
              (setf div.onclick
                    (lambda (event)
                      (declare (ignorable event))
                      (when (= div.textContent "")
                        (setf div.textContent
                              (aref markers color))
                        (setf color (- 1 color)))))
              div)))
    (set-layout w (V border: 16 spacing: 8 (dom grid)))
    (show-window w center: true)
    w))

(defun main ()
  (ttt))

(main)
