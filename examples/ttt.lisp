(import * from layout)
(import * from gui)

(defun ttt ()
  (let** ((w (window 0 0 400 400 title: "T-T-T"))
          (color 0)
          (#'cell()
            (let ((div (set-style (create-element "div")
                                  position "absolute"
                                  textAlign "center"
                                  backgroundColor "#EEEEEE")))
              (setf div."data-resize"
                    (lambda (x0 y0 x1 y1)
                      (declare (ignorable x0 x1))
                      (setf div.style.fontSize
                            (+ (* (- y1 y0) 0.9) "px"))))
              (set-handler div onclick
                (when (= div.textContent "")
                  (setf div.textContent (aref "OX" color))
                  (setf color (- 1 color))))
              (add-widget w div))))
    (set-layout w (V border: 16 spacing: 8
                     (H (dom (cell)) (dom (cell)) (dom (cell)))
                     (H (dom (cell)) (dom (cell)) (dom (cell)))
                     (H (dom (cell)) (dom (cell)) (dom (cell)))))
    (show-window w center: true)))

(defun main ()
  (ttt))

(main)
