(import * from gui)
(import * from rpc-client)
(import * from examples/pdfgrid)

(defun mm (x)
  (* x (/ 72  25.4)))

(defun parmform ()
  (let ((screen (create-element "div"))
        (width 500)
        (height 400))
    (set-style screen
               position "absolute"
               px/left 0
               px/top 0
               px/right 0
               px/bottom 0)
    (append-child document.body screen)
    (with-window (w ((/ (- screen.offsetWidth width) 2)
                     (/ (- screen.offsetHeight height) 2)
                     width height
                     title: "PDF grid")
                    ((page-width (input "Page width (mm)"))
                     (page-height (input "Page height (mm)"))
                     (rows (input "Number of rows"))
                     (cols (input "Number of columns"))
                     (left (input "Left margin (mm)"))
                     (top (input "Top margin (mm)"))
                     (x-step (input "Horizontal distance (mm)"))
                     (y-step (input "Vertical distance (mm)"))
                     (fontsize (input "Font size (mm)"))
                     (text (input "Text"))
                     (ok (button "OK"
                                 (lambda ()
                                   (let ((page-width (atof (text page-width)))
                                         (page-height (atof (text page-height)))
                                         (rows (atoi (text rows)))
                                         (cols (atoi (text cols)))
                                         (left (atof (text left)))
                                         (top (atof (text top)))
                                         (x-step (atof (text x-step)))
                                         (y-step (atof (text y-step)))
                                         (fontsize (atof (text fontsize)))
                                         (text (text text)))
                                     (unless (any (x (list page-width page-height
                                                           rows cols
                                                           left top
                                                           x-step y-step
                                                           fontsize))
                                                  (NaN? x))
                                       (let ((result
                                              (grid text (mm fontsize)
                                                    (mm page-width) (mm page-height)
                                                    rows cols
                                                    (mm left) (mm top)
                                                    (mm x-step) (mm y-step))))
                                         (setf document.location result)))))))
                     (cancel (button "Cancel" (lambda () (hide-window w)))))
                    (V: spacing: 16 border: 16
                        (H: size: 30
                            (Hdiv: page-width)
                            (Hdiv: page-height))
                        (H: size: 30
                            (Hdiv: rows)
                            (Hdiv: cols))
                        (H: size: 30
                            (Hdiv: left)
                            (Hdiv: top))
                        (H: size: 30
                            (Hdiv: x-step)
                            (Hdiv: y-step))
                        (H: size: 30
                            (Hdiv: fontsize)
                            (Hdiv: text))
                        (H:)
                        (H: size: 30
                            (H:)
                            (Hdiv: ok size: 80)
                            (Hdiv: cancel size: 80)
                            (H:))))
      (show-window w)
      (set-timeout (lambda () (page-width.lastChild.focus)) 100))))

(defun main ()
  (parmform))

(main)