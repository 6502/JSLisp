(import * from gui)
(import * from layout)
(import * from editor)
(import (mode) from editor-lispmode)
(import ilisp)

(defvar *ilisp*)

(defun src-window (name content ilisp)
  (let** ((w (window 0 0
                     (floor (* (screen-width) 0.60))
                     (- (screen-height) 4)
                     title: name))
          (editor (add-widget w (editor name content mode))))
    (setf (layout w) (V border: 8 (dom editor)))
    (setf mode.exec
          (lambda (lines row col)
            (let ((m (mode.parmatch lines row col)))
              (when m
                (let (((row0 col0) m)
                      (txt ""))
                  (if (= row row0)
                      (incf txt (slice (aref lines row).text col0 (- col col0)))
                      (progn
                        (incf txt (+ (slice (aref lines row0).text col0) "\n"))
                        (dolist (r (range (1+ row0) row))
                          (incf txt (+ (aref lines r).text "\n")))
                        (incf txt (slice (aref lines row).text 0 col))))
                  (ilisp.send "lisp" txt))))))
    (show-window w)))

(defun inferior-lisp ()
  (let** ((w (window (+ 4 (floor (* (screen-width) 0.60))) 0
                     (- (floor (* (screen-width) 0.40)) 8)
                     (- (screen-height) 4)
                     title: "Inferior Lisp"))
          (reset (add-widget w (button "Reset" #'reset)))
          (clear (add-widget w (button "Clear" #'clear)))
          (ilisp (ilisp:new #'reply))
          (#'reply (msg)
                   (when (= msg "ready")
                     (mode.inspect-ilisp ilisp))
                   (unless (list? msg)
                     (setf msg (list msg)))
                   (dolist (x msg)
                     (ilisp.send "javascript"
                                 (+ "repl.value+=f$$str_value(f$$json_parse$42_("
                                    (json (json* x))
                                    "))+\"\\n\""))))
          (#'reset ()
                   (ilisp.reset))
          (#'clear ()
                   (ilisp.send "javascript"
                               "repl.value=\"\"")))
    (add-widget w ilisp.iframe)
    (set-style ilisp.iframe opacity 1)
    (setf (layout w) (V border: 16 spacing: 16
                        (dom ilisp.iframe)
                        size: 30
                        (H :filler: size: 80 (dom reset) (dom clear) :filler:)))

    (show-window w)
    ilisp))

(defun main ()
  (let ((ilisp (inferior-lisp)))
    (src-window "test.lisp"
                ""
                ilisp)))

(main)