(import * from gui)
(import * from layout)
(import * from editor)
(import (mode) from editor-lispmode)
(import ilisp)

(defvar *ilisp*)

(defun src-tab (name content)
  (let ((editor (editor name content mode)))
    (set-style editor
               position "absolute")
    (setf editor.ilisp-exec
          (lambda ()
            (let ((lines (editor.lines))
                  ((row col s-row s-col) (editor.pos)))
              (declare (ignorable s-row s-col))
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
                    (*ilisp*.send "lisp" txt)))))))
    editor))

(defun inferior-lisp ()
  (let** ((container (set-style (create-element "div")
                                position "absolute"))
          (inspect (append-child container (button "Inspect" #'inspect)))
          (reset (append-child container (button "Reset" #'reset)))
          (clear (append-child container (button "Clear" #'clear)))
          (ilisp (ilisp:new #'reply))
          (#'inspect ()
                     (mode.inspect-ilisp ilisp))
          (#'reply (msg)
                   (when (= msg "\"ready\"")
                     (inspect))
                   (ilisp.send "javascript"
                               (+ "output(f$$str_value(f$$json_parse$42_("
                                  (json msg)
                                  "))+\"\\n\")")))
          (#'reset ()
                   (ilisp.reset))
          (#'clear ()
                   (ilisp.send "javascript"
                               "repl.value=\"\""))
          (layout (V border: 8 spacing: 8
                     (dom ilisp.iframe)
                     size: 30
                     (H :filler:
                        size: 80
                        (dom reset)
                        (dom clear)
                        (dom inspect)
                        :filler:))))
    (append-child container ilisp.iframe)
    (set-style ilisp.iframe
               position "absolute"
               border "solid 1px #CCCCCC"
               px/padding 0
               px/margin -1
               opacity 1)
    (setf container."data-resize"
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (setf container.ilisp ilisp)
    container))

(defun main ()
  (let** ((w (window 0 0 (* (screen-width) 0.75) (* (screen-height) 0.75)
                     title: "JsLisp IDE"))
          (sources (tabbed))
          (ilisp (inferior-lisp))
          (vs (add-widget w (v-splitter sources ilisp split: 70))))

    (setf *ilisp* ilisp.ilisp)

    (sources.add "test.lisp" (src-tab "test.lisp" ""))
    (sources.add "test2.lisp" (src-tab "test2.lisp" ""))
    (sources.add "test3.lisp" (src-tab "test3.lisp" ""))

    (document.body.addEventListener
     "keydown"
     (lambda (event)
       (let ((stop true))
         (cond
           ((and event.ctrlKey (= event.which 39))
            (sources.next)
            ((sources.current).focus))
           ((and event.ctrlKey (= event.which 37))
            (sources.prev)
            ((sources.current).focus))
           ((and event.ctrlKey (= event.which 13))
            ((sources.current).ilisp-exec))
           ((and event.ctrlKey (= event.which 73))
            (mode.inspect-ilisp *ilisp*))
           (true (setf stop false)))
         (when stop
           (event.stopPropagation)
           (event.preventDefault))))
     true)
    (set-layout w (V border: 8 spacing: 8
                     (dom vs)))
    (show-window w center: true)))

(main)