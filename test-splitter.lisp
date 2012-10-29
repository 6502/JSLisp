(import * from gui)
(import * from layout)
(import * from editor)

(defun browser (url)
  (let ((iframe (create-element "iframe")))
    (set-style iframe
               position "absolute"
               border "none"
               px/padding "0"
               px/margin "0")
    (setf iframe.src url)
    iframe))

(let** ((w (window 0 0 800 600 title: "Splitter test"))
        (a (browser "http://www.jslisp.org"))
        (b (editor "fibo.lisp" (replace (http-get "examples/fibo.lisp") "\r" "")))
        (c (editor "Test doc 2" "doc 2"))
        (s (add-widget w (gui:h-splitter a (gui:v-splitter b c)))))
  (set-layout w (V border: 8 (dom s)))
  (show-window w center: true))
