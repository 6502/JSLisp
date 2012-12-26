(import * from gui)
(import * from layout)

(defun browser ()
  (let** ((w (window 0 0 0.75 0.75 title: "Browser"))
          (address (add-widget w (input "address" autofocus: true)))
          (go (add-widget w (button "go" #'go default: true)))
          (page (add-widget w (create-element "iframe")))
          (#'go ()
            (setf page.src (text address))))
    (set-layout w (V border: 8 spacing: 8
                     size: 40
                     (H (dom address)
                        size: 60
                        (V :filler:
                           size: 25
                           (dom go)))
                     size: undefined
                     (dom page)))
    (show-window w center: true)))

(defun main ()
  (browser))

(main)