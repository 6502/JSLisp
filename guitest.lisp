(import gui)

(let* ((w (window 100 100 600 400
                  :title "Message Box"))
       (msg (create-element "div"))
       (ok (button "OK" (lambda ()
                          (hide (window-frame w)))))
       (layout (:V :border 16 :spacing 16
                   (:Hdiv msg)
                   (:H :max 30 (:H) (:Hdiv ok :max 80) (:H)))))
  (append-child (window-frame w) msg)
  (append-child (window-frame w) ok)
  (set-style (window-client w)
             overflow "hidden")
  (set-style msg
             position "absolute"
             overflow "auto"
             fontSize "120%")
  (setf (. msg textContent)
        "Lorem ipsum dolor sit amet, consectetur adipiscing elit. Fusce
         interdum tortor aliquam nibh tempus faucibus. Fusce libero risus,
         luctus et mattis vel, volutpat sit amet risus. Ut id tellus sit amet
         odio volutpat adipiscing. Mauris neque est, fermentum quis viverra
         vitae, commodo egestas risus. Morbi congue interdum nibh auctor
         pharetra. Etiam non nunc vel ipsum fringilla cursus. Lorem ipsum dolor
         sit amet, consectetur adipiscing elit. Donec dictum aliquet lorem,
         pharetra gravida leo aliquet ac. Donec leo eros, scelerisque quis
         pellentesque vel, euismod eget metus. Nam augue nisl, ullamcorper in
         varius vel, tempus vel erat.")
  (setf (window-resize-cback w)
        (lambda (x0 y0 x1 y1)
          (set-coords layout x0 y0 x1 y1)))
  (show-window w))

