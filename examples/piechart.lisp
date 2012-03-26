(import gui)

(defun wedge-center (x y r start-angle end-angle)
  "Circular sector barycenter"
  (let* ((angle (- end-angle start-angle))
         (r1 (/ (* 4 r (sin (/ angle 2)))
                (* 3 angle)))
         (aa (/ (+ start-angle end-angle) 2)))
    (list (+ x (* r1 (cos aa)))
          (+ y (* r1 (sin aa))))))

(defun pie-chart (canvas x y r data)
  "Draws a pie chart using [data] on [canvas] centered in ([x],[y]) with radius [r]"
  (let ((total (apply #'+ (map #'second data)))
        (start-angle 0)
        (i 0))
    (with-canvas canvas
      (dolist (item data)
        (let* ((label (first item))
               (value (second item))
               (end-angle (+ start-angle
                             (* 2 pi value (/ total))))
               (avg (/ (+ start-angle end-angle) 2))
               (cx (+ x (* (cos avg) (/ r 10))))
               (cy (+ y (* (sin avg) (/ r 10)))))
          (fill-style (let ((r (+ 128 (* 64 (logand i 1))))
                            (g (+ 128 (* 64 (logand (ash i -1) 1))))
                            (b (+ 128 (* 64 (logand (ash i -2) 1)))))
                        ~"rgb({r},{g},{b})"))
          (incf i)
          (begin-path)
          (move-to cx cy)
          (arc cx cy r start-angle end-angle false)
          (close-path)
          (save) (shadow "rgba(0,0,0,0.75)" 4 4 8) (fill) (restore)
          (fill-style "#000000")
          (font "20px Arial")
          (let ((b (wedge-center cx cy r start-angle end-angle)))
            (fill-text label
                       (+ (first b)
                          (* -0.5 (text-width label)))
                       (+ (second b)
                          10)))
          (setf start-angle end-angle))))))

(defun pie-chart-window (data x0 y0 width height title)
  (let* ((canvas (create-element "canvas"))
         (w (window x0 y0 width height
                    :title title
                    :client canvas)))
    (setf (window-resize-cback w)
          (lambda (x0 y0 x1 y1)
            (let ((w (- x1 x0))
                  (h (- y1 y0)))
              (set-style canvas
                         position "absolute"
                         px/width w
                         px/height h
                         px/left x0
                         px/top y0)
              (setf (. canvas width) w)
              (setf (. canvas height) h)
              (pie-chart canvas
                         (/ w 2) (/ h 2)
                         (* (min (- w 8) (- h 8)) 0.5 0.8)
                         data))))
    (show-window w)
    w))

(defun data-window (x0 y0 width height title)
  (let* ((w (window x0 y0 width height
                    :title title))
         (widgets (list))
         (title (create-element "input"))
         (layout (:V :border 16 :spacing 4
                     (:H :min 25 :max 25
                         (:H :min 20 :max 20)
                         (:Hdiv title :weight 300))))
         (ok (button "Show chart"
                     (lambda ()
                       (let ((data (list)))
                         (dolist (wl widgets)
                           (let* ((label (first wl))
                                  (value (second wl))
                                  (tx (. value value))
                                  (y (js-code "parseFloat(d$$tx)")))
                             (unless (or (NaN? y) (<= y 0))
                               (push (list (. label value) y) data))))
                         (when (>= (length data) 0))
                         (pie-chart-window data
                                           100 100 400 400
                                           (. title value)))))))
    (append-child (window-client w) ok)
    (append-child (window-client w) title)
    (set-style title
               position "absolute"
               border "none"
               backgroundColor "#EEEEEE"
               px/padding 0
               px/margin 0)
    (dotimes (i 8)
      (let ((caption (create-element "div"))
            (label (create-element "input"))
            (value (create-element "input")))
        (dolist (x (list label value))
          (append-child (window-client w) x)
          (set-style x
                     position "absolute"
                     border "none"
                     backgroundColor "#EEEEEE"
                     px/padding 0
                     px/margin 0))
        (append-child (window-client w) caption)
        (set-style caption
                   px/paddingTop 4
                   position "absolute"
                   textAlign "center")
        (setf (. caption innerText) (1+ i))
        (push (list label value) widgets)
        (push (:H :min 25 :max 25
                  (:Hdiv caption :max 20 :min 20)
                  (:Hdiv label :weight 200)
                  (:Hdiv value))
              (layout-node-children layout))))
    (push (:V) (layout-node-children layout))
    (push (:H :min 30 :max 30
              (:H) (:Hdiv ok :min 80) (:H))
          (layout-node-children layout))
    (setf (window-resize-cback w)
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (show-window w)))

(pie-chart-window '(("C++" 10)
                    ("Java" 20)
                    ("Lisp" 30)
                    ("Python" 40)
                    ("Ruby" 30))
                  100 100 400 400
                  "Pie chart window")

(data-window 200 200 350 350 "Data window")