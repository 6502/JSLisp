(import * from gui)
(import * from layout)

(defun editor ()
  (let** ((w (window 0 0 0.75 0.75 title: "GUI editor"))
          (area (set-style (create-element "div")
                           overflow "auto"
                           position "absolute"))
          (widget-list (set-style (create-element "div")
                                  position "absolute"
                                  background-color "#EEE"
                                  overflow "auto"))
          (current null)
          (#'set-current (x)
            (when current
              (set-style (aref current.children 1)
                         backgroundColor "rgba(0,0,0,0.02)")
              (set-style (aref current.children 2)
                         display "none")
              (set-style (aref current.children 3)
                         display "none"))
            (setf current x)
            (when current
              (set-style (aref current.children 1)
                         backgroundColor "rgba(255,0,0,0.25)")
              (set-style (aref current.children 2)
                         display "block")
              (set-style (aref current.children 3)
                         display "block")))
          (#'rect-selection (event)
            (set-current null)
            (let** ((x0 (first (relative-pos event area)))
                    (y0 (second (relative-pos event area)))
                    (x1 x0)
                    (y1 y0)
                    (d (set-style (create-element "div")
                                  position "absolute"
                                  px/left x0
                                  px/top y0
                                  px/width 0
                                  px/height 0
                                  backgroundColor "rgba(255,0,0,0.25)")))
              (append-child area d)
              (tracking (lambda (x y)
                          (setf x1 x)
                          (setf y1 y)
                          (set-style d
                                     px/left (min x0 x1)
                                     px/top (min y0 y1)
                                     px/width (abs (- x1 x0))
                                     px/height (abs (- y1 y0))))
                        (lambda ()
                          (remove-child area d)
                          (let** ((widgets (filter (lambda (d)
                                                     (and (>= d.offsetLeft (min x0 x1))
                                                          (>= d.offsetTop (min y0 y0))
                                                          (<= (+ d.offsetLeft d.offsetWidth) (max x0 x1))
                                                          (<= (+ d.offsetTop d.offsetHeight) (max y0 y1))))
                                                   area.children))
                                  (n (length widgets))
                                  (left-avg (/ (reduce #'+ (map (get offsetLeft) widgets)) n))
                                  (top-avg (/ (reduce #'+ (map (get offsetTop) widgets)) n))
                                  (h-score (reduce #'+ (map (lambda (d) (expt (- d.offsetTop top-avg) 2)) widgets)))
                                  (v-score (reduce #'+ (map (lambda (d) (expt (- d.offsetLeft left-avg) 2)) widgets))))
                            (when (> n 1)
                              (let** ((ww (if (< h-score v-score)
                                              (+ (* (1- (length widgets)) 8)
                                                 (reduce #'+ (map (get offsetWidth) widgets)))
                                              (apply #'max (map (get offsetWidth) widgets))))
                                      (hh (if (< h-score v-score)
                                              (apply #'max (map (get offsetHeight) widgets))
                                              (+ (* (1- (length widgets)) 8)
                                                 (reduce #'+ (map (get offsetHeight) widgets)))))
                                      (d (set-style (create-element "div")
                                                    position "absolute"
                                                    px/left 0
                                                    px/top 0
                                                    px/width ww
                                                    px/height hh))
                                      (layout (if (< h-score v-score)
                                                  (H border: 0 spacing: 8)
                                                  (V border: 0 spacing: 8)))
                                      (spacers (any (w widgets) w.firstChild.data-spacer))
                                      (xa (apply #'min (map (get offsetLeft) widgets)))
                                      (ya (apply #'min (map (get offsetTop) widgets)))
                                      (node #((children (list))
                                              (text (if (< h-score v-score) "H-group" "V-group")))))
                                (nsort widgets (if (< h-score v-score)
                                                   (lambda (a b) (< a.offsetLeft b.offsetLeft))
                                                   (lambda (a b) (< a.offsetTop b.offsetTop))))
                                (dolist (w widgets)
                                  (if spacers
                                      (if w.firstChild.data-spacer
                                          (add-element layout (dom w))
                                          (add-element layout
                                                       size: (if (< h-score v-score)
                                                                 w.offsetWidth
                                                                 w.offsetHeight)
                                                       (dom w)))
                                      (add-element layout
                                                   weight: (if (< h-score v-score)
                                                               w.offsetWidth
                                                               w.offsetHeight)
                                                   (dom w)))
                                  (set-style (aref w.children 1)
                                             backgroundColor "none")
                                  (nremove w.data-node tree.children)
                                  (push w.data-node node.children)
                                  (append-child d w))
                                (setf d.data-resize (lambda (x0 y0 x1 y1)
                                                      (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
                                (let ((box (wrap d xa ya)))
                                  (setf box.data-node node)
                                  (setf node.box box)
                                  (push node tree.children)
                                  (wtree.rebuild))))))
                        "pointer"
                        (element-pos area))))
          (#'wrap (d x0 y0)
            (let** ((box (set-style (create-element "div")
                                    position "absolute"))
                    (glass (set-style (create-element "div")
                                      position "absolute"
                                      px/left -4
                                      px/top -4
                                      px/right -4
                                      px/bottom -4
                                      cursor "move"
                                      backgroundColor "rgba(0,0,0,0.02)"))
                    (width-handle (set-style (create-element "div")
                                             position "absolute"
                                             px/width 8
                                             px/height 8
                                             cursor "e-resize"
                                             backgroundColor "#F00"))
                    (height-handle (set-style (create-element "div")
                                              position "absolute"
                                              px/width 8
                                              px/height 8
                                              cursor "s-resize"
                                              backgroundColor "#F00"))
                    (#'fix-box ()
                      (set-style d
                                 px/width box.offsetWidth
                                 px/height box.offsetHeight)
                      (when d.data-resize
                        (d.data-resize 0 0 d.offsetWidth d.offsetHeight))
                      (set-style height-handle
                                 px/left (+ -4 (/ box.offsetWidth 2))
                                 px/top (+ -4 box.offsetHeight))
                      (set-style width-handle
                                 px/left (+ -4 box.offsetWidth)
                                 px/top (+ -4 (/ box.offsetHeight 2)))))
              (setf box.data-resize #'fix-box)
              (append-child box d)
              (append-child box glass)
              (append-child box width-handle)
              (append-child box height-handle)
              (append-child area box)
              (set-style box
                         px/left x0
                         px/top y0
                         px/width d.offsetWidth
                         px/height d.offsetHeight)
              (fix-box)
              (set-handler box onmousedown
                (event.preventDefault)
                (event.stopPropagation)
                (set-current box)
                (let (((x y) (event-pos event)))
                  (dragging box x y)))
              (set-handler width-handle onmousedown
                (event.preventDefault)
                (event.stopPropagation)
                (let ((x (first (event-pos event))))
                  (tracking (lambda (xx yy)
                              (declare (ignorable yy))
                              (let ((dx (- xx x)))
                                (setf x xx)
                                (setf box.style.width ~"{(max 10 (min (+ box.offsetWidth dx)))}px"))
                              (fix-box)))))
              (set-handler height-handle onmousedown
                (event.preventDefault)
                (event.stopPropagation)
                (let ((y (second (event-pos event))))
                  (tracking (lambda (xx yy)
                              (declare (ignorable xx))
                              (let ((dy (- yy y)))
                                (setf y yy)
                                (setf box.style.height ~"{(max 10 (min (+ box.offsetHeight dy)))}px"))
                              (fix-box)))))
              (set-current box)
              box))
          (#'add-widget-button (text builder)
            (let** ((c (button text #'add))
                    (#'add ()
                      (let* ((box (wrap (funcall builder) 0 0))
                             (node #((children (list))
                                     (text text)
                                     (box box))))
                        (setf box.data-node node)
                        (push node tree.children)
                        (wtree.rebuild)
                        (set-style box
                                   px/left (/ (- area.offsetWidth box.offsetWidth) 2)
                                   px/top (/ (- area.offsetHeight box.offsetHeight) 2)))))
              (append-child widget-list c)))
          (#'fix-widget-list ()
            (do ((c widget-list.firstChild c.nextSibling)
                 (i 0 (1+ i)))
              ((not c))
              (set-style c
                         position "absolute"
                         px/left 2
                         px/top (+ (* i 30) 2)
                         px/right 2
                         px/height 26)))
          (tree #((children (list))
                  (text "Window")))
          (wtree (set-style (tree-view tree)
                            position "absolute"
                            overflow "auto"))
          (vs (v-splitter widget-list wtree))
          (hs (add-widget w (h-splitter area vs split: 90)))
          (widgets (list))
          (layout null))
    (setf widget-list.data-resize #'fix-widget-list)
    (set-handler area onmousedown
      (event.preventDefault)
      (event.stopPropagation)
      (rect-selection event))

    (add-widget-button "Button" (lambda () (set-style (button "Button" (lambda ()))
                                                      position "absolute"
                                                      px/width 80
                                                      px/height 30)))

    (add-widget-button "Input" (lambda () (set-style (input "Input field")
                                                     position "absolute"
                                                     px/width 200
                                                     px/height 40)))

    (add-widget-button "Select" (lambda () (set-style (select "Select field" (range 10))
                                                      position "absolute"
                                                      px/width 200
                                                      px/height 40)))

    (add-widget-button "Checkbox" (lambda () (set-style (checkbox "Checkbox")
                                                        position "absolute"
                                                        px/width 200
                                                        px/height 40)))

    (add-widget-button "Radio" (lambda () (set-style (radio 1 "Radio button")
                                                     position "absolute"
                                                     px/width 200
                                                     px/height 40)))

    (add-widget-button "Textarea" (lambda () (set-style (text-area "Text area")
                                                        position "absolute"
                                                        px/width 200
                                                        px/height 80)))

    (add-widget-button "Color" (lambda () (set-style (css-color-input "Color")
                                                     position "absolute"
                                                     px/width 200
                                                     px/height 40)))

    (add-widget-button "Date" (lambda () (set-style (date-input "Date")
                                                    position "absolute"
                                                    px/width 120
                                                    px/height 40)))

    (add-widget-button "Spacer" (lambda () (set-style (let ((d (create-element "div")))
                                                        (setf d.data-spacer true)
                                                        d)
                                                      position "absolute"
                                                      backgroundColor "#CEE"
                                                      px/width 20
                                                      px/height 20)))

    (set-layout w (H spacing: 8 border: 8
                     (dom hs)))
    (show-window w center: true)))

(defun main ()
  (editor))

(main)
