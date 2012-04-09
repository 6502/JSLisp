;;
;; Shortest path in an hexagonal tiling
;;  __
;; /  \__
;; \__/  \__
;; /  \__/  \__
;; \__/  \__/  \__
;; /  \__/  \__/  \__
;; \__/  \__/  \__/  \__
;; /  \__/  \__/  \__/  \__
;; \__/  \__/  \__/  \__/  \__
;; /  \__/  \__/  \__/  \__/  \
;; \__/  \__/  \__/  \__/  \__/
;;
;; as a number scheme each hexagon will be assigned a
;; [column row] pair of coordinates starting with
;; [0 0] as top hexagon. Coordinates may be negative.
;;

;; Install "[col row]" as a reader syntax for (list col row)
(setf (reader "[")
      (lambda (src)
              (funcall src 1)
              `(list ,@(parse-delimited-list src "]"))))

(defmacro/f col (x)
  "Column of hexagon [x]"
  `(first ,x))

(defmacro/f row (x)
  "Row of hexagon [x]"
  `(second ,x))

(defun neighbors (x)
  "Returns the list of neighbors of the given hexagon [x]"
  (let ((col (col x))
        (row (row x)))
    (list [col (1- row)]
          [col (1+ row)]
          [(1+ col) row]
          [(1- col) row]
          [(1+ col) (1- row)]
          [(1- col) (1+ row)])))

(defun shortest-path (x y walls)
  "Returns the shortest path starting from hexagon [x] and arriving to
  hexagon [y] that avoids the forbidden hexagons defined in the [walls] map."
  (do ((seen (js-object))
       (active-list (list x)))
      ((or (aref seen y)
           (zero? (length active-list)))
       (if (aref seen y)
           (do ((result (list))
                (p y))
               ((= p x)
                (push x result)
                (reverse result))
             (push p result)
             (setf p (aref seen p)))))
    (let ((new-active-list (list)))
      (dolist (b active-list)
        (dolist (nh (neighbors b))
          (unless (or (aref seen nh)
                      (aref walls nh))
            (setf (aref seen nh) b)
            (push nh new-active-list))))
      (setf active-list new-active-list))))

(import * from gui)
(import * from graphics)

(defconstant +HEXK+ (* 2 (/ (sqrt 3) 3)))

(defun tilemap (p scale)
  "Returns coordinates of the center of the specified tile [p]"
  (list (+ scale (* scale (col p)))
        (+ scale (* (+ (row p) (* 0.5 (col p))) scale +HEXK+))))

(defun tilerevmap (xy scale)
  "Returns the tile coordinates given an [xy] point in pixels"
  (let* ((col (floor (- (/ (first xy) scale) 0.5)))
         (row (floor (- (/ (second xy) scale +HEXK+) (* 0.5 col) 0.5))))
    [col row]))

(defun hexagon (canvas p color scale)
  "Draws an hexagonal tile on [canvas] at position [p] with specified [color] and [scale]"
  (let* ((xy (tilemap p scale))
         (x (first xy))
         (y (second xy)))
    (with-canvas canvas
      (begin-path)
      (fill-style color)
      (dotimes (i 6)
        (let ((a (/ (* 2 pi i) 6)))
          (let ((xx (+ x (* scale (cos a) 0.625)))
                (yy (+ y (* scale (sin a) 0.625))))
            (if (= i 0)
                (move-to xx yy)
                (line-to xx yy)))))
      (fill))))

(defun h= (a b)
  "True if two hexagon coordinate pairs [a] and [b] are equal"
  (and a b
       (= (row a) (row b))
       (= (col a) (col b))))

(let* ((c (create-element "canvas"))
       (wall (js-object))
       (from null)
       (to null)
       (solution (list))
       (mode "wall")
       (clear-btn null)
       (wall-btn null)
       (check-btn null)
       (layout null)
       (display (create-element "div"))
       (w (window 100 100 400 400
                  :title "Hextile shortest path")))
  (set-style (window-client w)
             overflow "hidden")
  (set-style c position "absolute")
  (set-style display
             position "absolute"
             backgroundColor "#404040"
             fontFamily "Arial, sans-serif"
             fontWeight "bold"
             px/fontSize 10
             color "#00FF00")
  (labels ((redraw ()
             (let ((w (. c offsetWidth))
                   (h (. c offsetHeight))
                   (solmap (js-object)))
               (setf (. c width) w)
               (setf (. c height) h)
               (dolist (p solution)
                 (setf (aref solmap p) true))
               (do ((drawn (js-object ([0 0] true)))
                    (todo (list [0 0]))
                    (i 0))
                   ((>= i (length todo)))
                 (let ((p (aref todo (1- (incf i)))))
                   (hexagon c p
                            (cond
                              ((h= p from) "#FF0000")
                              ((h= p to) "#0000FF")
                              ((aref solmap p) "#00FF00")
                              ((aref wall p) "#808080")
                              (true "#E0E0E0"))
                            30)
                   (dolist (nh (neighbors p))
                     (unless (aref drawn nh)
                       (let ((xy (tilemap nh 30)))
                         (when (and (<= -30 (first xy) (+ w 30))
                                    (<= -30 (second xy) (+ h 30)))
                           (push nh todo)
                           (setf (aref drawn nh) true)))))))))
           (fixstyle ()
             (set-style wall-btn
                        backgroundColor (if (= mode "wall") "#FFFF00" "#E0E0E0"))
             (set-style check-btn
                        backgroundColor (if (= mode "check") "#FFFF00" "#E0E0E0")))
           (clear-solution ()
             (setf from null)
             (setf to null)
             (setf solution (list))))
    (setf clear-btn (button
                     "Clear"
                     (lambda ()
                       (setf wall (js-object))
                       (setf mode "wall")
                       (clear-solution)
                       (redraw))))
    (setf wall-btn (button "Wall"
                           (lambda ()
                             (setf mode "wall")
                             (clear-solution)
                             (redraw)
                             (fixstyle))))
    (setf check-btn (button "Check"
                            (lambda ()
                              (setf mode "check")
                              (fixstyle))))
    (append-child (window-frame w) c)
    (append-child (window-frame w) clear-btn)
    (append-child (window-frame w) wall-btn)
    (append-child (window-frame w) check-btn)
    (append-child (window-frame w) display)
    (setf layout (:V :border 8 :spacing 8
                     (:Hdiv c)
                     (:H :max 30 :spacing 8
                         (:H :class 2)
                         (:Hdiv clear-btn :max 80)
                         (:Hdiv wall-btn :max 80)
                         (:Hdiv check-btn :max 80)
                         (:Hdiv display :max 120)
                         (:H :class 2))))
    (fixstyle)
    (dotimes (i 30)
      (setf (aref wall [(random-int 20) (random-int 20)])
            true))
    (setf (window-resize-cback w)
          (lambda (x0 y0 x1 y1)
            (set-coords layout x0 y0 x1 y1)
            (redraw)))

    (let ((walldraw null))
      (labels ((tile (event)
                 (let ((xy (event-pos event))
                       (xy0 (element-pos c)))
                   (tilerevmap (list (- (first xy) (first xy0))
                                     (- (second xy) (second xy0)))
                               30))))
        (set-handler c onmousemove
                     (let ((p (tile event)))
                       (cond
                         ((= mode "wall")
                          (when (bool? walldraw)
                            (setf (aref wall p) walldraw)
                            (redraw)))
                         ((= mode "check")
                          (when (and from
                                     (not (aref wall p))
                                     (not (aref wall from)))
                            (setf to p)
                            (let ((ct (time
                                       (setf solution
                                             (or (shortest-path from to wall)
                                                 (list))))))
                              (setf (. display innerText)
                                    ~"len={(length solution)}, time={ct}ms"))
                            (redraw))))))
        (set-handler c onmousedown
                     (let ((p (tile event)))
                       (cond
                         ((= mode "wall")
                          (setf walldraw (not (aref wall p)))
                          (setf (aref wall p) walldraw)
                          (redraw))
                         ((= mode "check")
                          (when (not (aref wall p))
                            (clear-solution)
                            (setf from p)
                            (redraw))))))
        (set-handler c onmouseup
                     (when (= mode "wall")
                       (setf walldraw null)))))

    (show-window w)))
