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
              (next-char src)
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
  (do ((to-x #(((progn x) true)))
       (to-y #(((progn y) true)))
       (common null)
       (x-active-list (list x))
       (y-active-list (list y)))
      ((or common
           (zero? (length x-active-list))
           (zero? (length y-active-list)))
         (if common
             (do ((result (list))
                  (p common))
                 ((= p true)
                    (nreverse result)
                    (do ((p (aref to-y common)))
                        ((= p true)
                           result)
                      (push p result)
                      (setf p (aref to-y p))))
               (push p result)
               (setf p (aref to-x p)))))
    (labels ((step (active-list map other-map)
               (dolist (b (splice active-list 0 (length active-list)))
                 (dolist (nh (neighbors b))
                   (unless (or (aref map nh)
                               (aref walls nh))
                     (setf (aref map nh) b)
                     (push nh active-list)
                     (when (aref other-map nh)
                       (setf common nh)))))))
      (step x-active-list to-x to-y)
      (unless common
        (step y-active-list to-y to-x)))))

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

(defun main ()
  (let* ((c (create-element "canvas"))
         (wall #())
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
    (set-style w.client
               overflow "hidden")
    (set-style c
               position "absolute")
    (set-style display
               position "absolute"
               backgroundColor "#404040"
               fontFamily "Arial, sans-serif"
               fontWeight "bold"
               px/fontSize 20
               textAlign "center"
               display "table-cell"
               verticalAlign "middle"
               px/lineHeight "30"
               color "#00FF00")
    (labels ((redraw ()
               (let ((w (. c offsetWidth))
                     (h (. c offsetHeight)))
                 (setf (. c width) w)
                 (setf (. c height) h)
                 (do ((drawn #(([0 0] true)))
                      (todo (list [0 0]))
                      (i 0))
                     ((>= i (length todo)))
                   (let ((p (aref todo (1- (incf i)))))
                     (hexagon c p
                              (if (aref wall p)
                                  "#808080"
                                  "#E0E0E0")
                              30)
                     (dolist (nh (neighbors p))
                       (unless (aref drawn nh)
                         (let ((xy (tilemap nh 30)))
                           (when (and (<= -30 (first xy) (+ w 30))
                                      (<= -30 (second xy) (+ h 30)))
                             (push nh todo)
                             (setf (aref drawn nh) true)))))))
                 (with-canvas c
                   (labels ((dot (center color)
                              (let ((p (tilemap center 30)))
                                (begin-path)
                                (arc (first p) (second p) 10 0 (* 2 pi) true)
                                (fill-style color)
                                (fill))))
                     (when from
                       (dot from "#FF0000"))
                     (when to
                       (dot to "#0000FF"))
                     (begin-path)
                     (stroke-style "#00FF00")
                     (line-width 4)
                     (dotimes (i (length solution))
                       (let ((p (tilemap (aref solution i) 30)))
                         (if (zero? i)
                             (move-to (first p) (second p))
                             (line-to (first p) (second p)))))
                     (stroke)))))
             (fixstyle ()
               (set-style wall-btn
                          backgroundColor (if (= mode "wall") "#FFFF00" "#E0E0E0"))
               (set-style check-btn
                          backgroundColor (if (= mode "check") "#FFFF00" "#E0E0E0")))
             (clear-solution ()
               (setf from null)
               (setf to null)
               (setf solution (list)))
             (set-mode (x)
               (setf mode x)
               (clear-solution)
               (redraw)
               (fixstyle)))
      (macrolet ((set-button (name caption &rest body)
                   `(progn
                      (setf ,name (button ,caption
                                          (lambda () ,@body)))
                      (append-child w.frame ,name))))
        (set-button clear-btn "Clear"
                    (setf wall #())
                    (set-mode "wall"))
        (set-button wall-btn "Wall"
                    (set-mode "wall"))
        (set-button check-btn "Check"
                    (set-mode "check")))
      (append-child w.frame c)
      (append-child w.frame display)
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
      (setf w.resize-cback
            (lambda (x0 y0 x1 y1)
              (set-coords layout x0 y0 x1 y1)
              (redraw)))

      (let ((walldraw null)
            (mousepress false))
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
                           ((and (= mode "check") mousepress)
                            (when (and from
                                       (not (aref wall p))
                                       (not (aref wall from)))
                              (setf to p)
                              (let ((ct (time
                                         (setf solution
                                               (or (shortest-path from to wall)
                                                   (list))))))
                                (setf (. display textContent) ~"{ct}ms"))
                              (redraw))))))
          (set-handler c onmousedown
                       (funcall event.preventDefault)
                       (funcall event.stopPropagation)
                       (let ((p (tile event)))
                         (setf mousepress true)
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
                       (setf mousepress false)
                       (when (= mode "wall")
                         (setf walldraw null)))))
      (show-window w))))

(main)
