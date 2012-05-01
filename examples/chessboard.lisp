(import * from gui)
(import * from graphics)
(import examples/chess as chess)

(defvar *piece-images* (js-object))

(defun canvas-chessboard (square-size &optional position move-callback)
  "Creates a canvas chessboard with given [square-size] and [position] (a 64-chars
string of chars from \"RBNBQKrnbqk.\"). If a [move-callback] is specified the
user can make a move by dragging a piece on the chessboard and the function will
be called with start/end squares (two numbers between 0 and 63)."
  (let ((canvas (create-element "canvas"))
        (k (floor (/ square-size 12)))
        (dragging null)
        (b 8))
    (set-style canvas
               border ~"solid {b}px #806040"
               position "absolute"
               px/left 0
               px/top 0
               px/width (* square-size 8)
               px/height (* square-size 8))
    (unless position
      (setf position #.(+ "rnbqkbnr"
                          "pppppppp"
                          "........"
                          "........"
                          "........"
                          "........"
                          "PPPPPPPP"
                          "RNBQKBNR")))
    (labels ((redraw ()
               (setf (. canvas width) (* square-size 8))
               (setf (. canvas height) (* square-size 8))
               (with-canvas canvas
                 (image-smoothing true)
                 (dotimes (i 8)
                   (dotimes (j 8)
                     (fill-style (if (even? (+ i j)) "#FFEECC" "#CCA060"))
                     (rect (* j square-size) (* i square-size)
                           square-size square-size)
                     (fill)
                     (let* ((index (+ (* i 8) j))
                            (p (aref position index)))
                       (unless (or (= index dragging) (= p "."))
                         (image (aref *piece-images* p)
                                (- (* j square-size) k)
                                (- (* i square-size) k)
                                (+ square-size (* 2 k))
                                (+ square-size (* 2 k))))))))))
      (redraw)
      (when move-callback
        (set-handler canvas onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (let* ((p (event-pos event))
                            (bp (element-pos canvas))
                            (x (- (first p) (first bp) b))
                            (y (- (second p) (second bp) b))
                            (ix (floor (/ x square-size)))
                            (iy (floor (/ y square-size))))
                       (when (and (<= 0 ix 7) (<= 0 iy 7))
                         (let ((i (+ (* iy 8) ix)))
                           (when (/= (aref position i) ".")
                             (let ((dragged (create-element "canvas"))
                                   (dx (- (first p) (+ (first bp) b (* square-size ix))))
                                   (dy (- (second p) (+ (second bp) b (* square-size iy)))))
                               (setf (. dragged width) square-size)
                               (setf (. dragged height) square-size)
                               (with-canvas dragged
                                 (image-smoothing true)
                                 (image (aref *piece-images* (aref position i))
                                        (- k)
                                        (- k)
                                        (+ square-size (* 2 k))
                                        (+ square-size (* 2 k))))
                               (set-style dragged
                                          position "absolute"
                                          px/left (- (first p) dx)
                                          px/top (- (second p) dy)
                                          px/width square-size
                                          px/height square-size)
                               (append-child (. document body) dragged)
                               (setf dragging i)
                               (with-canvas canvas
                                 (fill-style "#FF0000")
                                 (rect (* ix square-size) (* iy square-size)
                                       square-size square-size)
                                 (fill))
                               (tracking (lambda (x y)
                                           (set-style dragged
                                                      px/left (- x dx)
                                                      px/top (- y dy)))
                                         (lambda (x y)
                                           (setf dragging null)
                                           (remove-child (. document body) dragged)
                                           (redraw)
                                           (let ((ix (floor (+ 0.5 (/ (- x (first bp) b dx) square-size))))
                                                 (iy (floor (+ 0.5 (/ (- y (second bp) b dy) square-size)))))
                                             (when (and (<= 0 ix 7)
                                                        (<= 0 iy 7)
                                                        (/= i (+ (* iy 8) ix)))
                                               (funcall move-callback i (+ (* iy 8) ix)))))))))))))
      canvas)))

(defun chessboard-window (x0 y0 w h title
                          &optional position move-cback)
  (let ((w (window x0 y0 w h
                   :title title)))
    (labels ((update ()
               (let* ((client (window-client w))
                      (cw (. client clientWidth))
                      (ch (. client clientHeight))
                      (sqsz (floor (/ (min cw ch) 9)))
                      (board (canvas-chessboard sqsz
                                                (first (window-data w))
                                                move-cback)))
                 (set-style board
                            px/left (floor (/ (- cw (* 8 sqsz) 8) 2))
                            px/top (floor (/ (- ch (* 8 sqsz) 8) 2)))
                 (when (. client firstChild)
                   (remove-child client (. client firstChild)))
                 (append-child client board))))
      (setf (window-resize-cback w) #'update)
      (setf (window-data w) (list position #'update))
      (show-window w)
      w)))

(defun set-position (window position)
  (setf (first (window-data window)) position)
  (funcall (second (window-data window))))

(defun wmain ()
  (let ((pnames (js-object))
        (w null))
    (setf (aref pnames chess:+WP+) "P")
    (setf (aref pnames chess:+WR+) "R")
    (setf (aref pnames chess:+WN+) "N")
    (setf (aref pnames chess:+WB+) "B")
    (setf (aref pnames chess:+WQ+) "Q")
    (setf (aref pnames chess:+WK+) "K")
    (setf (aref pnames chess:+BP+) "p")
    (setf (aref pnames chess:+BR+) "r")
    (setf (aref pnames chess:+BN+) "n")
    (setf (aref pnames chess:+BB+) "b")
    (setf (aref pnames chess:+BQ+) "q")
    (setf (aref pnames chess:+BK+) "k")
    (setf (aref pnames chess:+EMPTY+) ".")
    (chess:init-board)
    (labels ((position ()
               (let ((res ""))
                 (dolist (x chess:*sq*)
                   (incf res (or (aref pnames x) "")))
                 res)))
      (setf w (chessboard-window
               100 100 400 400 "Chessboard"
               (position)
               (lambda (from to)
                 (let ((mm (list))
                       (x0 (chess:tosq (ash from -3) (logand from 7)))
                       (x1 (chess:tosq (ash to -3) (logand to 7))))
                   (chess:move-map (lambda (m)
                                     (when (and (= (chess:move-x0 m) x0)
                                                (= (chess:move-x1 m) x1))
                                       (push m mm))))
                   (when (> (length mm) 0)
                     (chess:play (first mm))
                     (set-position w (position))
                     (when true
                       (set-timeout (lambda ()
                                      (chess:computer 1)
                                      (set-position w (position)))
                                    0))))))))
    w))

(defun main ()
  (setf *piece-images*
        (let ((images (js-object))
              (count 0))
          (dolist (x "PRNBQKprnbqk")
            (let ((img (create-element "img")))
              (setf (. img onload)
                    (lambda () (when (= (incf count) 12)
                                 (wmain))))
              (setf (. img src) (+ x ".png"))
              (setf (aref images x) img)))
          images)))

(main)
