(import * from gui)
(import * from graphics)
(import examples/chess as chess)

(defvar *piece-images* #())
(defvar *image-urls* (let ((res #()))
                       (dolist (col "wb")
                         (dolist (piece "prnbqk")
                           (setf (aref res (if (= col "b") piece (piece.toUpperCase)))
                                 (image-data-url ~"examples/img/{col}{piece}.png"))))
                       res))

(defun canvas-chessboard (square-size &optional position move-callback)
  "Creates a canvas chessboard with given [square-size] and [position] (a 64-chars
string of chars from \"RBNBQKrnbqk.\"). If a [move-callback] is specified the
user can make a move by dragging a piece on the chessboard and the function will
be called with start/end squares (two numbers between 0 and 63)."
  (let ((canvas (create-element "canvas"))
        (k (floor (/ square-size 12)))
        (dragging null)
        (b 2))
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

(defun wmain ()
  (chess:init-board)
  (let** ((pnames (let ((pp #()))
                    (setf (aref pp chess:+WP+) "P")
                    (setf (aref pp chess:+WR+) "R")
                    (setf (aref pp chess:+WN+) "N")
                    (setf (aref pp chess:+WB+) "B")
                    (setf (aref pp chess:+WQ+) "Q")
                    (setf (aref pp chess:+WK+) "K")
                    (setf (aref pp chess:+BP+) "p")
                    (setf (aref pp chess:+BR+) "r")
                    (setf (aref pp chess:+BN+) "n")
                    (setf (aref pp chess:+BB+) "b")
                    (setf (aref pp chess:+BQ+) "q")
                    (setf (aref pp chess:+BK+) "k")
                    (setf (aref pp chess:+EMPTY+) ".")
                    pp))
          (flip false)
          (board (append-child document.body (create-element "div")))
          (back (append-child document.body (create-element "div")))
          (new (append-child document.body (create-element "div")))
          (dot (append-child document.body (create-element "div")))
          (#'move (m)
            (chess:play m)
            (repaint)
            (set-timeout (lambda ()
                           (chess:computer 0)
                           (repaint))
                         100))
          (#'play (p0 p1)
            (when flip
              (setf p0 (- 63 p0))
              (setf p1 (- 63 p1)))
            (let* ((L (list))
                   (x0 (logand p0 7))
                   (y0 (ash p0 -3))
                   (x1 (logand p1 7))
                   (y1 (ash p1 -3))
                   (i0 (+ 21 (* y0 10) x0))
                   (i1 (+ 21 (* y1 10) x1)))
              (chess:move-map (lambda (m)
                                (when (and (= (chess:move-x0 m) i0)
                                           (= (chess:move-x1 m) i1))
                                  (push m L))))
              (if (> (length L) 1)
                  (let ((shadow (append-child document.body (create-element "div")))
                        (sqsize (floor (min (/ (screen-height) 9)
                                            (/ (screen-width) 12)))))
                    (set-style shadow
                               position "absolute"
                               px/left 0
                               px/top 0
                               px/right 0
                               px/bottom 0
                               text-align "center"
                               background "rgba(192,192,192,0.75)")
                    (append-child shadow (set-style (create-element "div")
                                                    px/height (* sqsize 3)))
                    (dolist (p (list chess:+QUEEN+
                                     chess:+ROOK+
                                     chess:+KNIGHT+
                                     chess:+BISHOP+))
                      (let ((piece (append-child shadow (create-element "img"))))
                        (set-style piece
                                   px/width (* 2 sqsize)
                                   px/height (* 2 sqsize))
                        (setf piece.src (aref *image-urls* (aref pnames (+ chess:*color* p))))
                        (setf piece.onmousedown
                              (lambda ()
                                (remove-child document.body shadow)
                                (move (chess:move i0 i1 (+ chess:*color* p))))))))
                  (move (first L)))))
          (#'back ()
            (when (> (length chess:*history*) 1)
              (chess:undo)
              (chess:undo)
              (repaint)))
          (#'new ()
            (chess:init-board)
            (setf flip (not flip))
            (repaint)
            (when flip
              (set-timeout (lambda ()
                             (chess:computer 0)
                             (repaint))
                           100)))
          (#'repaint ()
            (let** ((sqsize (floor (min (/ (screen-height) 9)
                                        (/ (screen-width) 12))))
                    (bc (let ((pos (list)))
                          (dotimes (i 8)
                            (dotimes (j 8)
                              (push (aref pnames (aref chess:*sq* (+ 21 (* i 10) j))) pos)))
                          (canvas-chessboard sqsize (if flip (reverse pos) pos)
                                             #'play))))
              (setf board.innerHTML "")
              (append-child board bc)
              (set-style board
                         position "absolute"
                         px/left (/ sqsize 2)
                         px/top (/ (- (screen-height) (* 8 sqsize)) 2))
              (set-style dot
                         position "absolute"
                         px/left (* 8.75 sqsize)
                         px/width (/ sqsize 4)
                         px/height (/ sqsize 4)
                         px/border-radius (/ sqsize 8)
                         box-shadow "2px 2px 2px rgba(0,0,0,0.5)"
                         background-color (if (= chess:*color* chess:+WHITE+) "#FFFFFF" "#000000")
                         px/top (+ (/ (- (screen-height) (* 8 sqsize)) 2)
                                   (if (/= flip (= chess:*color* chess:+WHITE+))
                                       (* sqsize (- 7.5 0.125))
                                       (* sqsize (- 0.5 0.125)))))
              (set-style back
                         border "solid 1px #000"
                         box-shadow "4px 4px 4px rgba(0,0,0,0.25)"
                         px/border-radius (/ sqsize 8)
                         px/padding (/ sqsize 8)
                         background-color "#FFF"
                         color "#000"
                         position "absolute"
                         text-align "center"
                         px/font-size (/ sqsize 3)
                         font-weight "bold"
                         px/right (/ sqsize 2)
                         px/width (- (screen-width) (* 10 sqsize))
                         cursor "pointer"
                         px/top (+ sqsize (/ (- (screen-height) (* 8 sqsize)) 2)))
              (set-style new
                         border "solid 1px #000"
                         box-shadow "4px 4px 4px rgba(0,0,0,0.25)"
                         px/border-radius (/ sqsize 8)
                         px/padding (/ sqsize 8)
                         background-color "#FFF"
                         color "#000"
                         position "absolute"
                         px/right sqsize
                         text-align "center"
                         px/font-size (/ sqsize 3)
                         font-weight "bold"
                         px/right (/ sqsize 2)
                         px/width (- (screen-width) (* 10 sqsize))
                         cursor "pointer"
                         px/bottom (+ sqsize (/ (- (screen-height) (* 8 sqsize)) 2)))))
          (cwidth null)
          (cheight null))
    (setf back.textContent "Indietro")
    (setf new.textContent "Nuova partita")
    (setf back.onclick #'back)
    (setf new.onclick #'new)
    (repaint)
    (set-interval (lambda ()
                    (when (or (/= (screen-width) cwidth)
                              (/= (screen-height) cheight))
                      (setf cwidth (screen-width))
                      (setf cheight (screen-height))
                      (repaint)))
                  100)))

(defun main ()
  (setf document.body.style.backgroundColor "#778899")
  (let ((count 0))
    (dolist (k (keys *image-urls*))
      (let ((c (create-element "img")))
        (setf (aref *piece-images* k) c)
        (setf c.onload (lambda ()
                         (when (= 12 (incf count))
                           (wmain))))
        (setf c.src (aref *image-urls* k))))))

(main)
