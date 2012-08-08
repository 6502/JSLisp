(import * from gui)

(defun mandelbrot (x0 y0 max-iter)
  "Number of iterations after which [x0 y0] is proven to be out of Mandelbrot set or
   returns [max-iter] if it cannot be proven out before this limit"
  (do ((i 0 (1+ i))
       (x x0)
       (y y0)
       (x2 (* x0 x0) (* x x))
       (y2 (* y0 y0) (* y y)))
      ((or (= i max-iter) (> (+ x2 y2) 4)) i)
    (setf y (+ y0 (* 2 x y)))
    (setf x (+ x0 (- x2 y2)))))

(defun mandelbrot-pic (x0 y0 x1 y1 w h palette)
  "Builds a Mandelbrot set picture of rectangle [x0 y0]/[x1 y1] with [w]x[h] pixels
   resolution and the specified color [palette] that be a list of RGBA lists of integers.
   The lenght of [palette] is used as maximum number of iterations."
  (let ((pic (create-element "canvas")))
    (setf (. pic width) w)
    (setf (. pic height) h)
    (let* ((ctx (funcall (. pic getContext) "2d"))
           (idata (funcall (. ctx getImageData) 0 0 w h))
           (data (. idata data))
           (wp 0)
           (max-iter (1- (length palette))))
      (dotimes (i h)
        (let ((y (+ y0 (/ (* i (- y1 y0)) h))))
          (dotimes (j w)
            (let* ((x (+ x0 (/ (* j (- x1 x0)) w)))
                   (value (aref palette (mandelbrot x y max-iter))))
              (setf (aref data wp) (first value))
              (setf (aref data (+ wp 1)) (second value))
              (setf (aref data (+ wp 2)) (third value))
              (setf (aref data (+ wp 3)) (fourth value))
              (incf wp 4)))))
      (funcall (. ctx putImageData) idata 0 0))
    pic))

(defun mandelbrot-explorer (xa ya xb yb ww hh)
  "Shows a window that allows exploring the Mandelbrot fractal"
  (let ((w (window 20 20 ww hh :title "Mandelbrot fractal" :resize false))
        (zoomrect (create-element "div"))
        (lastw -1)
        (lasth -1)
        (palette (map (lambda (x) (list x x (min 255 (+ x  64)) 255)) (range 256)))
        (pic null))
    (set-style zoomrect
               position "absolute"
               backgroundColor "rgba(255,0,0,0.25)"
               display "none"
               px/left 50
               px/top 50
               px/width 300
               px/height 300)
    (append-child w.client zoomrect)
    (set-style w.client
               overflow "hidden")
    (labels ((pix-to-xy (p)
               (let* ((x (first p))
                      (y (second p)))
                 (list (+ xa (/ (* x (- xb xa)) w.client.offsetWidth))
                       (+ ya (/ (* y (- yb ya)) w.client.offsetHeight)))))
             (recalc (ww hh)
               (when pic
                 (remove-child w.client pic))
               (setf pic (mandelbrot-pic xa ya xb yb ww hh palette))
               (set-style pic
                          position "absolute"
                          px/left 0
                          px/top 0
                          px/width ww
                          px/height hh)
               (append-child w.client pic)
               (append-child w.client zoomrect)))

      (setf w.resize-cback
            (lambda (x0 y0 x1 y1)
              (let ((ww (- x1 x0))
                    (hh (- y1 y0)))
                (when (or (/= ww lastw) (/= hh lasth))
                  (setf lastw ww)
                  (setf lasth hh)
                  (recalc ww hh)))))

      (set-handler w.client onmousedown
                   (funcall (. event preventDefault))
                   (funcall (. event stopPropagation))
                   (let* ((p (event-pos event))
                          (p0 (element-pos w.client))
                          (xx (- (first p) (first p0)))
                          (yy (- (second p) (second p0)))
                          (k (/ lastw lasth)))
                     (tracking (lambda (ex ey)
                                 (let ((x (- ex (first p0)))
                                       (y (- ey (second p0))))
                                   (set-style zoomrect
                                              display "block"
                                              px/left (min xx x)
                                              px/top (min yy y)
                                              px/width (* k (abs (- yy y)))
                                              px/height (abs (- yy y)))))
                               (lambda (ex ey)
                                 (let* ((x (- ex (first p0)))
                                        (y (- ey (second p0)))
                                        (pa (pix-to-xy (list (min x xx) (min y yy))))
                                        (ph (- (second (pix-to-xy (list (max x xx) (max y yy))))
                                               (second pa)))
                                        (pw (* k ph)))
                                   (setf xa (first pa))
                                   (setf ya (second pa))
                                   (setf xb (+ xa pw))
                                   (setf yb (+ ya ph))
                                   (set-style zoomrect
                                              display "none")
                                   (recalc lastw lasth)))))))
    (show-window w)))

(defun main ()
  (mandelbrot-explorer -3 -2 1 2
                       512 512))

(main)