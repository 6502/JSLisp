;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                                                          ;;;
;;;  Copyright (c) 2011 by Andrea Griffini                                   ;;;
;;;                                                                          ;;;
;;;  Permission is hereby granted, free of charge, to any person obtaining   ;;;
;;;  a copy of this software and associated documentation files (the         ;;;
;;;  "Software"), to deal in the Software without restriction, including     ;;;
;;;  without limitation the rights to use, copy, modify, merge, publish,     ;;;
;;;  distribute, sublicense, and/or sell copies of the Software, and to      ;;;
;;;  permit persons to whom the Software is furnished to do so, subject to   ;;;
;;;  the following conditions:                                               ;;;
;;;                                                                          ;;;
;;;  The above copyright notice and this permission notice shall be          ;;;
;;;  included in all copies or substantial portions of the Software.         ;;;
;;;                                                                          ;;;
;;;  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,         ;;;
;;;  EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF      ;;;
;;;  MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND                   ;;;
;;;  NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT HOLDERS BE  ;;;
;;;  LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER IN AN ACTION  ;;;
;;;  OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN CONNECTION   ;;;
;;;  WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.         ;;;
;;;                                                                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defstruct p2d x y)

(defun p2d (x y)
  (make-p2d :x x :y y))

(defun canvas (width height)
  (let ((canvas (create-element "canvas")))
    (setf (. canvas width) width)
    (setf (. canvas height) height)
    (setf (. canvas style width) (+ width "px"))
    (setf (. canvas style height) (+ height "px"))
    canvas))

(defun box (canvas color x y w h)
  (let ((ctx (funcall (. canvas getContext) "2d")))
    (setf (. ctx fillStyle) color)
    (funcall (. ctx fillRect) x y w h)))

(defun polyline (canvas color pen-width pts)
  (let ((ctx (funcall (. canvas getContext) "2d")))
    (setf (. ctx strokeStyle) color)
    (setf (. ctx lineWidth) pen-width)
    (funcall (. ctx beginPath))
    (funcall (. ctx moveTo) (p2d-x (first pts)) (p2d-y (first pts)))
    (dolist (p (rest pts))
      (funcall (. ctx lineTo) (p2d-x p) (p2d-y p)))
    (funcall (. ctx stroke))))

(let ((canvas (canvas 200 200)))
  (box canvas "#FF0000" 0 0 200 200)
  (box canvas "#FFFF00" 5 5 190 190)
  (polyline canvas "#000000" 2
            (map (lambda (j)
                   (let ((t (/ (* j 2 pi) 6)))
                     (p2d (+ 100 (* 80 (cos t)))
                          (+ 100 (* 80 (sin t))))))
                 (range 7)))
  (setf (. canvas style position) "absolute")
  (setf (. canvas style left) "100px")
  (setf (. canvas style top) "100px")
  (setf (. canvas onclick) (lambda () (remove-child (. document body) canvas)))
  (append-child (. document body) canvas))
