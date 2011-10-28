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

(defmacro/f heap-parent (x)
  "Returns the index of the parent of the element with index x"
  `(js-code ,(+ "((" (js-compile x) "-1)>>1)")))

(defmacro/f heap-first-child (x)
  "Returns the index of the first child of the element with index x"
  `(js-code ,(+ "(1+(" (js-compile x) "<<1))")))

(defmacro/f heap-second-child (x)
  "Returns the index of the second child of the element with index x"
  `(js-code ,(+ "(2+(" (js-compile x) "<<1))")))

(defstruct heap
  data before-than)

(defun heap (&optional (before-than #'<=))
  "Creates an heap object sorted on the optionally specified comparison function"
  (make-heap :data (list)
             :before-than before-than))

(defmacro/f heap-length (heap)
  "Returns the number of elements contained in the heap"
  `(length (heap-data ,heap)))

(defun heap-push (x heap)
  "Adds a new element to an heap, maintaining the heap invariant"
  (let* ((data (heap-data heap))
         (ix (length data))
         (bt (heap-before-than heap)))
    (do ((i ix parent)
         (parent (heap-parent ix) (heap-parent parent)))
        ((or (= i 0) (funcall bt (aref data parent) x))
           (setf (aref data i) x))
      (setf (aref data i) (aref data parent)))))

(defmacro/f heap-top (heap)
  "Returns the current top element of an heap, without removing it"
  `(first (heap-data ,heap)))

(defun heap-pop (heap)
  "Removes the top element from an heap and returns it"
  (let* ((data (heap-data heap))
         (bt (heap-before-than heap))
         (result (first data))
         (sz (1- (length data))))
    (if (zerop sz)
        (pop data)
        (do ((i 0 c)
             (x (pop data))
             (c (heap-first-child 0) (heap-first-child c)))
            ((or (>= c sz)
                 (and (funcall bt x (aref data c))
                      (or (>= (1+ c) sz)
                          (funcall bt x (aref data (1+ c))))))
             (setf (aref data i) x))
          (when (and (< (1+ c) sz)
                     (not (funcall bt (aref data c) (aref data (1+ c)))))
            (incf c))
          (setf (aref data i) (aref data c))))
    result))

(defun heap-check (heap)
  "Checks internal heap consistency"
  (let ((data (heap-data heap))
        (bt (heap-before-than heap))
        (sz (heap-length heap)))
    (dotimes (i sz)
      (let ((c1 (heap-first-child i))
            (c2 (heap-second-child i)))
        (when (or (and (< c1 sz) (not (funcall bt (aref data i) (aref data c1))))
                  (and (< c2 sz) (not (funcall bt (aref data i) (aref data c2)))))
          (error "Invalid heap data"))))))

(let ((x (heap #'>=)))
  (dotimes (i 10)
    (heap-push (- 40 i) x)
    (heap-push (/ i 2) x)
    (heap-push i x))
  (do () ((zerop (heap-length x)))
    (heap-check x)
    (display (heap-pop x))))
