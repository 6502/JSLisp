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
  data before-than index-tracking)

(defun heap (&optional (before-than #'<=) index-tracking)
  "Creates an heap object
   - before-than is a callable accepting two parms that must
     return true if first element can precede second.
   - index-tracking is a callable that if present will be called with the
     element and the index to track its position in the heap."
  (make-heap :data (list)
             :before-than before-than
             :index-tracking index-tracking))

(defmacro/f heap-length (heap)
  "Returns the number of elements contained in the heap"
  `(length (heap-data ,heap)))

(defmacro/f heap-aref (heap index)
  "Returns an element from the heap given the index"
  `(aref (heap-data ,heap) ,index))

(defun heap-fix (heap index)
  "Adjustes the position of item index in the heap assuming all other elements are
currently are satisfying the heap invariant. Returns true if elements have been moved."
  (let ((data (heap-data heap))
        (bt (heap-before-than heap))
        (sz (heap-length heap))
        (it (heap-index-tracking heap))
        (moved false))
    ;; Bubble up if lighter
    (do ((parent (heap-parent index)))
        ((or (= index 0)
             (funcall bt (aref data parent) (aref data index))))
      (setf moved true)
      (let ((x (aref data index))
            (y (aref data parent)))
        (setf (aref data index) y)
        (setf (aref data parent) x)
        (when it
          (funcall it y index)
          (funcall it x parent))
        (setf index parent)
        (setf parent (heap-parent parent))))
    ;; Sink if heavier
    (do ((child (heap-first-child index)))
        ((or (>= child sz)
             (and (funcall bt (aref data index) (aref data child))
                  (or (>= (1+ child) sz)
                      (funcall bt (aref data index) (aref data (1+ child)))))))
      (setf moved true)
      (when (and (< (1+ child) sz)
                 (not (funcall bt (aref data child) (aref data (1+ child)))))
        (incf child))
      (let ((x (aref data index))
            (y (aref data child)))
        (setf (aref data index y))
        (setf (aref data child x))
        (when it
          (funcall it y index)
          (funcall it x child))
        (setf index child)
        (setf child (heap-first-child child))))
    moved))

(defun heap-push (x heap)
  "Adds a new element to an heap, maintaining the heap invariant"
  (push x (heap-data heap))
  (when (heap-index-tracking heap)
    (funcall (heap-index-tracking heap) x (1- (heap-length heap))))
  (heap-fix heap (1- (heap-length heap))))

(defmacro/f heap-top (heap)
  "Returns the current top element of an heap, without removing it"
  `(first (heap-data ,heap)))

(defun heap-pop (heap)
  "Removes the top element from an heap and returns it"
  (let ((result (heap-top heap))
        (x (pop (heap-data heap)))
        (it (heap-index-tracking heap)))
    (when it (funcall it result null))
    (when (> (heap-length heap) 0)
      (setf (aref (heap-data heap) 0) x)
      (when it (funcall it x 0))
      (heap-fix heap 0))
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
