(defvar documentation "\
        The [heap] module allows creation and manipulation of binary heap \
        objects. Heap objects itself are regular [list] objects in which \
        two members are added:

        [.lte]
        a function that given two elements must return [true] if \
        the first element is allowed to precede the second in the heap \
        order. Unless specified at heap creation the default value is [#'<=].

        [.tracking]
        a function that will be called passing [element] and [index] to inform \
        about the current position in the list. When an element is removed from \
        the heap the function is called passing [null] as index.
        ")

(defun heap-fix (heap index)
  "Eventually fixes the position of element [index] in the specified [heap] \
   if this is needed to maintain heap invariant.
   The function returns [true] if one or more changes were needed. The heap \
   invariant function is specified when creating the heap object with {{make-heap}}.
   This function must be called if the logical value of an element is changed \
   and this influences its positioning in respect to the heap invariant."
  (macrolet ((parent (x) `(ash (1- ,x) -1))
             (child (x) `(1+ (ash ,x 1)))
             (swap-with (x)
               `(progn
                  (setf moved true)
                  (swap (aref heap ,x) (aref heap index))
                  (when heap.tracking
                    (heap.tracking (aref heap ,x) ,x)
                    (heap.tracking (aref heap index) index))
                  (setf index ,x))))
    (let ((moved false))
      (do ((parent (parent index) (parent index)))
          ((or (= index 0) (heap.lte (aref heap parent) (aref heap index))))
        (swap-with parent))
      (do ((child (child index) (child index)))
          ((and (or (>= child (length heap))
                    (heap.lte (aref heap index) (aref heap child)))
                (or (>= (incf child) (length heap))
                    (heap.lte (aref heap index) (aref heap child)))))
        (swap-with child))
      moved)))

(defun heap-pop (heap &optional (index 0))
  "Removes and returns the specified element from the [heap], moving other \
   elements if this is required to maintain the heap invariant.
   If the heap has a [tracking] function (see {{make-heap}}) then it will \
   called on the extracted element passing [null] as index position."
  (let ((x (aref heap index)))
    (when heap.tracking (heap.tracking x null))
    (let ((y (pop heap)))
      (when (> (length heap) 0)
        (setf (aref heap index) y)
        (when heap.tracking (heap.tracking y index))
        (heap-fix heap index)))
    x))

(defun heap-push (x heap)
  "Adds the specified element to the heap, moving eventually other elements \
   if this is required to maintain the heap invariant."
  (push x heap)
  (when heap.tracking (heap.tracking x (1- (length heap))))
  (heap-fix heap (1- (length heap))))

(defun make-heap (data &optional lte tracking)
  "Transforms the specified list [data] into an heap data structure using \
   the specified heap invariant and tracking function.
   The heap invariant function [lte] takes two elements and must return [true] \
   if it's valid for the first element to appear before than the second.
   When not specified the [lte] function defaults to [#'<=].
   The tracking function if present will be called passing an element \
   and the index in which it's placed inside the heap. Every time an \
   element needs to be moved around to maintain the heap invariant the tracking \
   function will be called to inform the element. The function is also called \
   on all elements every time [make-heap] is called.
   The function [make-heap] can also accept an object that is already \
   an heap and in this case the invariant is re-established by sorting the \
   elements. When calling [make-heap] on an object that is already an heap \
   the current [lte] and [tracking] functions are maintained unless new values \
   are specified in the [make-heap] call."
  (setf lte (or lte data.lte #'<=))
  (setf tracking (or tracking data.tracking))
  (setf data.lte lte)
  (setf data.tracking tracking)
  (nsort data (lambda (a b) (not (funcall lte b a))))
  (when tracking
    (enumerate (i x data)
      (funcall tracking x i)))
  data)

(defun heap-check (heap &optional check-tracking)
  "Checks the internal consistency of an heap object
   The check is done by verifying the heap invariant and by \
   optionally calling a tracking check function on all elements.
   The check-tracking function is present should take an element and its current \
   index, returning [false] in case the index is not what is supposed to be."
  (enumerate (index x (length heap))
    (when check-tracking
      (unless (funcall check-tracking x index)
        (error ~"Invalid tracking value at index {index}")))
    (let ((parent (ash (1- index) -1))
          (child1 (+ 1 (ash index 1)))
          (child2 (+ 1 (ash index 2))))
      (unless (or (= index 0)
                  (heap.lte (aref heap parent) (aref heap index)))
        (error ~"Invalid parent invariant at index {index}"))
      (unless (or (>= child1 (length heap))
                  (heap.lte (aref heap index) (aref heap child1)))
        (error ~"Invalid child-1 invariant at index {index}"))
      (unless (or (>= child2 (length heap))
                  (heap.lte (aref heap index) (aref heap child2)))
        (error ~"Invalid child-2 invariant at index {index}")))))

(defun test-module ()
  "Module test funtion. Exercises all funcions in the module by doing 100,000+ \
   random operations and checks."
  (repeat 100
    (let ((data (map (lambda (i)
                       (list i (random-int 100)))
                     (range 10))))
      (labels ((tracking (x i)
                 (setf (first x) i))
               (check-tracking (x i)
                 (= (first x) i))
               (lte (a b)
                 (<= (second a) (second b))))
        (make-heap data #'lte #'tracking)
        (heap-check data #'check-tracking)
        (repeat 1000
          (when (and (> (length data) 0)
                     (= (random-int 2) 1))
            (heap-pop data)
            (heap-check data #'check-tracking))
          (when (= (random-int 2) 1)
            (heap-push (random-int 100) data)
            (heap-check data #'check-tracking))
          (when (and (> (length data) 0)
                     (= (random-int 100) 1))
            (let ((ix (random-int (length data))))
              (setf (second (aref data ix)) (random-int 100))
              (make-heap data)
              (heap-check data #'check-tracking))))))))

(export make-heap heap-check heap-push heap-pop heap-fix)