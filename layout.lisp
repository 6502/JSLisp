
(defun set-coords (node x0 y0 x1 y1)
  (error ~"Unable to set coords for {(str-value node)}"))

;; Setting the coordinates of a null, string or number node is a NOP

(defmethod set-coords (node x0 y0 x1 y1) (or (null? node)
                                             (string? node)
                                             (number? node))
  null)

;; A callback node; used to inform someone of geometry computation

(defobject callback
    (element f))

(defmethod set-coords (node x0 y0 x1 y1) (callback? node)
  (node.f x0 y0 x1 y1)
  (set-coords node.element x0 y0 x1 y1))

(defmacro callback (f element)
  `(make-callback f: ,f element: ,element))

(defun callback (f element)
  (new-callback element f))

;; A bordering node allowing space around a content element

(defobject border
    (element
     (left 0)
     (right 0)
     (top 0)
     (bottom 0)))

(defmethod set-coords (node x0 y0 x1 y1) (border? node)
  (set-coords node.element
              (+ x0 node.left)
              (+ y0 node.top)
              (- x1 node.right)
              (- y1 node.bottom)))

(defun border (&rest args)
  (let ((i 0)
        (any false)
        (left undefined)
        (right undefined)
        (top undefined)
        (bottom undefined))
    (labels ((current () (aref args i))
             (next () (incf i) (aref args (1- i))))
      (do () ((not (find (current) '(left: right: top: bottom:))))
        (case (next)
          (left: (setf left (next)))
          (right: (setf right (next)))
          (top: (setf top (next)))
          (bottom: (setf bottom (next))))
        (setf any true))
      (when (keyword? (current))
        (error ~"Invalid keyword {(current)} for border layout element"))
      (unless any
        (let ((x (next)))
          (setf left x)
          (setf right x)
          (setf top x)
          (setf bottom x)))
      (unless (= i (1- (length args)))
        (error "Exactly a single child node is expected"))
      (make-border element: (next)
                   left: left
                   right: right
                   top: top
                   bottom: bottom))))

;; An horizontal or vertical sequence of optionally spaced elements.
;; Each element can have a minimum and maximum size, and has a class
;; and a weight. Elements with higher class are given precedence in
;; space allocation, getting space in proportion to relative weights.
;; Spacing is inherited if not specified and affects all sub-elements.

(defvar *spacing* 0)

(defobject hv-element
    (element
     (min 0)
     (max infinity)
     (class 1)
     (weight 100)))

(defobject hv
    (elements            ;; List of hv-element
     (algorithm :H:)     ;; or :V:
     (spacing 0)))

(defmethod add-element-method (node args) (hv? node)
  (let ((i 0)
        (min undefined)
        (max undefined)
        (class undefined)
        (weight undefined))
    (labels ((current () (aref args i))
             (next () (aref args (1- (incf i)))))
      (do () ((not (find (current) '(min: max: range: size: class: weight:))))
        (case (next)
          (min: (setf min (next)))
          (max: (setf max (next)))
          (range: (setf min (next)) (setf max (next)))
          (size: (setf min (setf max (next))))
          (class: (setf class (next)))
          (weight: (setf weight (next)))))
      (unless (= i (1- (length args)))
        (error "A single element is expected"))
      (push (new-hv-element (next) min max class weight)
            node.elements))))

(defmethod set-coords (node x0 y0 x1 y1) (hv? node)
  (let* ((epsilon 0.0001)
         (elements node.elements)
         (num-elements (length elements))
         (spacing (or node.spacing *spacing*))
         (algo node.algorithm)
         (*spacing* spacing))
    (when (> num-elements 0)
      (let* ((assigned (map (get .min) elements))
             (active (filter (lambda (i) (< (aref assigned i)
                                            (aref elements i).max))
                             (range num-elements)))
             (avail (- (if (= algo :H:)
                           (- x1 x0)
                           (- y1 y0))
                       (reduce #'+ assigned)
                       (* (1- num-elements) spacing))))
        (do () ((or (zero? (length active))
                    (<= avail epsilon))
                  (if (= algo :H:)
                      (let ((x x0))
                        (dolist ((w c) (zip assigned elements))
                          (set-coords c.element x y0 (+ x w) y1)
                          (incf x (+ w spacing))))
                      (let ((y y0))
                        (dolist ((h c) (zip assigned elements))
                          (set-coords c.element x0 y x1 (+ y h))
                          (incf y (+ h spacing))))))
          ;;
          ;; Algorithm:
          ;;
          ;; First select the highest priority class among all active
          ;; nodes, then try to distribute the available space in
          ;; proportion to weights but not exceeding the maximum for a
          ;; given node.  Finally remove saturated nodes from the
          ;; active list.
          ;;
          ;; At every step at least one node is saturated, or all
          ;; available space is distributed.
          ;;
          ;; IOW we're not going to loop forever (unless there's a
          ;; numeric precision problem, that's why epsilon is used).
          ;;
          (let* ((minclass (apply #'min (map (lambda (i)
                                               (aref elements i).class)
                                             active)))
                 (selected (filter (lambda (i) (= (aref elements i).class
                                                  minclass))
                                   active))
                 (selected-nodes (map (lambda (i) (aref elements i))
                                      selected))
                 (total-weight (reduce #'+ (map (get .weight)
                                                selected-nodes)))
                 (share (/ avail total-weight)))
            (dolist (i selected)
              (let* ((n (aref elements i))
                     (quota (min (- n.max (aref assigned i))
                                 (* share n.weight))))
                (decf avail quota)
                (incf (aref assigned i) quota)))
            (setf active (filter (lambda (i)
                                   (< (+ (aref assigned i) epsilon)
                                      (aref elements i).max))
                                 active))))))))

(defun hv-parser (algorithm args)
  (let ((i 0)
        (min undefined)
        (max undefined)
        (class undefined)
        (weight undefined)
        (spacing undefined)
        (elements (list)))
    (labels ((current () (aref args i))
             (next () (incf i) (aref args (1- i))))
      (when (= (current) spacing:)
        (next)
        (setf spacing (next)))
      (do () ((= i (length args)))
        (case (current)
          (min:
             (next)
             (setf max undefined)
             (setf min (next)))
          (max:
             (next)
             (setf min undefined)
             (setf max (next)))
          (range:
             (next)
             (setf min (next))
             (setf max (next)))
          (size:
             (next)
             (setf min (setf max (next))))
          (class:
             (next)
             (setf weight undefined)
             (setf min undefined)
             (setf max undefined)
             (setf class (next)))
          (weight:
             (next)
             (setf min undefined)
             (setf max undefined)
             (setf weight (next)))
          (otherwise
             (when (keyword? (current))
               (error ~"Invalid keyword {(current)} for H/V layout element"))
             (push (new-hv-element (next)
                                   min
                                   max
                                   class
                                   weight)
                   elements))))
      (new-hv elements
              algorithm
              spacing))))

(defun H (&rest args) (hv-parser :H: args))
(defun V (&rest args) (hv-parser :V: args))

;; A collection of elements each placed starting top-left and moving
;; to the right until an element doesn't fit, changing row when this
;; happens.  Each element has a fixed width and height and there is an
;; optional horizontal and vertical spacing.
;;
;; If an element is too wide to be placed it will be placed anyway at
;; the beginning of its own line.
;;
;; [y1] parameter of a set-coords call to a flow node is ignored.

(defobject flow-element
    (element
     width               ;; Fixed width
     height))            ;; Fixed height

(defobject flow
    (elements            ;; List of flow-element
     (h-spacing 0)       ;; Space between elements on a row
     (v-spacing 0)))     ;; Space between rows

(defmethod add-element-method (node args) (flow? node)
  (let ((i 0)
        (width undefined)
        (height undefined))
    (labels ((current () (aref args i))
             (next () (aref args (1- (incf i)))))
      (do () ((not (find (current) '(width: height:))))
        (case (next)
          (width: (setf width (next)))
          (height: (setf height (next)))))
      (unless (= i (1- (length args)))
        (error "A single element is expected"))
      (push (new-flow-element (next) width height)
            node.elements))))

(defmethod set-coords (node x0 y0 x1 y1) (flow? node)
  (let ((x x0)
        (y y0)
        (row-height 0))
    (dolist (c node.elements)
      (when (and (> x x0)
                 (> (+ x c.width) x1))
        (incf y (+ row-height node.v-spacing))
        (setf row-height 0)
        (setf x x0))
      (set-coords c.element x y (+ x c.width) (+ y c.height))
      (setf row-height (max row-height c.height))
      (incf x (+ c.width node.h-spacing)))))

(defun flow (&rest args)
  (let ((i 0)
        (width undefined)
        (height undefined)
        (h-spacing undefined)
        (v-spacing undefined)
        (elements (list)))

    (labels ((current () (aref args i))
             (next () (incf i) (aref args (1- i))))
      (do () ((not (find (current) '(spacing: h-spacing: v-spacing:))))
        (case (next)
          (spacing: (setf h-spacing (setf v-spacing (next))))
          (h-spacing: (setf h-spacing (next)))
          (v-spacing: (setf v-spacing (next)))))
      (do () ((= i (length args)))
        (case (current)
          (size: (next) (setf width (setf height (next))))
          (width: (next) (setf width (next)))
          (height: (next) (setf height (next)))
          (otherwise
             (when (keyword? (current))
               (error ~"Invalid keyword {(current)} for flow layout element"))
             (push (new-flow-element (next)
                                     width
                                     height)
                   elements))))
      (new-flow elements
                h-spacing
                v-spacing))))

;; A bidimensional grid of elements.  [rows] and [columns] are lists
;; of hv-element used to hold the parameters for row and column
;; position allocation.

(defobject table
    (elements            ;; Bi-dimensional matrix
     columns             ;; hv of type :H:
     rows))              ;; hv of type :V:

(defmethod set-coords (node x0 y0 x1 y1) (table? node)
  (let ((col-pos (list))
        (row-pos (list))
        (columns node.columns)
        (rows node.rows))
    (unless rows
      (setf rows (apply #'V (range (length node.elements)))))
    (unless columns
      (setf columns (apply #'H (range (length (aref node.elements 0))))))
    ;; Compute rows and columns layout
    (let ((h (make-hv elements: (map (lambda (c)
                                       (make-hv-element
                                        element: (new-callback
                                                  null
                                                  (lambda (x0 y0 x1 y1)
                                                    (push (list x0 x1) col-pos)))
                                        min: c.min
                                        max: c.max
                                        class: c.class
                                        weight: c.weight))
                                     columns.elements)
                      algorithm: :H:
                      spacing: columns.spacing))
          (v  (make-hv elements: (map (lambda (c)
                                        (make-hv-element
                                         element: (new-callback
                                                   null
                                                   (lambda (x0 y0 x1 y1)
                                                     (push (list y0 y1) row-pos)))
                                         min: c.min
                                         max: c.max
                                         class: c.class
                                         weight: c.weight))
                                      rows.elements)
                       algorithm: :V:
                       spacing: rows.spacing)))
      (set-coords h x0 y0 x1 y1)
      (set-coords v x0 y0 x1 y1))

    ;; Fix elements
    (enumerate (row (ya yb) row-pos)
      (enumerate (col (xa xb) col-pos)
        (set-coords (aref node.elements row col) xa ya xb yb)))))

(defun table (&rest args)
  (let ((i 0)
        (columns undefined)
        (rows undefined)
        (elements (list)))
    (labels ((current () (aref args i))
             (next () (incf i) (aref args (1- i))))
      (do () ((not (find (current) '(columns: rows:))))
        (case (next)
          (columns: (setf cols (next)))
          (rows: (setf rows (next)))))
      (unless (= i (1- (length args)))
        (error "Table needs exactly one data element"))
      (new-table (next)
                 columns
                 rows))))

(defun add-element (node &rest args)
  (add-element-method node args))

(export set-coords add-element
        callback border flow H V table add-element
        hv-element hv-element? new-hv-element make-hv-element
        flow-element flow-element? new-flow-element make-flow-element)