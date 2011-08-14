(defmacro set-style (element &rest properties)
  (let ((el (gensym))
        (props (list))
        (vars (list)))
    (push `(,el ,element) vars)
    (do ((i 0 (+ i 2)))
        ((>= i (length properties))
         `(let (,@vars) ,@props))
      (let ((property (aref properties i))
            (value (aref properties (1+ i))))
        (unless (or (stringp value)
                    (numberp value)
                    (symbolp value))
          (let ((var (gensym)))
            (push `(,var ,value) vars)
            (setf value var)))
        (let ((cmd (if (= (substr (symbol-name property) 0 3) "px/")
                       `(setf (. ,el style ,(intern (substr (symbol-name property) 3)))
                              (+ ,value "px"))
                       `(setf (. ,el style ,property) ,value))))
          (push (if (symbolp value)
                    `(unless (undefinedp ,value) ,cmd)
                    cmd)
                props))))))

(defun show (x)
  (append-child (. document body) x))

(defun hide (x)
  (remove-child (. x parentNode) x))

(defmacro set-handler (element event &rest body)
  `(setf (. ,element ,event) (lambda (event) ,@body)))

(defun tracking (f)
  (let ((cover (create-element "div")))
    (set-style cover
               position "absolute"
               px/left 0
               px/top 0
               px/right 0
               px/bottom 0
               opacity 0.001
               backgroundColor "#000000")
    (set-handler cover onmousemove
                 (funcall (. event preventDefault))
                 (funcall f
                          (. event clientX)
                          (. event clientY)))
    (set-handler cover onmouseup
                 (hide cover))
    (show cover)))

(defun dragging (div x0 y0)
  (tracking (lambda (x y)
              (let  ((dx (- x x0))
                     (dy (- y y0)))
                (set-style div
                           px/left (+ (. div offsetLeft) dx)
                           px/top (+ (. div offsetTop) dy))
                (setf x0 x)
                (setf y0 y)))))

(defvar *spacing* 0)       ;; Spacing is inherited to a layout node subtree

(defstruct layout-node
  (class 1) (weight 100)   ;; Weighted distribution when in the same class
  (min 0) (max 1000000)    ;; Minimum and maximum space
  (buddy null)             ;; Someone to inform about the size
  (algorithm :H)           ;; Algorithm for children
  (border 0)               ;; Fixed border around space for children
  (spacing null)           ;; Fixed space between children (if null use *spacing*)
  (children (list)))       ;; List of children nodes (if any)

(defun set-coords (node x0 y0 x1 y1)
  (let* ((children (layout-node-children node))
         (nchild (length children))
         (border (layout-node-border node))
         (spacing (layout-node-spacing node))
         (algo (layout-node-algorithm node)))
    (when (nullp spacing)
      (setf spacing *spacing*))
    (when (layout-node-buddy node)
      (funcall (layout-node-buddy node) x0 y0 x1 y1))
    (when (> nchild 0)
      (let ((assigned (map #'layout-node-min children))
            (active (filter (lambda (i) (< (layout-node-min (aref children i))
                                           (layout-node-max (aref children i))))
                            (range nchild)))
            (avail (- (if (= algo :H)
                          (- x1 x0)
                          (- y1 y0))
                      (* 2 border)
                      (* (1- nchild) spacing))))
        (decf avail (reduce #'+ assigned))
        (do () ((or (zerop (length active))
                    (<= avail 0.0001))
                  (if (= algo :H)
                      (let ((x (+ x0 border))
                            (ya (+ y0 border))
                            (yb (- y1 border)))
                        (dotimes (i nchild)
                          (set-coords (aref children i)
                                      x ya
                                      (+ x (aref assigned i)) yb)
                          (incf x (+ (aref assigned i) spacing))))
                      (let ((y (+ y0 border))
                            (xa (+ x0 border))
                            (xb (- x1 border))
                            (*spacing* spacing))
                        (dotimes (i nchild)
                          (set-coords (aref children i)
                                      xa y
                                      xb (+ y (aref assigned i)))
                          (incf y (+ (aref assigned i) spacing))))))
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
          ;; We're not going to loop forever.
          ;;
          (let* ((minclass (min (map (lambda (i)
                                       (layout-node-class (aref children i)))
                                     active)))
                 (selected (filter (lambda (i) (= (layout-node-class (aref children i))
                                                  minclass))
                                   active))
                 (selected-nodes (map (lambda (i) (aref children i))
                                      selected))
                 (total-weight (reduce #'+ (map #'layout-node-weight selected-nodes)))
                 (share (/ avail total-weight)))
            (dolist (i selected)
              (let* ((n (aref children i))
                     (quota (min (list (- (layout-node-max n) (aref assigned i))
                                       (* share (layout-node-weight n))))))
                (decf avail quota)
                (incf (aref assigned i) quota)))
            (setf active (filter (lambda (i)
                                   (< (+ (aref assigned i) 0.0001)
                                      (layout-node-max (aref children i))))
                                 active))))))))

(defmacro deflayout (type)
  (let ((x0 (gensym))
        (y0 (gensym))
        (x1 (gensym))
        (y1 (gensym))
        (vdiv (gensym)))
    `(progn
       (defmacro ,type (&rest args)
         (do ((i 0 (+ i 2)))
             ((or (= i (length args))
                  (not (symbolp (aref args i)))
                  (/= (aref (symbol-name (aref args i)) 0) ":"))
                `(make-layout-node :algorithm ,,type
                                   ,@(slice args 0 i)
                                   :children (list ,@(slice args i))))))
       (defmacro ,(intern (+ (symbol-name type) "div")) (div &rest args)
         `(let ((,',vdiv ,div))
            (,,type :buddy (lambda (,',x0 ,',y0 ,',x1 ,',y1)
                             (set-style ,',vdiv
                                        px/left ,',x0
                                        px/top ,',y0
                                        px/width (- ,',x1 ,',x0)
                                        px/height (- ,',y1 ,',y0)))
                    ,@args))))))

(deflayout :H)
(deflayout :V)

(defun window (x0 y0 w h &key title close layout)
  (let ((window (create-element "div")))
    (set-style window
               position "absolute"
               px/left x0
               px/top y0
               px/width w
               px/height h
               borderRadius "4px"
               backgroundColor "#FFFFFF"
               border "solid 1px #000000")

    (unless (undefinedp title)
      (let ((title-bar (create-element "div")))
        (set-style title-bar
                   position "absolute"
                   px/left 0
                   px/top 0
                   px/right 0
                   px/height 20
                   borderTopLeftRadius "4px"
                   borderTopRightRadius "4px"
                   backgroundColor "#6389b7"
                   borderBottom "1px solid #000000"
                   color "#FFFFFF"
                   fontFamily "Arial"
                   px/fontSize 16
                   fontWeight "bold"
                   textAlign "center"
                   cursor "move")
        (setf (. title-bar innerHTML) title)
        (append-child window title-bar)
        (set-handler title-bar onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (append-child (. document body) window)
                     (dragging window
                               (. event clientX)
                               (. event clientY)))))

    (unless (undefinedp layout)
      (let ((resizer (create-element "canvas")))
        (set-style resizer
                   position "absolute"
                   px/right 0
                   px/bottom 0
                   px/width 12
                   px/height 12
                   cursor "se-resize")
        (setf (. resizer width) 12)
        (setf (. resizer height) 12)
        (let ((ctx (funcall (. resizer getContext) "2d")))
          (setf (. ctx strokeStyle) "#000000")
          (setf (. ctx lineWidth) 1.0)
          (dolist (i '(0 5))
            (funcall (. ctx moveTo) 10 i)
            (funcall (. ctx lineTo) i 10))
          (funcall (. ctx stroke)))
        (append-child window resizer)
        (set-handler resizer onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (append-child (. document body) window)
                     (let ((x0 (. event clientX))
                           (y0 (. event clientY)))
                       (tracking (lambda (x y)
                                   (let ((dx (- x x0))
                                         (dy (- y y0)))
                                     (set-style window
                                                px/width (+ (. window clientWidth) dx)
                                                px/height (+ (. window clientHeight) dy))
                                     (setf x0 x)
                                     (setf y0 y))
                                   (set-coords layout
                                               0
                                               (if (undefinedp title) 0 20)
                                               (. window clientWidth)
                                               (. window clientHeight))))))))

    (unless (undefinedp close)
      (let ((closer (create-element "canvas")))
        (set-style closer
                   position "absolute"
                   px/right 2
                   px/top 2
                   px/width 16
                   px/height 16
                   cursor "default")
        (setf (. closer width) 16)
        (setf (. closer height) 16)
        (let ((ctx (funcall (. closer getContext) "2d")))
          (setf (. ctx strokeStyle) "#000000")
          (setf (. ctx lineWidth) 1.0)
          (funcall (. ctx strokeRect) 0 0 16 16)
          (setf (. ctx strokeStyle) "#FFFFFF")
          (setf (. ctx lineWidth) 2.0)
          (funcall (. ctx moveTo) 4 4)
          (funcall (. ctx lineTo) 12 12)
          (funcall (. ctx moveTo) 12 4)
          (funcall (. ctx lineTo) 4 12)
          (funcall (. ctx stroke)))
        (append-child window closer)
        (set-handler closer onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (hide window)
                     (funcall close))))
    window))

(defun button (text action)
  (let ((button (create-element "input")))
    (setf (. button type) "button")
    (setf (. button value) text)
    (set-style button
               position "absolute")
    (setf (. button onclick) action)
    button))

(let ((disp-div (create-element "div"))
      (window (create-element "div"))
      (number ""))
  (labels ((update () (setf (. disp-div innerHTML)
                            (+ "<table width=100%><tr><td valign=center align=center>"
                               number "<font color=\"#FF0000\">|</font>"
                               "</td></tr></table>")))
           (key (k action &key (weight 100))
             (unless action
               (setf action (lambda ()
                              (setf number (+ number k))
                              (update))))
             (:Hdiv (append-child window (button k action))
                    :weight weight))
           (back ()
             (when (> (length number) 0)
               (setf number (substr number 0 (1- (length number)))))
             (update))
           (call ()
             (display (+ "Calling " number))))
    (set-style disp-div
               position "absolute"
               backgroundColor "#000000"
               color "#00FF00"
               fontFamily "Arial"
               fontWeight "bold"
               px/fontSize 20
               textAlign "right")
    (append-child window disp-div)
    (let* ((layout (:V :border 4 :spacing 4
                       (:Hdiv disp-div :min 30 :max 30)
                       (:H :weight 75
                           (key "C" #'back)
                           (key "Call" #'call :weight 200))
                       (:H (key "1") (key "2") (key "3"))
                       (:H (key "4") (key "5") (key "6"))
                       (:H (key "7") (key "8") (key "9"))
                       (:H (key "#") (key "0") (key "*"))))
           (frame (window 100 100 200 300
                          :title "Dialing pad"
                          :close (lambda ())
                          :layout layout)))
      (append-child frame window)
      (update)
      (set-coords layout 0 20 200 300)
      (show frame))))
