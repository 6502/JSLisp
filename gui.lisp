(import * from graphics)

(defmacro set-style (element &rest properties)
  "Allows settings multiple style properties for a DOM node, example:[[
     (set-style mynode
                position \"absolute\"
                px/left 0
                px/top  0
                px/width 200
                px/height 300)
]]
  The [px/] prefix means a CSS unit that will be appended to the expression."
  (let ((elstyle (gensym)))
    `(let ((,elstyle (. ,element style)))
       ,@(map (lambda (i)
                (let* ((prop (aref properties i))
                       (value (aref properties (1+ i)))
                       (pname (symbol-name prop))
                       (um (index "/" pname)))
                  (if (= um -1)
                      `(setf (. ,elstyle ,prop) ,value)
                      `(setf (. ,elstyle ,(intern (slice pname (1+ um))))
                             (+ ,value ,(slice pname 0 um))))))
              (range 0 (length properties) 2))
       ,elstyle)))

(defun element-pos (x)
  "Returns [(left top)] position of specified DOM element."
  (let ((left 0) (top 0))
    (do ()
        ((or (null? x)
             (undefined? (. x offsetParent)))
         (list left top))
      (incf left (. x offsetLeft))
      (incf top (. x offsetTop))
      (setf x (. x offsetParent)))))

(defun event-pos (event)
  "Returns [(x y)] absolute position of the specified mouse event."
  (let ((x 0) (y 0))
    (cond
      ((or (. event pageX) (. event pageY))
       (setf x (. event pageX))
       (setf y (. event pageY)))
      ((or (. event clientX) (. event clientY))
       (setf x (+ (. event clientX)
                  (. document body scrollLeft)
                  (. document documentElement scrollLeft)))
       (setf y (+ (. event clientY)
                  (. document body scrollTop)
                  (. document documentElement scrollTop)))))
    (list x y)))

(defun show (x)
  "Displays the specified DOM element by adding it to document body"
  (append-child (. document body) x))

(defun hide (x)
  "Removes the specified DOM element from its parent (hiding it)"
  (remove-child (. x parentNode) x))

(defmacro set-handler (element event &rest body)
  "Sets an event handler. Example:[[
     (set-handler mywidget onmousedown
                  (display ~\"Mouse pressed at {(event-pos event)}\"))
]]"
  `(setf (. ,element ,event) (lambda (,#"event") ,@body)))

(defun tracking (f &optional end cursor)
  "Starts tracking mouse movements with calls to [f] until mouseup and then call [end]"
  (let ((cover (create-element "div")))
    (set-style cover
               position "absolute"
               zIndex 999999999
               cursor cursor
               px/left 0
               px/top 0
               px/right 0
               px/bottom 0
               opacity 0.001
               backgroundColor "#000000")
    (set-handler cover oncontextmenu
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation)))
    (set-handler cover onmousemove
                 (funcall (. event preventDefault))
                 (apply f (event-pos event)))
    (set-handler cover onmouseup
                 (hide cover)
                 (when end
                   (apply end (event-pos event))))
    (show cover)))

(defun dragging (div x0 y0)
  "Starts dragging an absolute DOM element starting from position [(x0, y0)]"
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
  (size null)              ;; Min and max override if specified
  (buddy null)             ;; Someone to inform about the size
  (algorithm :H)           ;; Algorithm for children
  (border 0)               ;; Fixed border around space for children
  (spacing null)           ;; Fixed space between children (if null use *spacing*)
  (children (list)))       ;; List of children nodes (if any)

(defun set-coords (node x0 y0 x1 y1)
  "Sets the coordinates of a layout node, possibly triggering recomputation of nested nodes"
  (let* ((children (layout-node-children node))
         (nchild (length children))
         (border (layout-node-border node))
         (spacing (layout-node-spacing node))
         (algo (layout-node-algorithm node)))
    (when (null? spacing)
      (setf spacing *spacing*))
    (when (layout-node-buddy node)
      (funcall (layout-node-buddy node) x0 y0 x1 y1))
    (when (> nchild 0)
      (labels ((lmin (x) (or (layout-node-size x)
                             (layout-node-min x)))
               (lmax (x) (or (layout-node-size x)
                             (layout-node-max x))))
        (let ((assigned (map #'lmin children))
              (active (filter (lambda (i) (< (lmin (aref children i))
                                             (lmax (aref children i))))
                              (range nchild)))
              (avail (- (if (= algo :H)
                            (- x1 x0)
                            (- y1 y0))
                        (* 2 border)
                        (* (1- nchild) spacing))))
          (decf avail (reduce #'+ assigned))
          (do () ((or (zero? (length active))
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
            (let* ((minclass (apply #'min (map (lambda (i)
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
                       (quota (min (- (lmax n) (aref assigned i))
                                   (* share (layout-node-weight n)))))
                  (decf avail quota)
                  (incf (aref assigned i) quota)))
              (setf active (filter (lambda (i)
                                     (< (+ (aref assigned i) 0.0001)
                                        (lmax (aref children i))))
                                   active)))))))))

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
                  (not (symbol? (aref args i)))
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

(defstruct window
  frame            ;; DOM node
  titlebar         ;; DOM node
  resizer          ;; DOM node
  closer           ;; DOM node
  close-cback      ;; invoked after closing
  resize-cback     ;; invoked after resizing
  client           ;; DOM node
  data)            ;; Opaque payload

(defun window (x0 y0 w h &key title client (close true) (resize true))
  "Creates an initially invisible window object"
  (let ((frame (create-element "div"))
        (titlebar (create-element "div"))
        (resizer (create-element "canvas"))
        (closer (create-element "canvas"))
        (window null))
    (unless client
      (setf client (create-element "div")))
    (set-style frame
               position "absolute"
               px/left x0
               px/top y0
               px/width w
               px/height h
               borderRadius "4px"
               backgroundColor "#FFFFFF"
               boxShadow "5px 5px 10px rgba(0,0,0,0.5)"
               border "solid 1px #000000")
    (set-style titlebar
               display (if (undefined? title) "none" "block")
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
    (setf (. titlebar innerHTML) title)
    (append-child frame titlebar)
    (set-handler titlebar onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (append-child (. document body) frame)
                 (dragging frame
                           (first (event-pos event))
                           (second (event-pos event))))
    (set-style client
               position "absolute"
               px/left 0
               px/top (if (undefined? title) 0 20)
               px/width w
               px/height (- h
                            (if (undefined? title) 0 20)
                            (if resize 12 0))
               overflow "auto")
    (append-child frame client)
    (set-style resizer
               display (if resize "block" "none")
               position "absolute"
               px/right 0
               px/bottom 0
               px/width 12
               px/height 12
               cursor "se-resize")
    (setf (. resizer width) 12)
    (setf (. resizer height) 12)
    (with-canvas resizer
      (line-width 1)
      (dolist (i (list 0 5))
        (stroke-style "#000000")
        (line 10 i i 10)
        (stroke-style "#FFFFFF")
        (line 10 (1+ i) (1+ i) 10)))
    (append-child frame resizer)
    (set-handler resizer onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (append-child (. document body) frame)
                 (let ((x0 (first (event-pos event)))
                       (y0 (second (event-pos event))))
                   (tracking (lambda (x y)
                               (let ((dx (- x x0))
                                     (dy (- y y0)))
                                 (set-style frame
                                            px/width (+ (. frame clientWidth) dx)
                                            px/height (+ (. frame clientHeight) dy))
                                 (setf x0 x)
                                 (setf y0 y))
                               (set-style (window-client window)
                                          px/width (. frame clientWidth)
                                          px/height (- (. frame clientHeight)
                                                       (if (undefined? title) 0 20)
                                                       (if resize 12 0)))
                               (when (window-resize-cback window)
                                 (funcall (window-resize-cback window)
                                          (. client offsetLeft)
                                          (. client offsetTop)
                                          (+ (. client offsetLeft)
                                             (. client clientWidth))
                                          (+ (. client offsetTop)
                                             (. client clientHeight))))))))
    (set-style closer
               display (if close "block" "none")
               position "absolute"
               px/right 2
               px/top 2
               px/width 16
               px/height 16
               cursor "default")
    (setf (. closer width) 16)
    (setf (. closer height) 16)
    (with-canvas closer
      (stroke-style "#000000")
      (line-width 1)
      (rect 0 0 16 16)
      (stroke)
      (stroke-style "#FFFFFF")
      (line-width 2)
      (line 4 4 12 12)
      (line 12 4 4 12))
    (append-child frame closer)
    (set-handler closer onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (when (window-close-cback window)
                   (funcall (window-close-cback window)))
                 (hide frame))
    (setf window (make-window :frame frame
                              :titlebar titlebar
                              :resizer resizer
                              :closer closer
                              :resize-cback null
                              :close-cback null
                              :client client))
    window))

(defun hide-window (w)
  "Closes the specified window"
  (when (window-close-cback w)
    (funcall (window-close-cback w)))
  (hide (window-frame w)))

(defun show-window (w)
  "Displays the specified window"
  (show (window-frame w))
  (when (window-resize-cback w)
    (let ((client (window-client w)))
      (funcall (window-resize-cback w)
               (. client offsetLeft)
               (. client offsetTop)
               (+ (. client offsetLeft)
                  (. client offsetWidth))
               (+ (. client offsetTop)
                  (. client offsetHeight))))))

(defun button (text action)
  "Creates a button DOM object with provided [text] and callback [action]"
  (let ((button (create-element "input")))
    (setf (. button type) "button")
    (setf (. button value) text)
    (set-style button
               position "absolute")
    (setf (. button onclick) (lambda (&rest args) (funcall action)))
    button))

(defun checkbox (caption &optional action)
  "Creates a checkbox DOM object with provided [caption] ad an optional callback [action]"
  (let ((checkbox (create-element "input"))
        (text (create-element "span"))
        (container (create-element "label")))
    (setf checkbox.type "checkbox")
    (setf text.textContent caption)
    (set-style container
               position "absolute")
    (append-child container checkbox)
    (append-child container text)
    (when action
      (set-handler checkbox onchange
                   (funcall action)))
    container))

(defun radio (group caption &optional action)
  "Creates a radio button DOM object with specified logical [group], the provided [caption] ad an optional callback [action]"
  (let ((radio (create-element "input"))
        (text (create-element "span"))
        (container (create-element "label")))
    (setf radio.type "radio")
    (setf radio.name group)
    (setf text.textContent caption)
    (set-style container
               position "absolute")
    (append-child container radio)
    (append-child container text)
    (when action
      (set-handler radio onchange
                   (funcall action)))
    container))

(defun checked (checkbox/radio)
  "Returns current state of a [checkbox/radio]"
  checkbox/radio.firstChild.checked)

(defun set-checked (checkbox/radio value)
  "Sets the state of a [checkbox/radio]"
  (setf checkbox/radio.firstChild.checked value))

(defun input (caption)
  "Creates an input field with specified [caption]"
  (let ((input (create-element "input"))
        (label (create-element "div"))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style label
               %/fontSize 80
               fontWeight "bold")
    (set-style input
               %/width 100
               %/fontSize 110
               border "none"
               px/padding 1
               px/margin 0
               backgroundColor "#EEEEEE")
    (setf label.textContent caption)
    (setf input.type "text")
    (append-child container label)
    (append-child container input)
    container))

(defun text (input)
  "Returns current content of a text [input]"
  input.lastChild.value)

(defun set-text (input value)
  "Sets content of a text [input] to a new [value]"
  (setf input.lastChild.value value))

(defun select (caption values)
  "Creates an select field with specified [caption] and list of [values]"
  (let ((select (create-element "select"))
        (label (create-element "div"))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style label
               %/fontSize 80
               fontWeight "bold")
    (set-style select
               %/width 100
               %/fontSize 110
               border "none"
               px/padding 0
               px/margin 0
               backgroundColor "#EEEEEE")
    (setf label.textContent caption)
    (dolist (x values)
      (let ((item (create-element "option")))
        (setf item.textContent x)
        (append-child select item)))
    (append-child container label)
    (append-child container select)
    container))

(defun selection (select)
  "Current selected value of specified [select]"
  (aref select.lastChild.options
        select.lastChild.selectedIndex).value)

(defun set-selection (select value)
  "Sets selected [value] of specified [select]"
  (let ((ix (index value (map (lambda (option) option.value)
                              select.lastChild.options))))
    (setf select.lastChild.selectedIndex ix)))

(defun group (&optional title)
  "A group of related fields with an optional [title]"
  (let ((group (create-element "div"))
        (caption (when title (create-element "span"))))
    (set-style group
               position "absolute"
               border "solid 1px #CCCCCC"
               pointerEvents "none"
               px/borderRadius 4)
    (when title
      (set-style caption
                 %/fontSize 80
                 fontWeight "bold"
                 backgroundColor "#FFFFFF"
                 position "relative"
                 px/paddingLeft 4
                 px/paddingRight 4
                 px/left 10
                 px/top -11)
      (setf caption.textContent title)
      (append-child group caption))
    group))

(defmacro with-window ((var options widgets layout) &rest body)
  "Evaluates [body] forms by first binding [var] to a new window object"
  `(let* ((,var (window ,@options))
          ,@widgets
          (layout ,layout))
     (set-style (window-client ,var)
                overflow "hidden")
     ,@(map (lambda (w)
              `(append-child (window-client ,var) ,(first w)))
            widgets)
     (setf (window-resize-cback ,var)
           (lambda (x0 y0 x1 y1)
             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
     ,@body))

(export set-style
        element-pos event-pos
        show hide
        set-handler
        tracking dragging
        layout-node "layout-node-" "set-layout-node-"
        set-coords
        window "window-" "set-window-"
        show-window hide-window with-window
        button
        radio checkbox checked set-checked
        input text set-text
        group
        select selection set-selection)
