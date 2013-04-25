(import * from graphics)
(import * from layout)
(import * from locale)

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
  (let ((el '#.(gensym))
        (elstyle '#.(gensym)))
    `(let* ((,el ,element)
            (,elstyle (. ,el style)))
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
       ,el)))

(defun screen-width ()
  "Returns current width of browser window"
  (js-code "window").innerWidth)

(defun screen-height ()
  "Returns current height of browser window"
  (js-code "window").innerHeight)

(defun element-pos (x)
  "Returns [(left top)] position of specified DOM element."
  (let ((left 0) (top 0))
    (do ()
        ((or (null? x)
             (undefined? x.offsetParent))
           (list left top))
      (incf left x.offsetLeft)
      (incf top x.offsetTop)
      (setf x x.offsetParent))))

(defun event-pos (event)
  "Returns [(x y)] absolute position of the specified mouse event."
  (let ((x 0) (y 0))
    (cond
      ((or event.pageX event.pageY)
       (setf x event.pageX)
       (setf y event.pageY))
      ((or event.clientX event.clientY)
       (setf x (+ event.clientX
                  document.body.scrollLeft
                  document.documentElement.scrollLeft))
       (setf y (+ event.clientY
                  document.body.scrollTop
                  document.documentElement.scrollTop))))
    (list x y)))

(defun relative-pos (event element)
  "Returns [(x y)] relative position of specified mouse [event] in respect to [element]"
  (let (((ex ey) (event-pos event))
        ((cx cy) (element-pos element)))
    (list (- ex cx) (- ey cy))))

(defun show (x)
  "Displays the specified DOM element by adding it to document body"
  (append-child document.body x))

(defun hide (x)
  "Removes the specified DOM element from its parent (hiding it)"
  (remove-child x.parentNode x))

(defmacro set-handler (element event &rest body)
  "Sets an event handler. The return value can be used in {{unset-handler}} \
   to deactivate the installed handler. Example:[[
   (set-handler mywidget onmousedown
     (display ~\"Mouse pressed at {(event-pos event)}\"))
   ]]"
  (unless (= (slice (symbol-name event) 0 2) "on")
    (error "Event name must start with 'on'"))
  (let ((elm '#.(gensym))
        (handler '#.(gensym)))
  `(labels ((,handler (,#"event")
                      (declare (ignorable ,#"event"))
                      ,@body))
     (let ((,elm ,element))
       ((. ,elm addEventListener) ,(slice (symbol-name event) 2)
        #',handler)
       (list ,elm ,(slice (symbol-name event) 2) #',handler)))))

(defun unset-handler (x)
  "Removes an event handler installed with {{set-handler}}."
  (let (((elm event handler) x))
    (elm.removeEventListener event handler)))

(defun tracking (f &optional end cursor zero)
  "Starts tracking mouse movements with calls to [f] until mouseup and \
   then call [end] if specified. The parameter [cursor] is the shape \
   that the cursor should use (e.g. \"move\") and the parameter [zero] \
   if specified is an [(x y)] point to be use as zero for the coordinates."
  (let ((cover (set-style (create-element "div")
                          position "absolute"
                          zIndex 999999999
                          cursor cursor
                          px/left 0
                          px/top 0
                          px/right 0
                          px/bottom 0
                          opacity 0.001
                          backgroundColor "#000000"))
        (zx (if zero (first zero) 0))
        (zy (if zero (second zero) 0)))
    (labels ((call (f event)
                   (event.preventDefault)
                   (event.stopPropagation)
                   (when f
                     (let (((xx yy) (event-pos event)))
                       (funcall f (- xx zx) (- yy zy)))))
             (ignore (event)
                     (event.preventDefault)
                     (event.stopPropagation))
             (move (event)
                   (event.preventDefault)
                   (event.stopPropagation)
                   (call f event))
             (up (event)
                 (event.preventDefault)
                 (event.stopPropagation)
                 (document.removeEventListener "contextmenu" #'ignore true)
                 (document.removeEventListener "mousemove" #'move true)
                 (document.removeEventListener "mouseup" #'up true)
                 (hide cover)
                 (call end event)))
      (document.addEventListener "contextmenu" #'ignore true)
      (document.addEventListener "mousemove" #'move true)
      (document.addEventListener "mouseup" #'up true)
      (show cover))))

(defun dragging (div x0 y0)
  "Starts dragging an absolute DOM element starting from position [(x0, y0)]"
  (tracking (lambda (x y)
              (let  ((dx (- x x0))
                     (dy (- y y0)))
                (set-style div
                           px/left (+ div.offsetLeft dx)
                           px/top (+ div.offsetTop dy))
                (setf x0 x)
                (setf y0 y)))))

;; Modal gray screen

(defun modal-screen ()
  "Creates, displays and returns a modal translucent screen that blocks mouse clicks"
  (let ((modal (set-style (create-element "div")
                          position "absolute"
                          px/left 0
                          px/top 0
                          px/right 0
                          px/bottom 0
                          backgroundColor "rgba(0,0,0,0.25)")))
    (set-handler modal onmousedown
                 (event.preventDefault)
                 (event.stopPropagation))
    (show modal)
    modal))

;;

(defmacro def-accessor (class name field)
  `(progn
     (defmethod ,name (widget) (= widget.% ,class)
       ,field)
     (defmethod ,#"set-{name}" (widget ,name) (= widget.% ,class)
       (setf ,field ,name))))

(defun node (widget)
  "Returns the HTML5 DOM node associated to [widget] if it's not the widget itself"
  (or (and widget widget.node) widget))

;; A gui window object

(defobject window
    (frame            ;; DOM node
     titlebar         ;; DOM node
     resizer          ;; DOM node
     closer           ;; DOM node
     close-cback      ;; invoked after closing
     resize-cback     ;; invoked after resizing
     client           ;; DOM node
     data))           ;; Opaque payload

(defun window (x0 y0 w h &key title client (close true) (resize true))
  "Creates an initially invisible window object"
  (when (< x0 1) (setf x0 (* x0 (screen-width))))
  (when (< y0 1) (setf y0 (* y0 (screen-height))))
  (when (< w 1) (setf w (* w (screen-width))))
  (when (< h 1) (setf h (* h (screen-height))))
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
    (setf titlebar.innerHTML title)
    (append-child frame titlebar)
    (set-handler titlebar onmousedown
                 (event.preventDefault)
                 (event.stopPropagation)
                 (append-child document.body frame)
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
    (setf resizer.width 12)
    (setf resizer.height 12)
    (with-canvas resizer
      (line-width 1)
      (dolist (i (list 0 5))
        (stroke-style "#000000")
        (line 10 i i 10)
        (stroke-style "#FFFFFF")
        (line 10 (1+ i) (1+ i) 10)))
    (append-child frame resizer)
    (set-handler resizer onmousedown
                 (event.preventDefault)
                 (event.stopPropagation)
                 (append-child document.body frame)
                 (let ((x0 (first (event-pos event)))
                       (y0 (second (event-pos event)))
                       (disp (append-child document.body (set-style (create-element "div")
                                                                    position "absolute"
                                                                    backgroundColor "rgba(0,0,0,0.75)"
                                                                    px/padding 4
                                                                    px/borderRadius 4
                                                                    color "#FFF"
                                                                    fontFamily "sans-serif"
                                                                    px/fontSize 16
                                                                    fontWeight "bold"))))
                   (tracking (lambda (x y)
                               (let ((dx (- x x0))
                                     (dy (- y y0)))
                                 (set-style frame
                                            px/width (+ frame.clientWidth dx)
                                            px/height (+ frame.clientHeight dy))
                                 (setf disp.textContent ~"{frame.clientWidth} x {frame.clientHeight}")
                                 (set-style disp
                                            px/left (+ 20 x)
                                            px/top (+ 20 y))
                                 (setf x0 x)
                                 (setf y0 y))
                               (set-style window.client
                                          px/width frame.clientWidth
                                          px/height (- frame.clientHeight
                                                       (if (undefined? title) 0 20)
                                                       (if resize 12 0)))
                               (when window.resize-cback
                                 (window.resize-cback client.offsetLeft
                                                      client.offsetTop
                                                      (+ client.offsetLeft
                                                         client.clientWidth)
                                                      (+ client.offsetTop
                                                         client.clientHeight))))
                             (lambda ()
                               (remove-child document.body disp)))))
    (set-style closer
               display (if close "block" "none")
               position "absolute"
               px/right 2
               px/top 2
               px/width 16
               px/height 16
               cursor "default")
    (setf closer.width 16)
    (setf closer.height 16)
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
                 (event.preventDefault)
                 (event.stopPropagation)
                 (when window.close-cback
                   (window.close-cback))
                 (hide frame))

    (setf window (make-window frame: frame
                              titlebar: titlebar
                              resizer: resizer
                              closer: closer
                              resize-cback: null
                              close-cback: null
                              client: client))
    window))

(defun hide-window (w)
  "Closes the specified window"
  (when w.close-cback
    (w.close-cback))
  (hide w.frame))

(defun screen-width ()
  "Returns the width of the usable area in the browser"
  (js-code "window").innerWidth)

(defun screen-height ()
  "Returns the height of the usable area in the browser"
  (js-code "window").innerHeight)

(defun show-window (w &key center modal)
  "Displays the specified window"
  (let ((modal (when modal (modal-screen))))
    (show w.frame)

    (when center
      (set-style w.frame
                 px/left (/ (- (screen-width) w.frame.offsetWidth) 2)
                 px/top (/ (- (screen-height) w.frame.offsetHeight) 2)))
    (when w.resize-cback
      (let ((client w.client))
        (w.resize-cback client.offsetLeft
                        client.offsetTop
                        (+ client.offsetLeft
                           client.offsetWidth)
                        (+ client.offsetTop
                           client.offsetHeight))))
    (when modal
      (let ((ocb w.close-cback))
        (setf w.close-cback
              (lambda ()
                (setf w.close-cback ocb)
                (hide modal)
                (when ocb (funcall ocb))))))))

;;

(defun button (text action &key (default (= text "OK")) (cancel (= text "Cancel")))
  "Creates a button DOM object with provided [text] and callback [action]"
  (let ((button (create-element "input")))
    (setf button.type "button")
    (setf button.value text)
    (set-style button
               position "absolute")
    (set-handler button onclick
      (funcall action))
    (setf button.% #'button)
    (setf button.default default)
    (setf button.cancel cancel)
    button))

(def-accessor #'button caption widget.value)

(defun check-default-actions (container event)
  (cond
    ((= event.which 13)
     (dolist (x container.children)
       (when (and (= x.% #'button) x.default)
         ((node x).click))))
    ((= event.which 27)
     (dolist (x container.children)
       (when (and (= x.% #'button) x.cancel)
         ((node x).click)))))
  true)

(defmacro lbutton (text &rest body)
  "Syntactic sugar for simple buttons with inlined actions"
  `(button ,text (lambda () ,@body)))

(defun static-text (content)
  "Creates a static text object"
  (let ((text (create-element "div")))
    (set-style text
               position "absolute")
    (setf text.textContent content)
    (setf text.% #'static-text)
    text))

(def-accessor #'static-text text widget.textContent)

(defun checkbox (caption &optional action)
  "Creates a checkbox DOM object with provided [caption] ad an optional callback [action]"
  (let ((checkbox (create-element "input"))
        (text (set-style (create-element "span")
                         fontFamily "Arial"
                         px/fontSize 16))
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
    (setf container.% #'checkbox)
    (setf container.node checkbox)
    container))

(def-accessor #'checkbox caption widget.lastChild.textContent)
(def-accessor #'checkbox action widget.firstChild.onchange)
(def-accessor #'checkbox checked widget.firstChild.checked)

(defun radio (group caption &optional action)
  "Creates a radio button DOM object with specified logical [group], the provided [caption] ad an optional callback [action]"
  (let ((radio (create-element "input"))
        (text (set-style (create-element "span")
                         fontFamily "Arial"
                         px/fontSize 16))
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
    (setf container.% #'radio)
    (setf container.node radio)
    container))

(def-accessor #'radio caption widget.lastChild.textContent)
(def-accessor #'radio action widget.firstChild.onchange)
(def-accessor #'radio checked widget.firstChild.checked)

(defun label (txt)
  (let ((label (create-element "div")))
    (setf label.textContent txt)
    (set-style label
               fontFamily "sans-serif"
               %/fontSize 80
               color "#666666"
               whiteSpace "pre"
               fontWeight "bold")
    (setf label.% #'label)
    label))

(def-accessor #'label caption widget.textContent)

(defun input (caption &key (autofocus false) (autoselect true))
  "Creates an input field with specified [caption]"
  (let ((input (create-element "input"))
        (label (label caption))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style input
               %/width 100
               %/fontSize 110
               fontWeight "bold"
               border "none"
               px/padding 1
               px/margin 0
               backgroundColor "#EEEEEE")
    (setf input.type "text")
    (append-child container label)
    (append-child container input)

    (set-handler input onkeydown
      (check-default-actions container.parentNode event))

    (if autoselect
        (set-handler input onfocus
          (input.setSelectionRange 0 (length input.value))))
    (setf container.% #'input)
    (setf container.node input)
    (setf input.autofocus autofocus)
    container))

(def-accessor #'input caption widget.firstChild.textContent)
(def-accessor #'input text widget.lastChild.value)

(defun input-with-help (caption helper)
  "Creates an input field with specified [caption] and an helper button"
  (let ((input (create-element "input"))
        (label (label caption))
        (help (create-element "input"))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style input
               position "absolute"
               %/fontSize 110
               fontWeight "bold"
               border "none"
               px/padding 1
               px/margin 0
               backgroundColor "#EEEEEE")
    (set-style help
               position "absolute"
               px/width 30)
    (setf help.type "button")
    (setf help.value "?")
    (setf input.type "text")
    (append-child container label)
    (append-child container help)
    (append-child container input)
    (setf container."data-resize"
          (lambda (x0 y0 x1 y1)
            (let ((ly (+ label.offsetTop label.offsetHeight)))
              (set-style input
                         px/left 0
                         px/top ly
                         px/width (- x1 x0 25)
                         px/height (- y1 y0 ly))
              (set-style help
                         px/left (- x1 x0 25 -2)
                         px/top ly
                         px/width 25
                         px/height (- y1 y0 ly -2)))))
    (set-handler help onclick
                 (funcall helper container))
    (setf help.tabIndex -1)
    (set-handler input onkeydown
                 (when (= event.which 112)
                   (event.preventDefault)
                   (event.stopPropagation)
                   (funcall helper container)))
    (setf container.% #'input-with-help)
    (setf container.node input)
    container))

(def-accessor #'input-with-help text widget.lastChild.value)
(def-accessor #'input-with-help caption widget.firstChild.textContent)
(def-accessor #'input-with-help action widget.firstChild.nextSibling.onclick)

(defun text-area (caption)
  "Creates a multiline input field with specified [caption]"
  (let ((input (create-element "textarea"))
        (label (label caption))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style input
               %/fontSize 110
               fontWeight "bold"
               position "absolute"
               px/left 0
               px/top 16
               border "none"
               px/padding 4
               px/margin 0
               resize "none"
               backgroundColor "#EEEEEE")
    ;; Textarea HTML specs is badly broken and there is
    ;; no way to specify using style that the textarea
    ;; should take all space of the container because
    ;; using of bottom:0 and right:0 should not work.
    ;; Works in a reasonable way in chrome/safari, but
    ;; this is indeed a "bug" of chrome/safari.
    ;; Workaround is adding a resize callback that will
    ;; be called if present after resizing a div in
    ;; dom layout managers.
    ;; There is indeed an "onresize" event, but works only
    ;; on the window object and not on other elements
    ;; (except in IE, where it works on any element).
    (setf (aref container "data-resize")
          (lambda ()
            (set-style input
                       px/width (- container.offsetWidth 8)
                       px/height (- container.offsetHeight 16 8))))
    (append-child container label)
    (append-child container input)
    (setf container.% #'text-area)
    (setf container.node input)
    container))

(def-accessor #'text-area text widget.lastChild.value)
(def-accessor #'text-area caption widget.firstChild.textContent)

(defun select (caption values)
  "Creates an select field with specified [caption] and list of [values]"
  (let ((select (create-element "select"))
        (label (label caption))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style select
               %/width 100
               %/fontSize 110
               fontWeight "bold"
               border "none"
               px/padding 0
               px/margin 0
               backgroundColor "#EEEEEE")
    (dolist (x values)
      (let ((item (create-element "option")))
        (setf item.textContent x)
        (append-child select item)))
    (append-child container label)
    (append-child container select)
    (setf container.% #'select)
    (setf container.node select)
    (set-handler select onkeydown
      (check-default-actions container.parentNode event))
    container))

(def-accessor #'select caption widget.firstChild.textContent)

(defmethod text (widget) (= widget.% #'select)
  (aref (node widget).options
        (node widget).selectedIndex).value)

(defmethod set-text (widget text) (= widget.% #'select)
  (let ((ix (index text (map (get .value) (node widget).options))))
    (setf (node widget).selectedIndex ix)
    (if (>= ix 0) text null)))

(defun group (&optional title)
  "A group of related fields with an optional [title]"
  (let ((group (create-element "div"))
        (caption (create-element "span")))
    (set-style group
               position "absolute"
               border "solid 1px #CCCCCC"
               pointerEvents "none"
               px/borderRadius 4)
    (set-style caption
               fontFamily "sans-serif"
               whiteSpace "pre"
               %/fontSize 80
               color "#666666"
               backgroundColor "#FFFFFF"
               position "relative"
               px/left 10
               px/top -11)
    (setf caption.textContent (if title ~" {title} " ""))
    (append-child group caption)
    (setf group.% #'group)
    group))

(def-accessor #'group caption widget.caption)

;; Layout node for DOM elements

(defobject dom (element layout x0 y0 x1 y1))

(defmethod set-coords (node x0 y0 x1 y1) (dom? node)
  (set-style node.element
             px/left (round x0)
             px/top (round y0)
             px/width (round (- x1 x0))
             px/height (round (- y1 y0)))
  (setf node.x0 x0)
  (setf node.y0 y0)
  (setf node.x1 x1)
  (setf node.y1 y1)
  (when node.element."data-resize"
    (node.element."data-resize" x0 y0 x1 y1))
  (if node.layout
      (set-coords node.layout x0 y0 x1 y1)
      (list x0 y0 x1 y1)))

(defun dom (element &optional layout)
  (setf element."data-layout-node"
        (new-dom element layout)))

(defun dom-replace (element new-element)
  "Replaces an [element] that has been wrapped in a [dom] \
   layout node with a [new-element]. See {{dom}}."
  (let ((layout element."data-layout-node"))
    (append-child element.parentNode new-element)
    (setf layout.element new-element)
    (setf new-element."data-layout-node" layout)
    (set-coords layout layout.x0 layout.y0 layout.x1 layout.y1)
    (hide element)
    new-element))

;; Splitters

(defun setsplit (w0 container layout min max)
  (cond
    ((< w0 (/ min 2))
     (setf w0 0))
    ((< w0 min)
     (setf w0 min))
    ((> w0 (/ (+ max 100) 2))
     (setf w0 100))
    ((>= w0 max)
     (setf w0 max)))
  (setf (first layout.elements).weight w0)
  (set-style (first layout.elements).element.element
             display (if (= w0 0) "none" null))
  (setf (last layout.elements).weight (- 100 w0))
  (set-style (last layout.elements).element.element
             display (if (= w0 100) "none" null))
  (set-coords layout 0 0
              container.offsetWidth
              container.offsetHeight))

(defun h-splitter (a b &key (split 50) (min 10) (max 90))
  (let ((container (create-element "div"))
        (splitter (create-element "div")))
    (append-child container a)
    (append-child container b)
    (append-child container splitter)
    (set-style splitter
               position "absolute"
               cursor "move"
               backgroundColor "#DDDDDD")
    (let ((layout (H spacing: 2
                     weight: split
                     (dom a)
                     size: 8
                     (dom splitter)
                     weight: (- 100 split)
                     (dom b))))
      (set-handler splitter onmousedown
        (event.preventDefault)
        (event.stopPropagation)
        (tracking (lambda (x y)
                    (declare (ignorable y))
                    (let ((w container.offsetWidth))
                      (decf x (first (element-pos container)))
                      (setsplit (/ x w 0.01) container layout min max)))
                  (lambda ())
                  "move"))
      (setf container."data-resize"
            (lambda (x0 y0 x1 y1)
              (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
      (setf container.partition
            (lambda (w) (setsplit w container layout min max))))
    container))

(defun v-splitter (a b &key (split 50) (min 10) (max 90))
  (let ((container (create-element "div"))
        (splitter (create-element "div")))
    (append-child container a)
    (append-child container b)
    (append-child container splitter)
    (set-style container
               position "absolute")
    (set-style splitter
               position "absolute"
               cursor "move"
               backgroundColor "#DDDDDD")
    (let ((layout (V spacing: 2
                     weight: split
                     (dom a)
                     size: 8
                     (dom splitter)
                     weight: (- 100 split)
                     (dom b))))
      (set-handler splitter onmousedown
        (event.preventDefault)
        (event.stopPropagation)
        (tracking (lambda (x y)
                    (declare (ignorable x))
                    (let ((h container.offsetHeight))
                      (decf y (second (element-pos container)))
                      (setsplit (/ y h 0.01) container layout min max)))
                  (lambda ())
                  "move"))
      (setf container."data-resize"
            (lambda (x0 y0 x1 y1)
              (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
      (setf container.partition
            (lambda (w) (setsplit w container layout min max))))
    container))

;; Table widget

(defun table (data &key cols rows row-click cell-click)
  (let ((cells (list))
        (table (create-element "div")))
    (dolist (row data)
      (let ((rowcells (list)))
        (push (list) cells)
        (dolist (col row)
          (let ((cell col))
            (when (or (number? cell)
                      (string? cell))
              (setf cell (create-element "div"))
              (set-style cell
                         overflow "hidden"
                         textAlign "center"
                         position "absolute"
                         cursor "default"
                         backgroundColor "#EEEEEE")
              (setf cell.textContent col))
            (append-child table cell)
            (push cell rowcells)
            (cond
              (row-click
               (set-handler cell onmousedown
                            (event.preventDefault)
                            (event.stopPropagation)
                            (funcall row-click row rowcells)))
              (cell-click
               (set-handler cell onmousedown
                            (event.preventDefault)
                            (event.stopPropagation)
                            (funcall cell-click col cell))))
            (push (dom cell) (last cells))))))
    (let ((layout (V border: 4
                     (tablayout columns: cols
                                rows: rows
                                cells))))
      (set-style table
                 position "absolute"
                 backgroundColor "#CCCCCC"
                 overflow "hidden")
      (setf table."data-resize"
            (lambda ()
              (setf table.style.overflow "hidden")
              (let* ((w table.offsetWidth)
                     (h table.offsetHeight)
                     ((x0 y0 x1 y1) (set-coords layout 0 0 w h)))
                (when (or (< x0 0) (> x1 w) (< y0 0) (> y1 h))
                  (setf table.style.overflow "auto")
                  (setf w table.clientWidth)
                  (setf h table.clientHeight)
                  (set-coords layout 0 0 w h)))))
      (setf table.% #'table)
      table)))

;; Tree view

(defun tree-view (tree &key onclick (text-of (get .text)) (children-of (get .children)) (closed-nodes (list)))
  (let** ((container (set-style (create-element "div")
                                backgroundColor "#EEEEEE"
                                overflow "auto"))
          (select-nodes null)
          (select-places null)
          (#'rebuild ()
            (do () ((not container.firstChild))
              (remove-child container container.firstChild))
            (let** ((#'add (n depth)
                      (let ((row (create-element "div")))
                        (set-style row
                                   px/paddingLeft (+ 4 depth)
                                   px/paddingTop 2
                                   px/paddingBottom 2
                                   px/fontSize 16
                                   cursor "default"
                                   fontFamily "monospace"
                                   fontWeight "bold")
                        (set-handler row onmouseover
                          (when select-nodes
                            (set-style row backgroundColor "#FF0000")))
                        (set-handler row onmouseout
                          (set-style row backgroundColor ""))
                        (set-handler row onmousedown
                          (event.preventDefault)
                          (event.stopPropagation)
                          (cond
                            (select-nodes
                              (let ((f select-nodes))
                                (setf select-nodes null)
                                (funcall f n)))
                            (select-places)
                            (onclick
                              (funcall onclick n)))
                          false)
                        (let ((expander (create-element "span")))
                          (set-style expander
                                     cursor "default"
                                     backgroundColor "#FFFFFF"
                                     px/paddingLeft 3
                                     px/paddingRight 3
                                     px/marginRight 9
                                     border "solid 1px #000000")
                          (setf expander.textContent
                                (if (find n closed-nodes) "+" "-"))
                          (set-handler expander onmousedown
                            (event.preventDefault)
                            (event.stopPropagation)
                            (if (find n closed-nodes)
                                (nremove n closed-nodes)
                                (push n closed-nodes))
                            (rebuild)
                            false)
                          (append-child row expander))
                        (append-child row (document.createTextNode (funcall text-of n)))
                        (append-child container row)
                        (let** ((#'place (parent index d)
                                  (let ((place (set-style (create-element "div")
                                                          px/marginLeft d
                                                          px/height 4)))
                                    (set-handler place onmouseover
                                      (when select-places
                                        (set-style place backgroundColor "#FF0000")))
                                    (set-handler place onmouseout
                                      (set-style place backgroundColor ""))
                                    (set-handler place onmousedown
                                      (event.preventDefault)
                                      (event.stopPropagation)
                                      (when select-places
                                        (let ((f select-places))
                                          (setf select-places null)
                                          (funcall f parent index)))
                                      false)
                                    place)))
                          (unless (find n closed-nodes)
                            (append-child container (place n 0 (+ depth 28)))
                            (enumerate (i c n.children)
                              (add c (+ depth 28))
                              (when (find c closed-nodes)
                                (append-child container (place n (1+ i) (+ depth 28))))))))))
              (when container.tree
                (add container.tree 0)))))
    (setf container.rebuild #'rebuild)
    (setf container.tree tree)
    (setf container.select-node (lambda (f)
                                  (setf select-nodes f)))
    (setf container.select-place (lambda (f)
                                   (setf select-places f)))
    (rebuild)
    container))

;; Tab control

(defun tabbed ()
  (let** ((tabbed (set-style (create-element "div")
                             position "absolute"
                             fontFamily "Arial"
                             fontWeight "bold"))
          (ribbon (append-child tabbed (set-style (create-element "div")
                                                  position "absolute"
                                                  whiteSpace "pre")))
          (area (append-child tabbed (set-style (create-element "div")
                                                position "absolute"
                                                border "solid 1px #000000"
                                                backgroundColor "#FFFFFF")))
          (layout (V spacing: 0
                     size: 22 (dom ribbon)
                     size: undefined (dom area)))
          (tabs (list))
          (pages (list))
          (current -1)
          (#'current ()
            (aref pages current))
          (#'add (label page &optional (close false))
            (let ((tab (set-style (create-element "span")
                                  display "inline-block"
                                  position "relative"
                                  px/height 20
                                  px/top 1
                                  borderLeft "solid 1px #000000"
                                  borderRight "solid 1px #000000"
                                  borderTop "solid 1px #000000"
                                  borderBottom "solid 1px #000000"
                                  backgroundColor "#DDDDDD"
                                  px/borderTopLeftRadius 6
                                  px/borderTopRightRadius 6
                                  textAlign "center"
                                  px/marginRight 8
                                  px/paddingLeft 8
                                  px/paddingRight 8
                                  cursor "pointer")))
              (when (string? label)
                (let ((lab (create-element "span")))
                  (setf lab.textContent label)
                  (setf label lab)))
              (append-child tab label)
              (when close
                (let ((closer (set-style (create-element "span")
                                         color "#FF0000"
                                         fontWeight "normal"
                                         cursor "pointer")))
                  (setf closer.innerHTML "&nbsp;×")
                  (set-handler closer onmousedown
                    (event.stopPropagation)
                    (event.preventDefault)
                    (remove (index tab tabs)))
                  (append-child tab closer)))
              (append-child tabbed tab)
              (push tab tabs)
              (push page pages)
              (set-handler tab onclick
                (select (index tab tabs)))
              (append-child ribbon tab)
              (when (= (length pages) 1)
                (select 0))))
          (#'remove (index)
            (when (or (not (aref pages index).remove?)
                      (funcall (aref pages index).remove?))
              (if (= (length pages) 1)
                  (progn
                    (setf pages (list))
                    (setf tabs (list))
                    (remove-child area area.firstChild)
                    (remove-child ribbon ribbon.firstChild))
                  (progn
                    (when (= index current)
                      (select (% (+ current (1- (length pages)))
                                 (length pages))))
                    (when (< index current)
                      (decf current))
                    (remove-child ribbon (aref tabs index))
                    (splice pages index 1)
                    (splice tabs index 1)))))
          (#'select (index)
            (unless (= index current)
              (when (/= current -1)
                (set-style (aref tabs current)
                           borderBottom "solid 1px #000000"
                           backgroundColor "#DDDDDD")
                (remove-child area (aref pages current)))
              (setf current index)
              (set-style (aref tabs current)
                         borderBottom "solid 1px #FFFFFF"
                         backgroundColor "#FFFFFF")
              (append-child tabbed area)
              (append-child tabbed ribbon)
              (append-child area (aref pages current))
              (when (aref pages current).focus
                ((aref pages current).focus))
              (fix)))
          (#'fix ()
              (set-coords layout 0 0 tabbed.offsetWidth tabbed.offsetHeight)
              (when area.firstChild
                (set-style area.firstChild
                           px/left 8
                           px/top 8
                           px/width (- area.offsetWidth 16)
                           px/height (- area.offsetHeight 16))
                (when area.firstChild."data-resize"
                      (area.firstChild."data-resize"
                                       0 0
                                       area.firstChild.offsetWidth
                                       area.firstChild.offsetHeight)))))
    (setf tabbed.add #'add)
    (setf tabbed.remove #'remove)
    (setf tabbed.select #'select)
    (setf tabbed.current #'current)
    (setf tabbed.next (lambda ()
                        (when (> (length pages) 1)
                          (select (% (1+ current) (length pages))))))
    (setf tabbed.prev (lambda ()
                        (when (> (length pages) 1)
                          (select (% (+ current (1- (length pages))) (length pages))))))
    (setf tabbed.current-index (lambda () current))
    (setf tabbed.page-index (lambda (p) (index p pages)))
    (setf tabbed."data-resize" #'fix)
    (setf tabbed.% #'tabbed)
    tabbed))

(defun add-widget (window widget)
  "Adds a widget to a window client
   This function prepares a widget (a DOM element) for being \
   managed as a control of a window. This means making its \
   positionig \"absolute\" and adding it as a children of \
   the window's client area.
   The call returns as value the passed [widget]."
  (setf widget.style.position "absolute")
  (append-child window.client widget)
  widget)

(defun set-layout (window layout)
  "Sets the [layout] for the content of specified [window].
   The function installs as [resize-callback] for the window \
   a closure that passes the whole client area to the \
   specified layout manager. It also sets the window client \
   overflow property to \"hidden\" assuming that the layout \
   will be able to put all widgets in the provided area.[[
   (let** ((w (window 0 0 400 600 title: \"sub-browser\"))
           (browser (add-widget w (create-element \"iframe\")))
           (close (add-widget w (button \"Close\" #'close)))
           (#'close () (hide-window w)))
     (set-layout w (V spacing: 8 border: 8
                      (dom browser)
                      size: 30 (H :filler:
                                  size: 80 (dom close)
                                  :filler:)))
     (setf browser.src \"http://www.jslisp.org\")
     (show-window w center: true))
   ]]"
  (setf window.client.style.overflow "hidden")
  (setf window.resize-cback
        (lambda (x0 y0 x1 y1)
          (set-coords layout 0 0 (- x1 x0) (- y1 y0)))))

;;

(defmacro with-window ((var options widgets layout) &rest body)
  "Evaluates [body] forms by first binding [var] to a new window object"
  `(let* ((,var (window ,@options))
          ,@widgets
          (layout ,layout))
     (set-style (. ,var client)
                overflow "hidden")
     ,@(map (lambda (w)
              `(append-child (. ,var client) ,(first w)))
            widgets)
     (setf (. ,var resize-cback)
           (lambda (x0 y0 x1 y1)
             (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
     ,@body))

(defun ask-color (x y prompt color cback)
  "Asks modally for a color (initially [color]) and calls [cback] \
   passing the user selection or [null]"
  (let ((was-css? false))
    (when (string? color)
      (setf color (parse-color color))
      (setf was-css? true))
    (let** ((w (window x y 300 320 title: prompt close: false))
            (canvas (add-widget w (set-style (create-element "canvas")
                                             position "absolute"
                                             cursor "default")))
            (dark (add-widget w (set-style (create-element "canvas")
                                           position "absolute"
                                           cursor "pointer"
                                           pointerEvents "none")))
            (cursor (add-widget w (set-style (create-element "canvas")
                                             position "absolute"
                                             cursor "pointer"
                                             pointerEvents "none")))
            (vcursor (add-widget w (set-style (create-element "canvas")
                                              position "absolute"
                                              cursor "pointer"
                                              pointerEvents "none")))
            (ok (add-widget w (lbutton "OK"
                                (funcall cback (if was-css? (css-color color) color))
                                (hide-window w))))
            (cancel (add-widget w (lbutton "Cancel"
                                    (funcall cback null)
                                    (hide-window w))))
            (layout (V spacing: 8 border: 8
                       (dom canvas)
                       size: 30
                       (H null
                          size: 80 (dom ok) (dom cancel)
                          size: undefined
                          null))))
      (dolist (cv (list cursor vcursor))
        (setf cv.width 20)
        (setf cv.height 20)
        (with-canvas cv
          (begin-path)
          (arc 10 10 9 0 (* 2 pi) false)
          (arc 10 10 4 0 (* 2 pi) true)
          (fill-style "#000000")
          (fill)
          (begin-path)
          (arc 10 10 8 0 (* 2 pi) false)
          (arc 10 10 5 0 (* 2 pi) true)
          (fill-style "#FFFFFF")
          (fill)))
      (let ((colv (/ (max color.r color.g color.b)
                     255))
            (colx null)
            (coly null)
            (colerr null)
            (wheel (list)))

        ;; Computing saturated color wheel (1536 elements)
        (dotimes (i 256) (push (list 255 i 0) wheel))          ; R1 G+ B0
        (dotimes (i 256) (push (list (- 255 i) 255 0) wheel))  ; R- G1 B0
        (dotimes (i 256) (push (list 0 255 i) wheel))          ; R0 G1 B+
        (dotimes (i 256) (push (list 0 (- 255 i) 255) wheel))  ; R0 G- B1
        (dotimes (i 256) (push (list i 0 255) wheel))          ; R+ G0 B1
        (dotimes (i 256) (push (list 255 0 (- 255 i)) wheel))  ; R1 G0 B-

        (labels ((geometry ()
                   (let* ((width canvas.offsetWidth)
                          (height canvas.offsetHeight)
                          (r (/ (min width height) 2.25))
                          (cx (- (/ width 2) (/ r 8)))
                          (cy (/ height 2))
                          (bx (+ cx r (/ r 8))))
                     (list width height r cx cy bx)))

                 (fixcursor ()
                   (let (((width height r cx cy bx) (geometry)))
                     (declare (ignorable width height))
                     (set-style cursor
                                px/left (+ canvas.offsetLeft -10 cx (* r colx))
                                px/top (+ canvas.offsetTop -10 cy (* r coly))
                                px/width 20
                                px/height 20)
                     (set-style vcursor
                                px/left (+ canvas.offsetLeft -10 bx)
                                px/top (+ canvas.offsetTop -10 cy r (- (* 2 r colv)))
                                px/width 20
                                px/height 20)
                     (let (((r g b) (findrgb colx coly)))
                       (setf color (rgb (floor (* r colv))
                                        (floor (* g colv))
                                        (floor (* b colv)))))))

                 (findrgb (dx dy)
                   (let* ((d2 (min 1 (+ (* dx dx) (* dy dy))))
                          (ia (floor (* 1536 (/ (atan2 dy dx) pi 2))))
                          ((r g b) (aref wheel (% (+ 1536 ia) 1536)))
                          (k2 (* 255 (- 1 d2)))
                          (ir (floor (+ (* r d2) k2)))
                          (ig (floor (+ (* g d2) k2)))
                          (ib (floor (+ (* b d2) k2))))
                     (list ir ig ib)))

                 (redraw ()
                   (let (((width height r cx cy bx) (geometry)))
                     (setf canvas.width width)
                     (setf canvas.height height)
                     (setf dark.width width)
                     (setf dark.height height)
                     (set-style dark
                                px/left canvas.offsetLeft
                                px/top canvas.offsetTop
                                px/width width
                                px/height height
                                opacity (- 1 colv))
                     (with-canvas canvas
                       (rect (- bx 2) (- cy r) 4 (* 2 r))
                       (fill-style "#A0A0A0")
                       (fill))
                     (let* ((ctx (canvas.getContext "2d"))
                            (idata (ctx.getImageData 0 0 width height))
                            (data idata.data)
                            (dark-ctx (dark.getContext "2d"))
                            (dark-idata (dark-ctx.getImageData 0 0 width height))
                            (dark-data dark-idata.data)
                            (wp 0)
                            (bestx null)
                            (besty null))
                       (dotimes (y height)
                         (let ((dy (/ (- y cy) r)))
                           (dotimes (x width)
                             (let* ((dx (/ (- x cx) r))
                                    ((ir ig ib) (findrgb dx dy)))
                               (when (<= (+ (* dx dx) (* dy dy)) 1)
                                 (when (null? colx)
                                   (let* ((dr (- (* colv ir) color.r))
                                          (dg (- (* colv ig) color.g))
                                          (db (- (* colv ib) color.b))
                                          (e2 (+ (* dr dr) (* dg dg) (* db db))))
                                     (when (or (null? colerr) (< e2 colerr))
                                       (setf colerr e2)
                                       (setf bestx dx)
                                       (setf besty dy))))
                                 (setf (aref data wp) ir)
                                 (setf (aref data (+ wp 1)) ig)
                                 (setf (aref data (+ wp 2)) ib)
                                 (setf (aref data (+ wp 3)) 255)
                                 (setf (aref dark-data wp) 0)
                                 (setf (aref dark-data (+ wp 1)) 0)
                                 (setf (aref dark-data (+ wp 2)) 0)
                                 (setf (aref dark-data (+ wp 3)) 255)))
                             (incf wp 4))))
                       (when (null? colx)
                         (setf colx bestx)
                         (setf coly besty))
                       (dark-ctx.putImageData dark-idata 0 0)
                       (ctx.putImageData idata 0 0)))))

          (set-handler canvas onmousedown
            (event.stopPropagation)
            (event.preventDefault)
            (let (((width height r cx cy bx) (geometry)))
              (declare (ignorable width height bx))
              (let* (((x y) (event-pos event))
                     ((x0 y0) (element-pos canvas))
                     (handler (if (>= (- x x0) (+ cx r))
                                  (lambda (mx my)
                                    (declare (ignorable mx my))
                                    (let ((nv (/ (- (+ cy r) (- my y0)) (* 2 r))))
                                      (setf colv (max (min nv 1) 0)))
                                    (set-style dark
                                               opacity (- 1 colv))
                                    (fixcursor))
                                  (lambda (mx my)
                                    (setf colx (/ (- mx x0 cx) r))
                                    (setf coly (/ (- my y0 cy) r))
                                    (let* ((d2 (+ (* colx colx) (* coly coly)))
                                           (d (sqrt d2)))
                                      (when (> d 1)
                                        (setf colx (/ colx d))
                                        (setf coly (/ coly d))))
                                    (fixcursor)))))
                (funcall handler x y)
                (tracking handler))))

          (setf w.resize-cback
                (lambda (x0 y0 x1 y1)
                  (set-coords layout 0 0 (- x1 x0) (- y1 y0))
                  (redraw)
                  (fixcursor)))))
      (show-window w modal: true))))

;; Text input for a CSS color
(defun color-helper (input)
  (let (((x y) (element-pos input)))
    (ask-color x (+ y input.offsetHeight)
               "Preferred color"
               (parse-color (text input))
               (lambda (c)
                 (when c
                   (setf (text input) (css-color c)))
                 (set-timeout (lambda () (input.lastChild.focus)) 100)))))

(defun css-color-input (caption)
  "An input field containing a CSS color, with a color picker helper button"
  (let** ((x (input-with-help caption #'color-helper))
          (#' update-style ()
            (let* ((col (parse-color (text x)))
                   (luma (+ col.r (* 2 col.g) col.b)))
              (set-style x.lastChild
                         backgroundColor (css-color col)
                         color (if (< luma 512) "#FFFFFF" "#000000")))
            true))
    ((node x).addEventListener "keyup" #'update-style)
    ((node x).addEventListener "focus" #'update-style)
    ((node x).addEventListener "blur" #'update-style)
    (setf x.update-style #'update-style)
    x))

;; Simple message or question box

(defun message-box (htmltext &key (title "Message")
                                  cback
                                  (buttons (list "OK"))
                                  default
                                  (modal false)
                                  (width (/ (screen-width) 2))
                                  (height (/ (screen-height) 3)))
  (let ((btnlist (list))
        (btnrow (H spacing: 16 :filler:)))
    (with-window (w (0 0 width height title: title)
                    ((message (create-element "div")))
                    (V spacing: 8 border: 8
                       (dom message)
                       size: 30
                       btnrow))
      (setf message.style.position "absolute")
      (setf message.style.overflow "auto")
      (setf message.innerHTML htmltext)
      (dolist (btn-text buttons)
        (let ((b (button btn-text (lambda ()
                                    (hide-window w)
                                    (when cback
                                      (funcall cback btn-text))))))
          (append-child w.client b)
          (push b btnlist)
          (add-element btnrow size: 80 (dom b))))
      (add-element btnrow null)
      (set-timeout (if (find default buttons)
                       (lambda ()
                         ((aref btnlist (index default buttons)).focus))
                       (lambda ()
                         (when document.activeElement
                           (document.activeElement.blur))))
                   10)
      (show-window w center: true modal: modal))))

;; Calendar

(defun ask-date (x y cback selection)
  "Shows a modal calendar widget at position [x] [y] that will \
   call the specified function [cback] with the user \
   selected date object starting as default with specified \
   [selection] or today."
  (let** ((w (window x y 220 220 resize: false close: false))
          (prev (add-widget w (button "<<" #'prev)))
          (go-today (add-widget w (button "" #'today)))
          (current-month (add-widget w (set-style (create-element "div")
                                                  position "absolute"
                                                  textAlign "center"
                                                  fontFamily "Arial"
                                                  px/fontSize 14
                                                  fontWeight "bold")))
          (next (add-widget w (button ">>" #'next)))
          (cells (let ((cells (make-array (list 7 7))))
                   (dotimes (i 7)
                     (dotimes (j 7)
                       (setf (aref cells i j)
                             (set-style (add-widget w (create-element "div"))
                                        position "absolute"
                                        textAlign "center"
                                        cursor "pointer"
                                        backgroundColor (if (= i 0) "#DDDDDD" "#FFFFFF")
                                        px/borderRadius 4))
                       (setf (aref cells i j).textContent "XX")))
                   (let ((c (date 1970 0 4)))
                     (dotimes (j 7)
                       (setf (aref cells 0 j).textContent
                             (first (c.toString)))
                       (c.setDate (1+ (c.getDate)))))
                   (set-style (aref cells 0 0)
                              color "#FF0000")
                   cells))
          (today (date))
          (year ((or selection today).getFullYear))
          (month ((or selection today).getMonth))
          (#'today ()
                   (setf year (today.getFullYear))
                   (setf month (today.getMonth))
                   (recalc))
          (#'next ()
                  (when (= 12 (incf month))
                    (setf month 0)
                    (incf year))
                  (recalc))
          (#'prev ()
                  (when (= -1 (decf month))
                    (setf month 11)
                    (decf year))
                  (recalc))
          (#'recalc ()
                    (let ((d (date year month 1)))
                      (setf go-today.value
                            (+ (aref (split (d.toString) " ") 1)
                               " / "
                               year))
                      (dolist (i (range 1 7))
                        (dotimes (j 7)
                          (setf (aref cells i j).textContent "")
                          (setf (aref cells i j).style.backgroundColor "#FFFFFF")
                          (when (aref cells i j).data-handler
                            (unset-handler (aref cells i j).data-handler))))
                      (let ((x (d.getDay)))
                        (dolist (i (range 1 7))
                          (dolist (j (range x 7))
                            (when (= (d.getMonth) month)
                              (setf (aref cells i j).textContent (d.getDate))
                              (set-style (aref cells i j)
                                         border (if (= (d.toDateString)
                                                       (today.toDateString))
                                                    "solid 1px #FF0000"
                                                    null)
                                         backgroundColor (if (and selection
                                                                  (= (d.toDateString)
                                                                     (selection.toDateString)))
                                                             "#FFFF80"
                                                             "#FFFFFF"))
                              (let ((dd (date (d.getTime))))
                                (setf (aref cells i j).data-handler
                                      (set-handler (aref cells i j) onmousedown
                                        (setf selection dd)
                                        (hide-window w))))
                              (d.setDate (1+ (d.getDate)))))
                          (setf x 0))))))
    (declare (ignorable current-month))
    (set-style w.client
               backgroundColor "#DDDDDD")
    (set-style w.frame
               backgroundColor "#DDDDDD")
    (recalc)
    (let ((layout (V spacing: 4 border: 8
                     (H size: 40 (dom prev)
                        size: undefined (dom go-today)
                        size: 40 (dom next)))))
      (dotimes (i 7)
        (let ((h (H spacing: 4)))
          (dotimes (j 7)
            (add-element h (dom (aref cells i j))))
          (add-element layout h)))
      (set-layout w layout))
    (setf w.close-cback
          (lambda () (funcall cback selection)))
    (show-window w modal: true)))

(defun date-input (caption)
  "Creates an input field for a date with an helper button \
   that shows a calendar."
  (labels ((calendar (input)
             (let ((current (or (parse-date (text input))
                                (date)))
                   ((x y) (element-pos input)))
               (ask-date x (+ y input.offsetHeight)
                         (lambda (d)
                           (when d
                             (setf (text input)
                                   (str-date d))))
                         current))))
    (input-with-help caption #'calendar)))

;; Login/password request

(defun ask-login (cback &key (title "Login")
                             (user-value "")
                             (password-value "")
                             (user-caption "user")
                             (show-password-checkbox false)
                             (width 400)
                             (height (if show-password-checkbox 230 210)))
  "Displays a user/password dialog calling [cback] with provided values on confirm
   or passing [null] values in case the user cancels"
  (with-window (w (0 0 width height title: title close: false)
                  ((user (input user-caption))
                   (password (input "password"))
                   (showpass (if show-password-checkbox
                                 (checkbox "Show password")
                                 (create-element "div")))
                   (ok (button "OK"
                               (lambda ()
                                 (hide-window w)
                                 (funcall cback (text user) (text password)))))
                   (cancel (button "Cancel"
                                   (lambda ()
                                     (hide-window w)
                                     (funcall cback null null)))))
                  (V spacing: 16 border: 16
                     size: 30
                     (dom user)
                     (dom password)
                     size: (if show-password-checkbox 20 0)
                     (dom showpass)
                     :filler:
                     size: 30
                     (H :filler: size: 80 (dom ok) (dom cancel) :filler:)))
    (show-window w modal: true center: true)
    (setf password.lastChild.type "password")
    (setf (text user) user-value)
    (setf (text password) password-value)
    (when show-password-checkbox
      (set-handler showpass onchange
                   (setf password.lastChild.type (if (checked showpass)
                                                     "text"
                                                     "password"))))
    (user.lastChild.focus)))

(defun baloon (html &optional (duration 2000))
  "Shows a message in a baloon that will disappear by itself"
  (let ((baloon (set-style (create-element "div")
                           position "absolute"
                           zIndex 999999998
                           px/padding 16
                           px/borderRadius 8
                           px/right 16
                           px/top 16
                           backgroundColor "rgba(0,0,0,0.75)"
                           color "#FFFFFF"
                           opacity 0
                           fontFamily "monospace"
                           px/fontSize 18
                           px/fontWeight "bold"))
        (animation null)
        (direction 1)
        (start (clock)))
    (setf baloon.innerHTML html)
    (append-child document.body baloon)
    (setf animation
          (set-interval (lambda ()
                          (let ((s (/ (- (clock) start) 500)))
                            (cond
                              ((<= 0 s 1)
                               (set-style baloon opacity (if (= direction 1) s (- 1 s))))
                              ((and (= direction 1) (>= s 1))
                               (set-style baloon opacity 1)
                               (setf direction -1)
                               (setf start (+ (clock) duration)))
                              ((>= s 1)
                               (clear-interval animation)
                               (remove-child document.body baloon)))))
                        20))
    baloon))

(defun menu (options &optional pos)
  (let** ((menu (set-style (create-element "div")
                           position "absolute"
                           zIndex 1000000000
                           px/padding 8
                           border "1px solid #000000"
                           backgroundColor "#FFFFFF"
                           boxShadow "5px 5px 10px rgba(0,0,0,0.5)"))
          (cover (set-style (create-element "div")
                            position "absolute"
                            zIndex 999999999
                            cursor "default"
                            px/left 0
                            px/top 0
                            px/right 0
                            px/bottom 0
                            opacity 0.1
                            backgroundColor "#000000"))
          (#'close ()
            (hide cover)
            (hide menu)))
    (dolist ((text action) options)
      (let ((entry (append-child menu (create-element "div"))))
        (set-style entry
                   cursor "pointer"
                   fontFamily "Arial"
                   px/fontSize 16
                   px/paddingTop 1
                   px/paddingBottom 1
                   px/paddingLeft 8
                   px/paddingRight 8
                   fontWeight "bold")
        (setf entry.textContent text)
        (set-handler entry onmouseover
          (set-style entry
                     color "#FFFFFF"
                     backgroundColor "#000040"))
        (set-handler entry onmouseout
          (set-style entry
                     color null
                     backgroundColor null))
        (set-handler entry onmousedown
                     (event.stopPropagation)
                     (event.preventDefault)
                     (close)
                     (funcall action))
        (set-handler entry onmouseup
                     (event.stopPropagation)
                     (event.preventDefault)
                     (close)
                     (funcall action))))
    (set-handler cover onmousedown
      (event.stopPropagation)
      (event.preventDefault)
      (close))
    (set-handler cover onmouseup
      (event.stopPropagation)
      (event.preventDefault)
      (close))
    (set-handler cover oncontextmenu
      (event.stopPropagation)
      (event.preventDefault)
      (close))
    (set-handler cover onmousemove
      (let (((x y) (event-pos event)))
        (when (< x (- menu.offsetLeft 10))
          (set-style menu px/left (+ x 10)))
        (when (< y (- menu.offsetTop 10))
          (set-style menu px/top (+ y 10)))
        (when (> x (+ menu.offsetLeft menu.offsetWidth 10))
          (set-style menu px/left (- x menu.offsetWidth 10)))
        (when (> y (+ menu.offsetTop menu.offsetHeight 10))
          (set-style menu px/top (- y menu.offsetHeight 10)))))
    (show cover)
    (set-style menu opacity 0.001)
    (show menu)
    (unless pos
      (setf pos (list (/ (- (screen-width) menu.offsetWidth) 2)
                      (/ (- (screen-height) menu.offsetHeight) 2))))
    (set-style menu
               px/left (first pos)
               px/top (second pos))
    (set-style menu opacity 1.0)))

(defun focus (widget)
  "Sets the focus to DOM node of [widget] after 10ms.
   Delaying is necessary because setting the focus in the handler \
   of an event (e.g. a button click) is not going to work."
  (set-timeout (lambda () ((node widget).focus)) 10))

(export set-style
        screen-width screen-height

        element-pos event-pos relative-pos
        show hide
        set-handler unset-handler
        tracking dragging
        dom dom-replace
        window

        show-window hide-window with-window
        add-widget set-layout node focus

        label button lbutton radio checkbox input
        input-with-help date-input text-area group
        css-color-input static-text select
        table tabbed h-splitter v-splitter
        tree-view

        caption set-caption
        text set-text
        action set-action
        checked set-checked

        ask-login ask-date ask-color
        message-box
        baloon
        menu)
