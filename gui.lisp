(import * from graphics)
(import * from layout)

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
  "Sets an event handler. Example:[[
     (set-handler mywidget onmousedown
                  (display ~\"Mouse pressed at {(event-pos event)}\"))
]]"
  `(setf (. ,element ,event) (lambda (,#"event") ,@body)))

(defun tracking (f &optional end cursor)
  "Starts tracking mouse movements with calls to [f] until mouseup and then call [end]"
  (let ((cover (set-style (create-element "div")
                          position "absolute"
                          zIndex 999999999
                          cursor cursor
                          px/left 0
                          px/top 0
                          px/right 0
                          px/bottom 0
                          opacity 0.001
                          backgroundColor "#000000")))
    (set-handler cover oncontextmenu
                 (event.preventDefault)
                 (event.stopPropagation))
    (set-handler cover onmousemove
                 (event.preventDefault)
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
                       (y0 (second (event-pos event))))
                   (tracking (lambda (x y)
                               (let ((dx (- x x0))
                                     (dy (- y y0)))
                                 (set-style frame
                                            px/width (+ frame.clientWidth dx)
                                            px/height (+ frame.clientHeight dy))
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
                                                         client.clientHeight)))))))
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

(defun button (text action)
  "Creates a button DOM object with provided [text] and callback [action]"
  (let ((button (create-element "input")))
    (setf button.type "button")
    (setf button.value text)
    (set-style button
               position "absolute")
    (setf button.onclick (lambda (&rest args) (funcall action)))
    button))

(defun static-text (content)
  "Creates a static text object"
  (let ((text (create-element "div")))
    (set-style text
               position "absolute")
    (setf text.textContent content)
    text))

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

(defun label (txt)
  (let ((label (create-element "div")))
    (setf label.textContent txt)
    (set-style label
               fontFamily "sans-serif"
               %/fontSize 80
               color "#666666"
               whiteSpace "pre"
               fontWeight "bold")))

(defun input (caption)
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
    container))

(defun text (input)
  "Returns current content of a text [input]"
  input.lastChild.value)

(defun set-text (input value)
  "Sets content of a text [input] to a new [value]"
  (setf input.lastChild.value value))

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
            (set-style input
                       px/left 0
                       px/right 30
                       px/top (+ label.offsetTop label.offsetHeight)
                       px/bottom 0)
            (set-style help
                       px/right 0
                       px/top (- (+ label.offsetTop label.offsetHeight) 2)
                       px/bottom -2
                       px/width 30)))
    (set-handler help onclick
                 (funcall helper container))
    (setf help.tabIndex -1)
    (set-handler input onkeydown
                 (when (= event.which 112)
                   (event.preventDefault)
                   (event.stopPropagation)
                   (funcall helper container)))
    container))

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
    container))

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
                 color "#666666"
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

;; Layout node for DOM elements

(defobject dom (element layout))

(defmethod set-coords (node x0 y0 x1 y1) (dom? node)
  (set-style node.element
             px/left x0
             px/top y0
             px/width (- x1 x0)
             px/height (- y1 y0))
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
    (hide element)))

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
      table)))

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

(defun ask-color (prompt color cback)
  "Asks for a color (initially [color]) and calls [cback] passing the selection or [null]"
  (with-window (w (100 100 300 320 title: prompt close: false)
                  ((canvas (set-style (create-element "canvas")
                                      position "absolute"
                                      cursor "default"))
                   (dark (set-style (create-element "canvas")
                                    position "absolute"
                                    cursor "pointer"
                                    pointerEvents "none"))
                   (cursor (set-style (create-element "canvas")
                                      position "absolute"
                                      cursor "pointer"
                                      pointerEvents "none"))
                   (vcursor (set-style (create-element "canvas")
                                       position "absolute"
                                       cursor "pointer"
                                       pointerEvents "none"))
                   (ok (button "OK" (lambda ()
                                      (funcall cback color)
                                      (hide-window w))))
                   (cancel (button "Cancel" (lambda ()
                                              (funcall cback null)
                                              (hide-window w)))))
                  (V spacing: 8 border: 8
                     (dom canvas)
                     size: 30
                     (H null
                        size: 80 (dom ok) (dom cancel)
                        size: undefined
                        null)))
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
                                 (let* ((dr (- ir color.r))
                                        (dg (- ig color.g))
                                        (db (- ib color.b))
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
                       (let* (((x y) (event-pos event))
                              ((x0 y0) (element-pos canvas))
                              (handler (if (>= (- x x0) (+ cx r))
                                           (lambda (mx my)
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
                (fixcursor))))

      (show-window w))))

;; Simple message or question box

(defun message-box (htmltext &key (title "Message")
                                  cback
                                  (buttons (list "OK"))
                                  (modal false)
                                  (width (/ (screen-width) 2))
                                  (height (/ (screen-height) 3)))
  (let ((btnrow (H spacing: 16 :filler:)))
    (with-window (w (0 0 width height title: title)
                    ((message (create-element "div")))
                    (V spacing: 8 border: 8
                       (dom message)
                       :filler:
                       size: 30
                       btnrow))
      (setf message.style.position "absolute")
      (setf message.innerHTML htmltext)
      (dolist (btn-text buttons)
        (let ((b (button btn-text (lambda ()
                                    (hide-window w)
                                    (when cback
                                      (funcall cback btn-text))))))
          (append-child w.client b)
          (add-element btnrow size: 80 (dom b))))
      (add-element btnrow null)
      (show-window w center: true modal: modal))))

;; Login/password request

(defun login (cback &key (title "Login")
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

(export set-style
        element-pos event-pos relative-pos
        show hide
        set-handler
        tracking dragging
        dom dom-replace
        window
        ask-color
        show-window hide-window with-window
        add-widget set-layout
        button
        radio checkbox checked set-checked
        input text set-text
        input-with-help
        text-area
        group
        static-text
        select selection set-selection
        table
        screen-width screen-height
        message-box)
