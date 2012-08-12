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

(defun show-window (w)
  "Displays the specified window"
  (show w.frame)
  (when w.resize-cback
    (let ((client w.client))
      (w.resize-cback client.offsetLeft
                      client.offsetTop
                      (+ client.offsetLeft
                         client.offsetWidth)
                      (+ client.offsetTop
                         client.offsetHeight)))))

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

(defun text-area (caption)
  "Creates a multiline input field with specified [caption]"
  (let ((input (create-element "textarea"))
        (label (create-element "div"))
        (container (create-element "div")))
    (set-style container
               position "absolute")
    (set-style label
               %/fontSize 80
               fontWeight "bold")
    (set-style input
               %/fontSize 110
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
    (setf label.textContent caption)
    (append-child container label)
    (append-child container input)
    container))

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
  (when node.layout
    (set-coords node.layout x0 y0 x1 y1)))

(defun dom (element &optional layout)
  (new-dom element layout))

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
                  ((canvas (let ((c (create-element "canvas")))
                             (set-style c
                                        position "absolute"
                                        cursor "default")
                             c))
                   (dark (let ((c (create-element "canvas")))
                           (set-style c
                                      position "absolute"
                                      cursor "pointer"
                                      pointerEvents "none")
                           c))
                   (cursor (let ((c (create-element "canvas")))
                             (set-style c
                                        position "absolute"
                                        cursor "pointer"
                                        pointerEvents "none")
                             c))
                   (vcursor (let ((c (create-element "canvas")))
                              (set-style c
                                         position "absolute"
                                         cursor "pointer"
                                         pointerEvents "none")
                              c))
                   (ok (button "OK" (lambda ()
                                      (funcall cback color)
                                      (hide-window w))))
                   (cancel (button "Cancel" (lambda ()
                                              (funcall cback null)
                                              (hide-window w)))))
                  (border 8
                    (V spacing: 8
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

(export set-style
        element-pos event-pos relative-pos
        show hide
        set-handler
        tracking dragging
        dom
        window
        ask-color
        show-window hide-window with-window
        button
        radio checkbox checked set-checked
        input text set-text
        text-area
        group
        static-text
        select selection set-selection)
