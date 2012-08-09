(import * from serialize)
(import * from gui)
(import * from graphics)
(import * from rpc-client)
(import * from examples/forms)

(defvar *pages* null)
(defvar *current-page* null)
(defvar *screen* null)
(defvar *canvas* null)
(defvar *selection* null)
(defvar *zx* 170)
(defvar *zy* 50)
(defvar *sf* 0.3)
(defvar *dirty* false)
(defvar *tracking* null)
(defvar *borders* false)
(defvar *grid* false)

(defun hit (e p)
  "Returns a callable tracker if point [p] hit element [e]"
  null)

(defun draw (e)
  "Draws element e"
  null)

(defun fill-rect (x0 y0 x1 y1 color)
  "Fills rectangle ([x0] [y0])-([x1] [y1]) with specified [color]"
  (with-canvas *canvas*
    (fill-style color)
    (begin-path)
    (rect (+ *zx* (* *sf* x0))
          (+ *zy* (* *sf* y0))
          (* *sf* (- x1 x0))
          (* *sf* (- y1 y0)))
    (fill)))

(defun frame (x0 y0 x1 y1 width color)
  "Draws rectangle ([x0] [y0])-([x1] [y1]) with specified [color] and line [width]"
  (if (or (>= (+ x0 width) (- x1 width))
          (>= (+ y0 width) (- y1 width)))
      (fill-rect x0 y0 x1 y1 color)
      (progn
        (fill-rect x0 y0 x1 (+ y0 width) color)
        (fill-rect x0 (+ y0 width) (+ x0 width) (- y1 width) color)
        (fill-rect (- x1 width) (+ y0 width) x1 (- y1 width) color)
        (fill-rect x0 (- y1 width) x1 y1 color))))

(defun update ()
  "Checks if a repaint is needed and eventually updates the screen"
  (when (or *dirty*
            (/= *canvas*.width *screen*.offsetWidth)
            (/= *canvas*.height *screen*.offsetHeight))
    (setf *canvas*.width *screen*.offsetWidth)
    (setf *canvas*.height *screen*.offsetHeight)
    (fill-rect 20 20 (+ 20 *current-page*.width) (+ 20 *current-page*.height) "#A0A0A0")
    (fill-rect 0 0 *current-page*.width *current-page*.height "#FFFFFF")
    (with-canvas *canvas*
      (stroke-style "#C0C0C0")
      (line-width 1)
      (rect *zx* *zy*
            (* *sf* *current-page*.width)
            (* *sf* *current-page*.height))
      (stroke))
    (when *grid*
      (with-canvas *canvas*
        (save)
        (translate *zx* *zy*)
        (scale *sf* *sf*)
        (begin-path)
        (dotimes (i (1+ (floor (/ *current-page*.height 100))))
          (move-to 0 (* i 100)) (line-to *current-page*.width (* i 100)))
        (dotimes (i (1+ (floor (/ *current-page*.width 100))))
          (move-to (* i 100) 0) (line-to (* i 100) *current-page*.height))
        (stroke-style "#E0E0E0")
        (line-width (/ *sf*))
        (stroke)
        (restore)))
    (dolist (x *current-page*.entities)
      (draw x)
      (when *borders*
        (with-canvas *canvas*
          (save)
          (stroke-style "#C0C0C0")
          (line-width 1)
          (rect (+ *zx* (* *sf* x.x0))
                (+ *zy* (* *sf* x.y0))
                (* *sf* (- x.x1 x.x0))
                (* *sf* (- x.y1 x.y0)))
          (stroke)
          (restore))))
    (setf *dirty* false)))

(defobject p2d (x y))
(defun p2d (x y) (new-p2d x y))

;; Rectangle tracking

(defun rect-tracker (e &optional update end)
  "Tracker for object [e] handling drag/resizing and optionally calling function
   [update] during editing and function [end] at mouseup."
  (labels ((wa (a b) (+ (* 0.75 a) (* 0.25 b))))
    (let ((mode 0)
          (p0 null))
      (lambda (p event)
        (case event
          ('down
           (setf mode (logior (if (< p.x (wa e.x0 e.x1)) 1 0)
                              (if (> p.x (wa e.x1 e.x0)) 2 0)
                              (if (< p.y (wa e.y0 e.y1)) 4 0)
                              (if (> p.y (wa e.y1 e.y0)) 8 0)))
           (when (= mode 0)
             (setf mode 15))
           (setf p0 p))
          ('move
           (let ((dx (- p.x p0.x))
                 (dy (- p.y p0.y)))
             (when (logand mode 1) (setf e.x0 (min (- e.x1 5) (+ e.x0 dx))))
             (when (logand mode 2) (setf e.x1 (max (+ e.x0 5) (+ e.x1 dx))))
             (when (logand mode 4) (setf e.y0 (min (- e.y1 5) (+ e.y0 dy))))
             (when (logand mode 8) (setf e.y1 (max (+ e.y0 5) (+ e.y1 dy)))))
           (setf p0 p)
           (when update (funcall update))
           (setf *dirty* true))
          ('up
           (when end (funcall end))))))))

;; Null tracking

(defvar *wait-up* (lambda (p event)))

;; Generic editor

(defobject editor (x0 y0 x1 y1 e propedit))

(defmethod hit (e p) (editor? e)
           (and (<= e.x0 p.x e.x1)
                (<= e.y0 p.y e.y1)
                (let ((moved false))
                  (rect-tracker e.e
                                (lambda ()
                                  (setf moved true)
                                  (setf e.x0 e.e.x0)
                                  (setf e.y0 e.e.y0)
                                  (setf e.x1 e.e.x1)
                                  (setf e.y1 e.e.y1))
                                (lambda ()
                                  (unless moved
                                    (funcall e.propedit)))))))

(defmethod draw (e) (editor? e)
  (fill-rect e.x0 e.y0 e.x1 e.y1
             "rgba(255,0,0,0.25)")
  (with-canvas *canvas*
    (save)
    (translate *zx* *zy*)
    (scale *sf* *sf*)
    (line-width (/ 2 *sf*))
    (stroke-style "#FF0000")
    (rect e.x0 e.y0 (- e.x1 e.x0) (- e.y1 e.y0))
    (stroke)
    (restore)))

(defun remove-editors ()
  (let ((old-len (length *current-page*.entities)))
    (setf *current-page*.entities
          (filter (lambda (x) (not (editor? x)))
                  *current-page*.entities))
    (when (/= old-len (length *current-page*.entities))
      (setf *dirty* true))))

(defun edit (e))

(defun open-editor (e)
  (remove-editors)
  (push (new-editor e.x0 e.y0 e.x1 e.y1 e
                    (lambda () (edit e)))
        *current-page*.entities)
  (setf *dirty* true)
  *wait-up*)

;; Rectangle object

(defmethod edit (e) (rect? e)
  (ask-color "Fill color"
             e.color
             (lambda (color)
               (when color
                 (setf e.color color)
                 (setf *dirty* true)))))

(defmethod hit (e p) (rect? e)
  (and (<= e.x0 p.x e.x1)
       (<= e.y0 p.y e.y1)
       (open-editor e)))

(defmethod draw (e) (rect? e)
  (fill-rect e.x0 e.y0 e.x1 e.y1 (css-color e.color)))

;; Image object

(defmethod edit (e) (image? e)
  (with-window (w (100 100 500 150
                       :title "Image properties")
                  ((url (input "URL"))
                   (ok (button "OK"
                               (lambda ()
                                 (setf e.url (text url))
                                 (remove-key e "img")
                                 (setf *dirty* true)
                                 (hide-window w))))
                   (cancel (button "Cancel"
                                   (lambda ()
                                     (hide-window w)))))
                  (:V :spacing 8 :border 8
                      (:H :size 35
                          (:Hdiv url))
                      (:H)
                      (:H :size 30
                          (:H)
                          (:Hdiv ok :size 80)
                          (:Hdiv cancel :size 80)
                          (:H))))
    (show-window w)
    (setf (text url) e.url)))

(defmethod hit (e p) (image? e)
  (and (<= e.x0 p.x e.x1)
       (<= e.y0 p.y e.y1)
       (open-editor e)))

(defmethod draw (e) (image? e)
  (unless e.img
    (setf e.img (create-element "img"))
    (set-handler e.img onload
                 (setf *dirty* true))
    (setf e.img.src e.url))
  (with-canvas *canvas*
    (save)
    (translate *zx* *zy*)
    (scale *sf* *sf*)
    (image e.img e.x0 e.y0 (- e.x1 e.x0) (- e.y1 e.y0))
    (restore)))

;; Text object

(defmethod edit (e) (text? e)
  (with-window (w (100 100 500 500
                       :title "Text properties")
                  ((family (input "font family"))
                   (size (input "font size"))
                   (bold (checkbox "Bold"))
                   (italic (checkbox "Italic"))
                   (effects (group "Effects"))
                   (color (button "Color" (lambda ()
                                            (ask-color "Text color"
                                                       e.color
                                                       (lambda (color)
                                                         (when color
                                                           (setf e.color color)
                                                           (setf *dirty* true)))))))
                   (content (text-area "content"))
                   (ok (button "OK"
                               (lambda ()
                                 (setf e.text (text content))
                                 (setf e.size (atof (text size)))
                                 (setf e.family (text family))
                                 (setf e.bold (checked bold))
                                 (setf e.italic (checked italic))
                                 (setf *dirty* true)
                                 (hide-window w))))
                   (cancel (button "Cancel"
                                   (lambda ()
                                     (hide-window w)))))
                  (:V :spacing 8 :border 8
                      (:H :size 100
                          (:Hdiv family)
                          (:Hdiv size :size 80)
                          (:Vdiv effects :border 8 :spacing 4 :size 80
                                 (:V :size 3)
                                 (:H :size 20 (:Hdiv bold))
                                 (:H :size 20 (:Hdiv italic))
                                 (:H :size 30 (:Hdiv color))))
                      (:Hdiv content)
                      (:H :size 30
                          (:H)
                          (:Hdiv ok :size 80)
                          (:Hdiv cancel :size 80)
                          (:H))))
    (show-window w)
    (setf (text content) e.text)
    (setf (text size) e.size)
    (setf (text family) e.family)
    (setf (checked bold) e.bold)
    (setf (checked italic) e.italic)))

(defmethod hit (e p) (text? e)
  (and (<= e.x0 p.x e.x1)
       (<= e.y0 p.y e.y1)
       (open-editor e)))

(defmethod draw (e) (text? e)
   (with-canvas *canvas*
     (save)
     (text-baseline "top")
     (font (+ (if e.italic "italic " "")
              (if e.bold "bold " "")
              e.size "px "
              e.family))
     (translate *zx* *zy*)
     (scale *sf* *sf*)
     (rect e.x0 e.y0 (- e.x1 e.x0) (- e.y1 e.y0))
     (clip)
     (fill-style (css-color e.color))
     (let ((y e.y0))
       (dolist (line (split e.text "\n"))
         (let ((x e.x0))
           (dolist (word (split line " "))
             (let ((wsz (text-width word)))
               (when (and (> (+ x wsz) e.x1) (> x e.x0))
                 (incf y e.size)
                 (setf x e.x0))
               (fill-text word x y (- e.x1 x))
               (incf x (+ wsz (/ e.size 2))))))
         (incf y e.size)))
     (restore)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun mousedown (p)
  (dolist (e (reverse *current-page*.entities))
    (when (setf *tracking* (hit e p))
      (funcall *tracking* p 'down)
      (return-from mousedown)))
  (unless (remove-editors)
    (setf *tracking* (let ((p0 p))
                       (lambda (p event)
                         (when (= event 'move)
                           (incf *zx* (* (- p.x p0.x) *sf*))
                           (incf *zy* (* (- p.y p0.y) *sf*))
                           (setf *dirty* true)))))))

(defun mouseup (p)
  (when *tracking*
    (funcall *tracking* p 'up)
    (setf *tracking* null)))

(defun mousemove (p)
  (when *tracking*
    (funcall *tracking* p 'move)))

(defun mousewheel (p delta)
  (let ((k (/ delta 120)))
    (let ((oldsf *sf*))
      (setf *sf* (* *sf* (exp (* (log 1.1) k))))
      (setf *sf* (max (min *sf* 1000) 0.001))
      (incf *zx* (* p.x (- oldsf *sf*)))
      (incf *zy* (* p.y (- oldsf *sf*)))
      (setf *dirty* true))))

(defun init ()
  (setf *screen* (create-element "div"))
  (setf *canvas* (create-element "canvas"))
  (set-style *screen*
             position "absolute"
             px/left 0
             px/top 0
             px/right 0
             px/bottom 0)
  (set-style *canvas*
             position "absolute"
             px/left 0
             px/top 0)
  (append-child document.body *screen*)
  (append-child *screen* *canvas*)
  (setf *dirty* true)
  (setf *current-page* (new-page 2100 2970 (list)))
  (setf *pages* (list *current-page*))
  (macrolet ((coords ()
               `(let ((cp (event-pos event))
                      (sp (element-pos *screen*)))
                  (p2d (/ (- (first cp) (first sp) *zx*) *sf*)
                       (/ (- (second cp) (second sp) *zy*) *sf*)))))
    (set-handler *screen* onmousedown
                 (event.stopPropagation)
                 (event.preventDefault)
                 (mousedown (coords)))
    (set-handler *screen* onmouseup
                 (event.stopPropagation)
                 (event.preventDefault)
                 (mouseup (coords)))
    (set-handler *screen* onmousemove
                 (event.stopPropagation)
                 (event.preventDefault)
                 (mousemove (coords)))
    (set-handler *screen* onmousewheel
                 (event.stopPropagation)
                 (event.preventDefault)
                 (mousewheel (coords) event.wheelDeltaY)))
  (set-interval #'update 20))

(defun create-object (creator)
  (lambda ()
    (push (funcall creator) *current-page*.entities)
    (open-editor (last *current-page*.entities))
    (setf *dirty* true)))

(defun selected ()
  (let ((editors (filter #'editor? *current-page*.entities)))
    (map (lambda (editor) editor.e) editors)))

(defun document-selector (prompt f)
  (let ((selection null)
        (cursel null)
        (docs (list-documents)))
    (with-window (w (100 100 500 500
                         :title prompt)
                    ((doclist (create-element "table"))
                     (ok (button "OK" (lambda ()
                                        (funcall f selection)
                                        (hide-window w))))
                     (cancel (button "Cancel" (lambda ()
                                                (funcall f null)
                                                (hide-window w)))))
                    (:V :border 8 :spacing 8
                        (:Vdiv doclist)
                        (:H :size 35
                            (:H)
                            (:Hdiv ok :size 80)
                            (:Hdiv cancel :size 80)
                            (:H))))
      (let ((head (create-element "tr")))
        (setf doclist.border 1)
        (append-child doclist head)
        (setf head.height 20)
        (labels ((add-th (x) (let ((th (create-element "th")))
                               (setf th.textContent x)
                               (append-child head th))))
          (add-th "Name")
          (add-th "Description")))
      (dolist ((name description) docs)
        (let ((row (create-element "tr")))
          (setf row.height 20)
          (set-handler row onclick
                       (event.preventDefault)
                       (event.stopPropagation)
                       (setf selection name)
                       (when cursel
                         (set-style cursel
                                    backgroundColor "#FFFFFF"))
                       (set-style row
                                  backgroundColor "#FFFF00")
                       (setf cursel row))
          (append-child doclist row)
          (labels ((add-td (x) (let ((td (create-element "td")))
                                 (setf td.textContent x)
                                 (append-child row td))))
            (add-td name)
            (add-td description))))
      (show-window w))))

(defun make-toolbar ()
  (with-window (w (20 50 120 650
                      :title "Menu"
                      :close false)
                  ((new-commands (group "New"))
                   (rect (button "Rect" (create-object (lambda () (new-rect 100 100 500 300
                                                                            (rgb 255 255 128))))))
                   (text (button "Text" (create-object (lambda () (new-text 100 100 500 300
                                                                            "<Type your text here>"
                                                                            (rgb 0 0 0) 14
                                                                            "Arial" false false)))))
                   (image (button "Image" (create-object (lambda () (new-image 100 100 500 300
                                                                               "jslisp.png")))))
                   (edit-commands (group "Edit"))
                   (del (button "Delete" (lambda ()
                                           (let ((selected (selected)))
                                             (setf *current-page*.entities
                                                   (filter (lambda (x)
                                                             (not (find x selected)))
                                                           *current-page*.entities))
                                             (remove-editors)
                                             (setf *dirty* true)))))
                   (front (button "To front" (lambda ()
                                               (dolist (x (selected))
                                                 (setf *current-page*.entities
                                                       (remove x *current-page*.entities))
                                                 (push x *current-page*.entities))
                                               (setf *dirty* true))))
                   (back (button "To back" (lambda ()
                                             (dolist (x (selected))
                                               (setf *current-page*.entities
                                                     (remove x *current-page*.entities))
                                               (setf *current-page*.entities
                                                     (append (list x)
                                                             *current-page*.entities)))
                                             (setf *dirty* true))))
                   (clone (button "Copy" (lambda ()
                                           (dolist (x (selected))
                                             (push (copy x) *current-page*.entities))
                                           (remove-editors)
                                           (setf *dirty* true))))
                   (page-commands (group "Pages"))
                   (next (button "Next" (lambda ()
                                          (let ((ix (index *current-page* *pages*)))
                                            (setf ix (% (1+ ix) (length *pages*)))
                                            (setf *current-page* (aref *pages* ix)))
                                          (setf *dirty* true))))
                   (prev (button "Prev" (lambda ()
                                          (let ((ix (index *current-page* *pages*)))
                                            (setf ix (% (1- (+ ix (length *pages*)))
                                                        (length *pages*)))
                                            (setf *current-page* (aref *pages* ix)))
                                          (setf *dirty* true))))
                   (new (button "New" (lambda ()
                                        (setf *current-page* (new-page
                                                              *current-page*.width
                                                              *current-page*.height
                                                              (list)))
                                        (push *current-page* *pages*)
                                        (setf *dirty* true))))
                   (view-commands (group "View"))
                   (zoom+ (button "Zoom+" (lambda ()
                                            (setf *sf* (* *sf* 1.2))
                                            (setf *dirty* true))))
                   (zoom- (button "Zoom-" (lambda ()
                                            (setf *sf* (/ *sf* 1.2))
                                            (setf *dirty* true))))
                   (show-borders (button "Borders" (lambda ()
                                                     (setf *borders* (not *borders*))
                                                     (setf *dirty* true))))
                   (show-grid (button "Grid" (lambda ()
                                               (setf *grid* (not *grid*))
                                               (setf *dirty* true))))
                   (document-commands (group "Document"))
                   (doc-new (button "New" (lambda ()
                                            (setf *current-page* (new-page
                                                                  *current-page*.width
                                                                  *current-page*.height
                                                                  (list)))
                                            (setf *pages* (list *current-page*))
                                            (setf *dirty* true))))
                   (doc-open (button "Open" (lambda ()
                                              (document-selector "Open"
                                                 (lambda (name)
                                                   (when name
                                                     (let ((doc (get-document name)))
                                                       (setf *pages* doc.pages)
                                                       (setf *current-page* (first *pages*))
                                                       (setf *dirty* true))))))))
                   (doc-save (button "Save" (lambda ()
                                              (document-selector "Save"
                                                 (lambda (name)
                                                   (when name
                                                     (let ((doc (get-document name)))
                                                       (setf doc.pages *pages*)
                                                       (save-document doc))))))))
                   (doc-save-as (button "Save as..." (lambda ()
                                                       (alert "To do")))))
                  (:V :spacing 16 :border 8
                      (:Vdiv new-commands :border 8 :spacing 2 :weight 300
                             (:Hdiv rect)
                             (:Hdiv text)
                             (:Hdiv image))
                      (:Vdiv edit-commands :border 8 :spacing 2 :weight 400
                             (:Hdiv del)
                             (:Hdiv front)
                             (:Hdiv back)
                             (:Hdiv clone))
                      (:Vdiv page-commands :border 8 :spacing 2 :weight 300
                             (:Hdiv next)
                             (:Hdiv prev)
                             (:Hdiv new))
                      (:Vdiv view-commands :border 8 :spacing 2 :weight 400
                             (:Hdiv zoom+)
                             (:Hdiv zoom-)
                             (:Hdiv show-borders)
                             (:Hdiv show-grid))
                      (:Vdiv document-commands :border 8 :spacing 2 :weight 400
                             (:Hdiv doc-new)
                             (:Hdiv doc-open)
                             (:Hdiv doc-save)
                             (:Hdiv doc-save-as))))
    (show-window w)))

(defun main ()
  (init)
  (make-toolbar)
  (setf *dirty* true))

(main)