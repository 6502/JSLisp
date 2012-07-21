(import * from gui)
(import * from graphics)

(defobject page (width height entities))

(defvar *pages* null)
(defvar *current-page* null)
(defvar *screen* null)
(defvar *canvas* null)
(defvar *selection* null)
(defvar *zx* 0)
(defvar *zy* 0)
(defvar *sf* 0.1)
(defvar *dirty* false)
(defvar *tracking* null)

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
    (fill-rect 5 5 (+ 5 *current-page*.width) (+ 5 *current-page*.height) "#000000")
    (fill-rect 0 0 *current-page*.width *current-page*.height "#FFFFFF")
    (dolist (x *current-page*.entities)
      (draw x))
    (setf *dirty* false)))

(defobject p2d (x y))
(defun p2d (x y) (new-p2d x y))

;; Rectangle tracking

(defun rect-tracker (e)
  "Trackers for object [e] handling drag/resizing"
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
           (setf *dirty* true)))))))

;; Rectangle object

(defobject rect (x0 y0 x1 y1 width stroke fill))

(defmethod hit (e p) (rect? e)
           (and (<= e.x0 p.x e.x1)
                (<= e.y0 p.y e.y1)
                (rect-tracker e)))

(defmethod draw (e) (rect? e)
  (fill-rect e.x0 e.y0 e.x1 e.y1 e.fill)
  (when (> e.width 0)
    (frame e.x0 e.y0 e.x1 e.y1 e.width e.stroke)))

;; Image object

(defobject image (x0 y0 x1 y1 img))

(defmethod hit (e p) (image? e)
           (and (<= e.x0 p.x e.x1)
                (<= e.y0 p.y e.y1)
                (rect-tracker e)))

(defmethod draw (e) (image? e)
  (with-canvas *canvas*
    (save)
    (translate *zx* *zy*)
    (scale *sf* *sf*)
    (image e.img
           e.x0 e.y0
           (- e.x1 e.x0) (- e.y1 e.y0))
    (restore)))

;; Text object

(defobject text (x0 y0 x1 y1 text color size))

(defmethod hit (e p) (text? e)
           (and (<= e.x0 p.x e.x1)
                (<= e.y0 p.y e.y1)
                (rect-tracker e)))

(defmethod draw (e) (text? e)
   ;(fill-rect e.x0 e.y0 e.x1 e.y1 "#E0FFE0")
   (with-canvas *canvas*
     (save)
     (text-baseline "top")
     (font ~"italic bold {e.size}px Arial")
     (translate *zx* *zy*)
     (scale *sf* *sf*)
     (rect e.x0 e.y0 (- e.x1 e.x0) (- e.y1 e.y0))
     (clip)
     (fill-style e.color)
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

(defun mousedown (p)
  (dolist (e (reverse *current-page*.entities))
    (when (setf *tracking* (hit e p))
      (funcall *tracking* p 'down)
      (return-from mousedown)))
  (setf *tracking* (let ((p0 p))
                     (lambda (p event)
                       (when (= event 'move)
                         (incf *zx* (* (- p.x p0.x) *sf*))
                         (incf *zy* (* (- p.y p0.y) *sf*))
                         (setf *dirty* true))))))

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

(init)
(push (new-rect 100 100 500 500 0 "#000000" "#FFFF00")
      *current-page*.entities)

(push (new-text 100 600 500 1100
                (+ "Sed ut perspiciatis unde omnis iste natus error sit voluptatem "
                   "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa "
                   "quae ab illo inventore veritatis et quasi architecto beatae vitae "
                   "dicta sunt explicabo.\n"
                   "Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit "
                   "aut fugit, sed quia consequuntur magni dolores eos qui ratione "
                   "voluptatem sequi nesciunt.\n"
                   "Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, "
                   "consectetur, adipisci velit, sed quia non numquam eius modi tempora "
                   "incidunt ut labore et dolore magnam aliquam quaerat voluptatem.\n"
                   "Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis "
                   "suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? "
                   "Quis autem vel eum iure reprehenderit qui in ea voluptate velit "
                   "esse quam nihil molestiae consequatur, vel illum qui dolorem eum "
                   "fugiat quo voluptas nulla pariatur?")
                "#0000FF" 50)
      *current-page*.entities)

(push (new-text 100 600 500 1100
                (+ "Sed ut perspiciatis unde omnis iste natus error sit voluptatem "
                   "accusantium doloremque laudantium, totam rem aperiam, eaque ipsa "
                   "quae ab illo inventore veritatis et quasi architecto beatae vitae "
                   "dicta sunt explicabo.\n"
                   "Nemo enim ipsam voluptatem quia voluptas sit aspernatur aut odit "
                   "aut fugit, sed quia consequuntur magni dolores eos qui ratione "
                   "voluptatem sequi nesciunt.\n"
                   "Neque porro quisquam est, qui dolorem ipsum quia dolor sit amet, "
                   "consectetur, adipisci velit, sed quia non numquam eius modi tempora "
                   "incidunt ut labore et dolore magnam aliquam quaerat voluptatem.\n"
                   "Ut enim ad minima veniam, quis nostrum exercitationem ullam corporis "
                   "suscipit laboriosam, nisi ut aliquid ex ea commodi consequatur? "
                   "Quis autem vel eum iure reprehenderit qui in ea voluptate velit "
                   "esse quam nihil molestiae consequatur, vel illum qui dolorem eum "
                   "fugiat quo voluptas nulla pariatur?")
                "#0000FF" 50)
      *current-page*.entities)

(push (new-image 100 1200 500 1700
                 (let ((img (create-element "img")))
                   (setf img.src "me_toon.png")
                   img))
      *current-page*.entities)

(setf *dirty* true)
