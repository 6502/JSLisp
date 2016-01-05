;; Utility

;; Reader macro: [x y] --> (aref x y)
(setf (reader "[")
      (lambda (src)
        (next-char src)
        `(aref ,@(parse-delimited-list src "]"))))

;; Defines a simple structure + constructor
(defmacro defobj (name &rest attributes)
  `(progn
     (defobject ,name ,attributes)
     (defun ,name ,attributes
       (,#"new-{name}" ,@attributes))))

;; Sprite object

(defvar *lastid* 0)

(defobject sprite
  (id          ;; Sprite id
   parent      ;; Parent sprite
   matrix      ;; Matrix relative to parent as (m11 m12 m21 m22 m31 m32)
   graphics    ;; List of drawing commands
   children    ;; List of child sprites
   hit         ;; Hit function or null
   cache       ;; Either null or (matrix, dx, dy, canvas) where dx, dy is topleft relative to m31 m32
   canvas      ;; Either null or current canvas DOM element displayed
   dirty))     ;; Next dirty sprite or null if not dirty

;; Dirty sprites singly-linked list ('*dirty* used as end of list flag)
(defvar *dirty* '*dirty*)

;; True if z ordering needs recomputation
(defvar *zdirty* false)

(defun invalidate (sprite)
  "Adds the sprite to dirty sprites list if not already there"
  (unless sprite.dirty
    (setf sprite.dirty *dirty*)
    (setf *dirty* sprite)))

(defun rinvalidate (sprite)
  "Recursive invalidate (needed when matrix changes)"
  (invalidate sprite)
  (map #'rinvalidate sprite.children))

(defun set-parent (sprite parent)
  "Remove from current parent and append as last children of specified parent"
  (unless (and parent
               (= sprite (last parent.children)))
    (when sprite.parent
      (nremove-first sprite sprite.parent.children))
    (setf sprite.parent parent)
    (if parent
        (progn
          (push sprite parent.children)
          (rinvalidate sprite)
          (setf *zdirty* true))
        (when sprite.canvas
          (remove-child document.body sprite.canvas)
          (setf sprite.canvas null)
          (setf sprite.cache null)))))

;; 2d geometry

(defun xform (x y m)
  "Transforms a point by a matrix"
  (list (+ (* x [m 0]) (* y [m 2]) [m 4])
        (+ (* x [m 1]) (* y [m 3]) [m 5])))

; m[0]*x + m[2]*y + m[4] = X
; m[1]*x + m[3]*y + m[5] = Y
;
; m[0]*x + m[2]*y = X - m[4]
; m[1]*x + m[3]*y = Y - m[5]
;
; delta = m[0]*m[3] - m[1]*m[2]
; x = ((X - m[4])*m[3] - (Y - m[5])*m[2]) / delta
; y = (m[0]*(Y - m[5]) - m[1]*(X - m[4])) / delta

(defun revxform (X Y m)
  "Reverse point-matrix transformation"
  (let* ((tx (- X [m 4]))
         (ty (- Y [m 5]))
         (delta (- (* [m 0] [m 3]) (* [m 1] [m 2])))
         (deltax (- (* tx [m 3]) (* ty [m 2])))
         (deltay (- (* [m 0] ty) (* [m 1] tx))))
    (list (/ deltax delta)
          (/ deltay delta))))

(defun matmul (a b)
  "Matrix combination"
  (list (+ (* [a 0] [b 0]) (* [a 1] [b 2]))        (+ (* [a 0] [b 1]) (* [a 1] [b 3]))
        (+ (* [a 2] [b 0]) (* [a 3] [b 2]))        (+ (* [a 2] [b 1]) (* [a 3] [b 3]))
        (+ (* [a 4] [b 0]) (* [a 5] [b 2]) [b 4])  (+ (* [a 4] [b 1]) (* [a 5] [b 3]) [b 5])))

(defun transform (sprite m)
  "Transform a sprite with a matrix"
  (setf sprite.matrix (matmul m sprite.matrix))
  (rinvalidate sprite))

(defun scale (sprite sf)
  "Scale a sprite"
  (transform sprite (list sf 0
                          0 sf
                          0 0)))

(defun rotate (sprite angle)
  "Rotate a sprite"
  (transform sprite
             (list (cos angle) (sin angle)
                   (- (sin angle)) (cos angle)
                   0 0)))

(defun translate (sprite x y)
  "Translate a sprite"
  (let ((m sprite.matrix))
    (setf sprite.matrix
          (list [m 0] [m 1]
                [m 2] [m 3]
                (+ [m 4] x) (+ [m 5] y))))
  (rinvalidate sprite))

(defun set-rotation (sprite angle)
  "Sets absolute angle for a sprite (scale from m11 and m12)"
  (let* ((m sprite.matrix)
         (sf (sqrt (+ (* [m 0] [m 0]) (* [m 1] [m 1])))))
    (setf sprite.matrix
          (list (* sf (cos angle)) (* sf (sin angle))
                (- (* sf (sin angle))) (* sf (cos angle))
                [m 4] [m 5]))
    (rinvalidate sprite)))

(defun set-translation (sprite x y)
  "Sets absolute translation for a sprite"
  (let ((m sprite.matrix))
    (setf sprite.matrix
          (list [m 0] [m 1]
                [m 2] [m 3]
                x y))
    (rinvalidate sprite)))

(defun add-graphics (sprite &rest L)
  "Adds graphic commands to a sprite"
  (nappend sprite.graphics L)
  (setf sprite.cache null)
  (invalidate sprite))

(defun set-graphics (sprite &rest L)
  "Sets graphic commands of a sprite"
  (setf sprite.graphics L)
  (setf sprite.cache null)
  (invalidate sprite))

;; Constructor

(defun sprite (&optional (parent null) &rest graphics)
  "Allocates a new sprite"
  (let ((s (make-sprite id: (incf *lastid*)
                        parent: null
                        matrix: (list 1 0 0 1 0 0)
                        graphics: graphics
                        children: (list)
                        hit: null
                        cache: null
                        canvas: null
                        dirty: null)))
    (when parent
      (set-parent s parent))
    (invalidate s)
    s))

;; Graphic management

(defun draw (obj ctx))
(defun fix-bbox (obj matrix bb))

(defun bbadj (xy bb)
  (let ((x (first xy))
        (y (second xy)))
    (macrolet ((fixwhen (test var value)
                 `(when (or (null? (,var bb)) (,test (,var bb) ,value))
                    (setf (,var bb) ,value))))
      (fixwhen > first  x)
      (fixwhen > second y)
      (fixwhen < third  x)
      (fixwhen < fourth y))))

(defobj rect x y w h)
(defmethod draw (o ctx) (rect? o)
  (ctx.beginPath)
  (ctx.moveTo o.x o.y)
  (ctx.lineTo (+ o.w o.x) o.y)
  (ctx.lineTo (+ o.w o.x) (+ o.h o.y))
  (ctx.lineTo o.x (+ o.h o.y))
  (ctx.closePath))
(defmethod fix-bbox (o m bb) (rect? o)
  (let ((p0 (xform o.x o.y m))
        (p1 (xform (+ o.x o.w) o.y m))
        (p2 (xform (+ o.x o.w) (+ o.y o.h) m))
        (p3 (xform o.x (+ o.y o.h) m)))
    (dolist (p (list p0 p1 p2 p3))
      (bbadj p bb))))

(defobj arc x y r a0 a1 ccw)
(defmethod draw (o ctx) (arc? o)
  (ctx.arc o.x o.y o.r o.a0 o.a1 o.ccw))
(defmethod fix-bbox (o m bb) (arc? o)
  (dotimes (i 20)
    (let ((t (+ o.a0 (/ (* (- o.a1 o.a0) i) 20))))
      (bbadj (xform (+ (* o.r (cos t)) o.x)
                    (+ (* o.r (sin t)) o.y)
                    m)
             bb))))

(defobj move-to x y)
(defmethod draw (o ctx) (move-to? o)
  (ctx.moveTo o.x o.y))
(defmethod fix-bbox (o m bb) (move-to? o)
  (bbadj (xform o.x o.y m) bb))

(defobj line-to x y)
(defmethod draw (o ctx) (line-to? o)
  (ctx.lineTo o.x o.y))
(defmethod fix-bbox (o m bb) (line-to? o)
  (bbadj (xform o.x o.y m) bb))

(defobj begin-path)
(defmethod draw (o ctx) (begin-path? o)
  (ctx.beginPath))

(defobj close-path)
(defmethod draw (o ctx) (close-path? o)
  (ctx.closePath))

(defobj fill color)
(defmethod draw (o ctx) (fill? o)
  (setf ctx.fillStyle o.color)
  (ctx. fill))

(defobj stroke color width)
(defmethod draw (o ctx) (stroke? o)
  (setf ctx.strokeStyle o.color)
  (setf ctx.lineWidth o.width)
  (ctx.stroke))
(defmethod fix-bbox (o m bb) (stroke? o)
  (let ((w (* (sqrt (+ (* [m 0] [m 0]) (* [m 1] [m 1]))) o.width)))
    (when (< (fifth bb) w)
      (setf (fifth bb) w))))

(defobj image img x y)
(defmethod draw (o ctx) (image? o)
  (ctx.drawImage o.img
                 (- o.x (/ (. o.img width) 2))
                 (- o.y (/ (. o.img height) 2))))
(defmethod fix-bbox (o m bb) (image? o)
  (let ((w (/ o.img.width 2))
        (h (/ o.img.height 2)))
    (let ((p0 (xform (- o.x w) (- o.y h) m))
          (p1 (xform (+ o.x w) (- o.y h) m))
          (p2 (xform (+ o.x w) (+ o.y h) m))
          (p3 (xform (- o.x w) (+ o.y h) m)))
      (dolist (p (list p0 p1 p2 p3))
        (bbadj p bb)))))

(defun circle (x y r)
  (arc x y r 0 (* 2 pi) true))

(defun load-image (sprite src x y)
  (let ((img (create-element "img")))
    (setf img.onload
          (lambda (&rest args) (add-graphics sprite (image img x y))))
    (setf img.src src)))

;;;;;;;;;;;;;;

(defvar *root* (sprite))

(defun total-matrix (sprite)
  "Returns the total transformation matrix of a sprite"
  (do ((m sprite.matrix)
       (s sprite.parent s.parent))
      ((null? s) m)
    (setf m (matmul m s.matrix))))

(defun bounding-box (sprite m)
  "Returns the bounding box of a sprite graphics given the total transformation matrix"
  (let ((bb (list null null null null 0)))
    (dolist (g sprite.graphics)
      (fix-bbox g m bb))
    (when (first bb)
      (decf (first bb) (fifth bb))
      (decf (second bb) (fifth bb))
      (incf (third bb) (fifth bb))
      (incf (fourth bb) (fifth bb))
      (slice bb 0 4))))

(defun render (sprite m canvas)
  "Computes a bitmap representation of a sprite given its total matrix in the given canvas.
Returns null for an empty sprite or (dx dy canvas) with delta being the translation of the bitmap from (m31 m32)."
  (let ((bb (bounding-box sprite m)))
    (when bb
      (let ((ix0 (1- (floor (first bb))))
            (iy0 (1- (floor (second bb))))
            (ix1 (+ (floor (third bb)) 2))
            (iy1 (+ (floor (fourth bb)) 2)))
        (let ((w (- ix1 ix0))
              (h (- iy1 iy0)))
          (setf canvas.width w)
          (setf canvas.height h)
          (setf canvas.style.width (+ w "px"))
          (setf canvas.style.height (+ h "px"))
          (setf canvas.style.position "absolute")
          (setf canvas.style.left (+ ix0 "px"))
          (setf canvas.style.top (+ iy0 "px"))
          (let ((ctx (canvas.getContext "2d")))
            (funcall ctx.setTransform
                     [m 0] [m 1]
                     [m 2] [m 3]
                     (- [m 4] ix0) (- [m 5] iy0))
            (dolist (el sprite.graphics)
              (draw el ctx)))
          (list m (- ix0 [m 4]) (- iy0 [m 5]) canvas))))))


(defun update ()
  "Updates screen content by recomputing/translating updated sprites"
  (do ()((= *dirty* '*dirty*))
    (let ((sprite *dirty*))
      (setf *dirty* sprite.dirty)
      (setf sprite.dirty null)
      (let ((m (total-matrix sprite))
            (cm (and sprite.cache (first sprite.cache))))
        (when (or (null? cm)
                  (/= [m 0] [cm 0])
                  (/= [m 1] [cm 1])
                  (/= [m 2] [cm 2])
                  (/= [m 3] [cm 3]))
          ;; No cache or matrix change is not a translation; canvas must be (re)computed
          (unless sprite.canvas
            (setf sprite.canvas (create-element "canvas"))
            (append-child document.body sprite.canvas)
            (setf *zdirty* true))
          (setf sprite.cache (render sprite m sprite.canvas))
          (unless sprite.cache
            (remove-child document.body sprite.canvas)
            (setf sprite.canvas null)))
        (when sprite.canvas
          (setf (first sprite.cache) m)
          (setf sprite.canvas.style.left
                (+ [m 4] (second sprite.cache) "px"))
          (setf sprite.canvas.style.top
                (+ [m 5] (third sprite.cache) "px"))))))
  (when *zdirty*
    (setf *zdirty* false)
    (let ((body document.body))
      (labels ((visit (sprite)
                   (when sprite.canvas
                   (append-child body sprite.canvas))
                 (map #'visit sprite.children)))
        (visit *root*)))))

;;;;;;;;;;;;;;

;; Input "glass" (covers everything intercepting and dispatching mouse events)
(defvar *glass* (create-element "div"))
(setf *glass*.style.position "absolute")
(setf *glass*.style.left "0px")
(setf *glass*.style.right "0px")
(setf *glass*.style.top "0px")
(setf *glass*.style.bottom "0px")
;(setf *glass*.style.backgroundColor "rgba(255,0,0,0.25)")
(setf *glass*.style.zIndex 100)
(append-child document.body *glass*)

;; Currently active tracker or null
(defvar *tracking* null)

(defun mouse-move (x y)
  (when *tracking*
    (funcall *tracking* x y 1)))

(defun mouse-up (x y)
  (when *tracking*
    (funcall *tracking* x y 2)
    (setf *tracking* null)))

(defun mouse-down (x y)
  (labels ((check (s)
             (map #'check (reverse s.children))
             (when (and s.hit s.canvas)
               (let ((sx s.canvas.offsetLeft)
                     (sy s.canvas.offsetTop)
                     (sw s.canvas.width)
                     (sh s.canvas.height))
                 (when (and (<= sx x (+ sx sw -1))
                            (<= sy y (+ sy sh -1)))
                   (let* ((ctx (s.canvas.getContext "2d"))
                          (idata (ctx.getImageData (- x sx) (- y sy) 1 1)))
                     (when (/= 0 [idata.data 3])
                       (setf *tracking* s.hit)
                       (funcall *tracking* x y 0)
                       (return-from mouse-down))))))))
    (check *root*)))

(setf *glass*.onmousedown
      (lambda (event)
        (event.preventDefault)
        (mouse-down event.clientX event.clientY)))

(setf *glass*.onmouseup
      (lambda (event)
        (event.preventDefault)
        (mouse-up event.clientX event.clientY)))

(setf *glass*.onmousemove
      (lambda (event)
        (event.preventDefault)
        (mouse-move event.clientX event.clientY)))

;;;;;;;;;;;;;;

(defun drag (sprite)
  (let ((xx null) (yy null))
    (lambda (sx sy phase)
      (let* ((rp (revxform sx sy (total-matrix sprite.parent)))
             (x (first rp))
             (y (second rp)))
        (if (= 0 phase)
            (set-parent sprite sprite.parent)
            (translate sprite (- x xx) (- y yy)))
        (setf xx x)
        (setf yy y)))))

(set-interval #'update 10)
