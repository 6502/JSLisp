;; Utility

;; Reader macro: [x y] --> (aref x y)
(setf (reader "[")
      (lambda (src)
        (funcall src 1)
        `(aref ,@(parse-delimited-list src "]"))))

;; Removes an element from a list
(defun delete (x L)
  "Removes first occurence of x from L if present and returns true. Returns false if x is not found."
  (let ((i (index x L)))
    (if (>= i 0)
        (progn
          (splice L i 1)
          true)
        false)))

;; Defines a simple structure + constructor
(defmacro defobj (name &rest attributes)
  `(progn
     (defstruct ,name ,@attributes)
     (defun ,name (,@attributes)
       (,(intern ~"make-{(symbol-name name)}")
         ,@(let ((res (list)))
                (dolist (a attributes)
                  (push (intern ~":{(symbol-name a)}") res)
                  (push a res))
                res)))))

;; Virtual single-dispatch
(defmacro defgeneric (name default-impl)
  (let ((vmt (intern ~"*{(symbol-name name)}*")))
    `(progn
       (defvar ,vmt (js-object))
       (defun ,name (&rest args)
         (let ((f (aref ,vmt (symbol-name (first (first args))))))
           (if f
               (apply f args)
               (progn ,@default-impl)))))))

(defmacro defmethod (name type args &rest body)
  (let ((vmt (intern ~"*{(symbol-name name)}*")))
    `(setf (aref ,vmt ,(symbol-name type))
           (lambda ,args ,@body))))

;; Sprite object

(defvar *lastid* 0)

(defstruct sprite
  id          ;; Sprite id
  parent      ;; Parent sprite
  matrix      ;; Matrix relative to parent as (m11 m12 m21 m22 m31 m32)
  graphics    ;; List of drawing commands
  children    ;; List of child sprites
  hit         ;; Hit function or null
  cache       ;; Either null or (dx, dy, canvas) where delta is relative to m31 m32
  canvas      ;; Either null or current canvas DOM element displayed
  dirty)      ;; Next dirty sprite or null if not dirty

;; Dirty sprites singly-linked list ('*dirty* used as end of list flag)
(defvar *dirty* '*dirty*)

;; True if z ordering needs recomputation
(defvar *zdirty* false)

(defun touch (sprite)
  "Adds the sprite to dirty sprites list if not already there"
  (unless (sprite-dirty sprite)
    (setf (sprite-dirty sprite) *dirty*)
    (setf *dirty* sprite)))

(defun invalidate (sprite)
  "Invalidates sprite cached canvas"
  (touch sprite)
  (setf (sprite-cache sprite) null))

(defun rtouch (sprite)
  "Recursive touch (needed when translating a sprite)"
  (touch sprite)
  (map #'rtouch (sprite-children sprite)))

(defun rinvalidate (sprite)
  "Recursive invalidate (needed when matrix change is not a pure translation)"
  (invalidate sprite)
  (map #'rinvalidate (sprite-children sprite)))

(defun set-parent (sprite parent)
  "Remove from current parent and append as last children of specified parent"
  (unless (and parent
               (== parent (sprite-parent sprite))
               (== sprite (last (sprite-children parent))))
    (when (sprite-parent sprite)
      (delete sprite (sprite-children (sprite-parent sprite)))
      (when (sprite-canvas sprite)
        (remove-child (. document body) (sprite-canvas sprite))
        (setf (sprite-canvas sprite) null)))
    (setf (sprite-parent sprite) parent)
    (when parent
      (push sprite (sprite-children parent))
      (rinvalidate sprite))
    (setf *zdirty* true)))

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
  (setf (sprite-matrix sprite)
        (matmul m (sprite-matrix sprite)))
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
  (let ((m (sprite-matrix sprite)))
    (setf (sprite-matrix sprite)
          (list [m 0] [m 1]
                [m 2] [m 3]
                (+ [m 4] x) (+ [m 5] y))))
  (rtouch sprite))

(defun set-rotation (sprite angle)
  "Sets absolute angle for a sprite (scale from m11 and m12)"
  (let* ((m (sprite-matrix sprite))
         (sf (sqrt (+ (* [m 0] [m 0]) (* [m 1] [m 1])))))
    (setf (sprite-matrix sprite)
          (list (* sf (cos angle)) (* sf (sin angle))
                (- (* sf (sin angle))) (* sf (cos angle))
                [m 4] [m 5]))
    (rinvalidate sprite)))

(defun set-translation (sprite x y)
  "Sets absolute translation for a sprite"
  (let ((m (sprite-matrix sprite)))
    (setf (sprite-matrix sprite)
          (list [m 0] [m 1]
                [m 2] [m 3]
                x y))
    (rtouch sprite)))

(defun add-graphics (sprite &rest L)
  "Adds graphic commands to a sprite"
  (setf (sprite-graphics sprite)
        (append (sprite-graphics sprite) L))
  (invalidate sprite))

(defun set-graphics (sprite &rest L)
  "Sets graphic commands of a sprite"
  (setf (sprite-graphics sprite) L)
  (invalidate sprite))

;; Constructor

(defun sprite (&optional (parent null) &rest graphics)
  "Allocates a new sprite"
  (let ((s (make-sprite :id (incf *lastid*)
                        :parent null
                        :matrix (list 1 0 0 1 0 0)
                        :graphics graphics
                        :children (list)
                        :hit null
                        :cache null
                        :canvas null
                        :dirty null)))
    (when parent
      (set-parent s parent))
    (touch s)
    s))

;; Graphic management

(defgeneric draw null)
(defgeneric fix-bbox null)

(defun bbadj ((x y) bb)
  (macrolet ((fixwhen (test var value)
               `(when (or (nullp (,var bb)) (,test (,var bb) ,value))
                  (setf (,var bb) ,value))))
    (fixwhen > first  x)
    (fixwhen > second y)
    (fixwhen < third  x)
    (fixwhen < fourth y)))

(defobj rect x y w h)
(defmethod draw rect (o ctx)
  (funcall (. ctx beginPath))
  (funcall (. ctx moveTo) (rect-x o) (rect-y o))
  (funcall (. ctx lineTo) (+ (rect-w o) (rect-x o)) (rect-y o))
  (funcall (. ctx lineTo) (+ (rect-w o) (rect-x o)) (+ (rect-h o) (rect-y o)))
  (funcall (. ctx lineTo) (rect-x o) (+ (rect-h o) (rect-y o)))
  (funcall (. ctx closePath)))
(defmethod fix-bbox rect (o m bb)
  (let ((p0 (xform (rect-x o) (rect-y o) m))
        (p1 (xform (+ (rect-x o) (rect-w o)) (rect-y o) m))
        (p2 (xform (+ (rect-x o) (rect-w o)) (+ (rect-y o) (rect-h o)) m))
        (p3 (xform (rect-x o) (+ (rect-y o) (rect-h o)) m)))
    (dolist (p (list p0 p1 p2 p3))
      (bbadj p bb))))

(defobj arc x y r a0 a1 ccw)
(defmethod draw arc (o ctx)
  (funcall (. ctx arc) (arc-x o) (arc-y o) (arc-r o) (arc-a0 o) (arc-a1 o) (arc-ccw o)))
(defmethod fix-bbox arc (o m bb)
  (dotimes (i 20)
    (let ((t (+ (arc-a0 o) (/ (* (- (arc-a1 o) (arc-a0 o)) i) 20))))
      (bbadj (xform (+ (* (arc-r o) (cos t)) (arc-x o))
                    (+ (* (arc-r o) (sin t)) (arc-y o))
                    m)
             bb))))

(defobj move-to x y)
(defmethod draw move-to (o ctx)
  (funcall (. ctx moveTo) (move-to-x o) (move-to-y o)))
(defmethod fix-bbox move-to (o m bb)
  (bbadj (xform (move-to-x o) (move-to-y o) m) bb))

(defobj line-to x y)
(defmethod draw line-to (o ctx)
  (funcall (. ctx lineTo) (line-to-x o) (line-to-y o)))
(defmethod fix-bbox line-to (o m bb)
  (bbadj (xform (line-to-x o) (line-to-y o) m) bb))

(defobj begin-path)
(defmethod draw begin-path (o ctx)
  (funcall (. ctx beginPath)))

(defobj close-path)
(defmethod draw close-path (o ctx)
  (funcall (. ctx closePath)))

(defobj fill color)
(defmethod draw fill (o ctx)
  (setf (. ctx fillStyle) (fill-color o))
  (funcall (. ctx fill)))

(defobj stroke color width)
(defmethod draw stroke (o ctx)
  (setf (. ctx strokeStyle) (stroke-color o))
  (setf (. ctx lineWidth) (stroke-width o))
  (funcall (. ctx stroke)))
(defmethod fix-bbox stroke (o m bb)
  (let ((w (* (sqrt (+ (* [m 0] [m 0]) (* [m 1] [m 1]))) (stroke-width o))))
    (when (< (fifth bb) w)
      (setf (fifth bb) w))))

(defobj image img x y)
(defmethod draw image (o ctx)
  (funcall (. ctx drawImage) (image-img o)
           (- (image-x o) (/ (. (image-img o) width) 2))
           (- (image-y o) (/ (. (image-img o) height) 2))))
(defmethod fix-bbox image (o m bb)
  (let ((w (/ (. (image-img o) width) 2))
        (h (/ (. (image-img o) height) 2)))
    (let ((p0 (xform (- (image-x o) w) (- (image-y o) h) m))
          (p1 (xform (+ (image-x o) w) (- (image-y o) h) m))
          (p2 (xform (+ (image-x o) w) (+ (image-y o) h) m))
          (p3 (xform (- (image-x o) w) (+ (image-y o) h) m)))
      (dolist (p (list p0 p1 p2 p3))
        (bbadj p bb)))))

(defun circle (x y r)
  (arc x y r 0 (* 2 pi) true))

(defun load-image (sprite src x y)
  (let ((img (create-element "img")))
    (setf (. img onload)
          (add-graphics sprite (image img x y)))
    (setf (. img src) src)))

;;;;;;;;;;;;;;

(defvar *root* (sprite))

(defun total-matrix (sprite)
  "Returns the total transformation matrix of a sprite"
  (do ((m (sprite-matrix sprite))
       (s (sprite-parent sprite) (sprite-parent s)))
      ((nullp s) m)
    (setf m (matmul m (sprite-matrix s)))))

(defun bounding-box (sprite m)
  "Returns the bounding box of a sprite graphics given the total transformation matrix"
  (let ((bb (list null null null null 0)))
    (dolist (g (sprite-graphics sprite))
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
          (setf (. canvas width) w)
          (setf (. canvas height) h)
          (setf (. canvas style width) (+ w "px"))
          (setf (. canvas style height) (+ h "px"))
          (setf (. canvas style position) "absolute")
          (setf (. canvas style left) (+ ix0 "px"))
          (setf (. canvas style top) (+ iy0 "px"))
          (let ((ctx (funcall (. canvas getContext) "2d")))
            (funcall (. ctx setTransform)
                     [m 0] [m 1]
                     [m 2] [m 3]
                     (- [m 4] ix0) (- [m 5] iy0))
            (dolist (el (sprite-graphics sprite))
              (draw el ctx)))
          (list (- ix0 [m 4]) (- iy0 [m 5]) canvas))))))

(defun update ()
  "Updates screen content by recomputing/translating updated sprites"
  (do ()((== *dirty* '*dirty*))
    (let ((sprite *dirty*))
      (setf *dirty* (sprite-dirty sprite))
      (setf (sprite-dirty sprite) null)
      (let ((m (total-matrix sprite)))
        (when (nullp (sprite-cache sprite))
          (unless (sprite-canvas sprite)
            (setf (sprite-canvas sprite) (create-element "canvas"))
            (append-child (. document body) (sprite-canvas sprite))
            (setf *zdirty* true))
          (setf (sprite-cache sprite) (render sprite m (sprite-canvas sprite)))
          (unless (sprite-cache sprite)
            (remove-child (. document body) (sprite-canvas sprite))
            (setf (sprite-canvas sprite) null)))
        (when (sprite-canvas sprite)
          (setf (. (sprite-canvas sprite) style left)
                (+ [m 4] (first (sprite-cache sprite)) "px"))
          (setf (. (sprite-canvas sprite) style top)
                (+ [m 5] (second (sprite-cache sprite)) "px"))))))
  (when *zdirty*
    (setf *zdirty* false)
    (let ((body (. document body)))
      (labels ((visit (sprite)
                 (when (sprite-canvas sprite)
                   (append-child body (sprite-canvas sprite)))
                 (map #'visit (sprite-children sprite))))
        (visit *root*)))))

;;;;;;;;;;;;;;

;; Input "glass" (covers everything intercepting and dispatching mouse events)
(defvar *glass* (create-element "div"))
(setf (. *glass* style position) "absolute")
(setf (. *glass* style left) "0px")
(setf (. *glass* style right) "0px")
(setf (. *glass* style top) "0px")
(setf (. *glass* style bottom) "0px")
;(setf (. *glass* style backgroundColor) "rgba(255,0,0,0.25)")
(setf (. *glass* style zIndex) 100)
(append-child (. document body) *glass*)

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
             (map #'check (reverse (sprite-children s)))
             (when (and (sprite-hit s) (sprite-canvas s))
               (let ((sx (. (sprite-canvas s) offsetLeft))
                     (sy (. (sprite-canvas s) offsetTop))
                     (sw (. (sprite-canvas s) width))
                     (sh (. (sprite-canvas s) height)))
                 (when (and (<= sx x (+ sx sw -1))
                            (<= sy y (+ sy sh -1)))
                   (let* ((ctx (funcall (. (sprite-canvas s) getContext) "2d"))
                          (idata (funcall (. ctx getImageData) (- x sx) (- y sy) 1 1)))
                     (when (/= 0 [(. idata data) 3])
                       (setf *tracking* (sprite-hit s))
                       (funcall *tracking* x y 0)
                       (return-from mouse-down))))))))
    (check *root*)))

(setf (. *glass* onmousedown)
      (lambda (event)
        (funcall (. event preventDefault))
        (mouse-down (. event clientX) (. event clientY))))

(setf (. *glass* onmouseup)
      (lambda (event)
        (funcall (. event preventDefault))
        (mouse-up (. event clientX) (. event clientY))))

(setf (. *glass* onmousemove)
      (lambda (event)
        (funcall (. event preventDefault))
        (mouse-move (. event clientX) (. event clientY))))

;;;;;;;;;;;;;;

(defun drag (sprite)
  (let ((xx null) (yy null))
    (lambda (sx sy phase)
      (let* ((rp (revxform sx sy (total-matrix (sprite-parent sprite))))
             (x (first rp))
             (y (second rp)))
        (if (= 0 phase)
            (set-parent sprite (sprite-parent sprite))
            (translate sprite (- x xx) (- y yy)))
        (setf xx x)
        (setf yy y)))))

(set-interval #'update 10)
