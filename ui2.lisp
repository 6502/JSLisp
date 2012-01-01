
(defvar *screen* (create-element "div"))
(setf (. *screen* style position) "absolute")
(setf (. *screen* style left) "0px")
(setf (. *screen* style top) "0px")
(setf (. *screen* style right) "0px")
(setf (. *screen* style bottom) "0px")
(setf (. *screen* style overflow) "hidden")
(setf (. *screen* className) "screen")
(append-child (. document body) *screen*)

(defvar *parent* *screen*)
(defvar *ctx* null)

(defmacro matrix (dom-object)
  `(. ,dom-object dataMatrix))

(defun dom-object (type)
  (let ((x (create-element type)))
    (setf (matrix x) (list 1 0 0
                           0 1 0
                           0 0 1
                           0 0 0))
    x))

(setf (reader "[")
      (lambda (src)
        (funcall src 1)
        `(aref ,@(parse-delimited-list src "]"))))

(defun matmul (a &rest others)
  (dolist (b others)
    (setf a
          (list (+ (* [a  0] [b  0]) (* [a  1] [b  3]) (* [a  2] [b  6]))
                (+ (* [a  0] [b  1]) (* [a  1] [b  4]) (* [a  2] [b  7]))
                (+ (* [a  0] [b  2]) (* [a  1] [b  5]) (* [a  2] [b  8]))

                (+ (* [a  3] [b  0]) (* [a  4] [b  3]) (* [a  5] [b  6]))
                (+ (* [a  3] [b  1]) (* [a  4] [b  4]) (* [a  5] [b  7]))
                (+ (* [a  3] [b  2]) (* [a  4] [b  5]) (* [a  5] [b  8]))

                (+ (* [a  6] [b  0]) (* [a  7] [b  3]) (* [a  8] [b  6]))
                (+ (* [a  6] [b  1]) (* [a  7] [b  4]) (* [a  8] [b  7]))
                (+ (* [a  6] [b  2]) (* [a  7] [b  5]) (* [a  8] [b  8]))

                (+ (* [a  9] [b  0]) (* [a 10] [b  3]) (* [a 11] [b  6]) [b  9])
                (+ (* [a  9] [b  1]) (* [a 10] [b  4]) (* [a 11] [b  7]) [b 10])
                (+ (* [a  9] [b  2]) (* [a 10] [b  5]) (* [a 11] [b  8]) [b 11]))))
  a)

(defmacro relnode (type &rest body)
  `(let ((node (dom-object ,type)))
     (setf (. node draggable) false)
     (setf (. node style position) "relative")
     (when *parent*
       (append-child *parent* node))
     (let ((*parent* node))
       ,@body)
     node))

(defmacro node (type &rest body)
  `(relnode ,type
            (setf (. node style position) "absolute")
            (setf (. node style MozTransformOrigin) "0% 0%")
            (setf (. node style WebkitTransformOrigin) "0% 0%")
            ,@body))

(defmacro style (&rest args)
  `(progn
     ,@(let ((res (list)))
         (dolist (arg args)
           (push `(setf (. node style ,(first arg)) ,(second arg)) res))
         res)))

(defmacro attr (&rest args)
  `(progn
     ,@(let ((res (list)))
         (dolist (arg args)
           (push `(setf (. node ,(first arg)) ,(second arg)) res))
         res)))

(defmacro handler (name &rest body)
  `(setf (. node ,name)
         (lambda (&rest args)
           (funcall (. (first args) preventDefault))
           ,@body)))

(defmacro onclick     (&rest body) `(handler onclick ,@body))
(defmacro onmousedown (&rest body) `(handler onmousedown ,@body))
(defmacro onmouseover (&rest body) `(handler onmouseover ,@body))
(defmacro onmouseout  (&rest body) `(handler onmouseout ,@body))

(defmacro div (&rest body)
  `(node "div" ,@body))

(defmacro img (src &rest body)
  `(node "img"
         (setf (. *parent* src) ,src)
         ,@body))

(defmacro canvas (width height &rest body)
  (let ((canvas (gensym))
        (w (gensym))
        (h (gensym)))
    `(let* ((,w ,width)
            (,h ,height)
            (*parent* (div
                       (style (width (+ ,w "px"))
                              (height (+ ,h "px"))
                              (lineHeight "0"))))
            (,canvas (create-element "canvas")))
       (append-child *parent* ,canvas)
       (setf (. ,canvas width) ,w)
       (setf (. ,canvas height) ,h)
       (setf (. ,canvas style width) (+ ,w "px"))
       (setf (. ,canvas style height) (+ ,h "px"))
       (setf (. ,canvas style margin) "0px")
       (setf (. ,canvas style padding) "0px")
       (setf (. ,canvas style border) "none")
       (let ((*ctx* (funcall (. ,canvas getContext) "2d")))
         ,@body)
       *parent*)))

(defun set-matrix (node m)
  (setf (. node dataMatrix) m)
  (setf (. node style WebkitTransform) ~"matrix3d({[m 0]},{[m 1]},{[m 2]},0,{[m 3]},{[m 4]},{[m 5]},0,{[m 6]},{[m 7]},{[m 8]},0,{[m 9]},{[m 10]},{[m 11]},1)")
  (setf (. node style MozTransform) ~"matrix({[m 0]},{[m 1]},{[m 3]},{[m 4]},{[m 9]},{[m 10]})"))

(defun matrix-combine (obj &rest m)
  (setf (matrix obj) (apply #'matmul (append (list (matrix obj)) m))))

(defun translation (x y &optional (z 0))
  (list 1 0 0
        0 1 0
        0 0 1
        x y z))

(defun scaling (x &optional y z)
  (when (undefinedp y) (setf y x))
  (when (undefinedp z) (setf z x))
  (list x 0 0
        0 y 0
        0 0 z
        0 0 0))

(defun rotation-x (angle)
  (let* ((c (cos angle))
         (s (sin angle))
         (n (- s)))
    (list 1 0 0
          0 c s
          0 n c
          0 0 0)))

(defun rotation-y (angle)
  (let* ((c (cos angle))
         (s (sin angle))
         (n (- s)))
    (list c 0 s
          0 1 0
          n 0 c
          0 0 0)))

(defun rotation-z (angle)
  (let* ((c (cos angle))
         (s (sin angle))
         (n (- s)))
    (list c s 0
          n c 0
          0 0 1
          0 0 0)))

(defun centered (x y z &rest t)
  (apply #'matmul (append (list (translation (- x) (- y) (- z)))
                          t
                          (list (translation x y z)))))

(defun scale (x &optional y z)
  (matrix-combine *parent* (scaling x y z)))

(defun translate (x y &optional (z 0))
  (matrix-combine *parent* (translation x y z)))

(defun rotate-x (y z angle)
  (matrix-combine *parent* (centered 0 y z (rotation-x angle))))

(defun rotate-y (x z angle)
  (matrix-combine *parent* (centered x 0 z (rotation-y angle))))

(defun rotate-z (x y angle)
  (matrix-combine *parent* (centered x y 0 (rotation-z angle))))

(defmacro html (x &rest body)
  `(relnode "div"
         (style (width "100%")
                (height "100%"))
         (setf (. *parent* innerHTML) ,x)
         ,@body))

(defun begin-path () (funcall (. *ctx* beginPath)))
(defun close-path () (funcall (. *ctx* closePath)))
(defun arc (x y r a0 a1 ccw) (funcall (. *ctx* arc) x y r a0 a1 ccw))
(defun move-to (x y) (funcall (. *ctx* moveTo) x y))
(defun line-to (x y) (funcall (. *ctx* lineTo) x y))
(defun fill (style)
  (setf (. *ctx* fillStyle) style)
  (funcall (. *ctx* fill)))
(defun stroke (style width)
  (setf (. *ctx* strokeStyle) style)
  (setf (. *ctx* lineWidth) width)
  (funcall (. *ctx* stroke)))

(defun polyline (&rest pts)
  (begin-path)
  (do ((i 0 (+ i 2)))
      ((>= i (length pts)))
    (let ((x (aref pts i))
          (y (aref pts (1+ i))))
      (if (= i 0)
          (move-to x y)
          (line-to x y)))))

(defun polygon (&rest pts)
  (apply #'polyline pts)
  (close-path))

(defmacro absdiv (x y w h &rest body)
  `(div
    (style (width (+ ,w "px"))
           (height (+ ,h "px")))
    (translate ,x ,y 0)
    ,@body))

(defmacro button (caption id f &rest body)
  `(relnode "input"
            (attr (className "button")
                  (id ,id)
                  (type "button")
                  (value ,caption))
            (onclick (funcall ,f))
            ,@body))

(defmacro table (&rest body)
  `(relnode "table" ,@body))

(defmacro tr (&rest body)
  `(relnode "tr" ,@body))

(defmacro td (&rest body)
  `(relnode "td" ,@body))

(defmacro th (&rest body)
  `(relnode "th" ,@body))

(defmacro center (&rest body)
  `(relnode "table"
            (style (width "100%")
                   (height "100%"))
            (attr (cellpadding "0")
                  (cellspacing "0"))
            (tr (td (attr (width "100%")
                          (height "100%")
                          (align "center")
                          (valign "middle"))
                    ,@body))))

(setf (reader "$")
      (lambda (src)
        (funcall src 1)
        `(. *fields* ,(parse-value src) value)))

(defmacro dialog-store (var)
  (let ((k (gensym)))
    `(dolist (,k (keys *fields*))
       (setf [,var ,k] (. [*fields* ,k] value)))))

(defmacro dialog-load (var)
  (let ((k (gensym)))
    `(dolist (,k (keys ,var))
       (when [*fields* ,k]
             (setf (. [*fields* ,k] value) [,var ,k])))))

(defmacro dialog-clear ()
  (let ((k (gensym)))
    `(dolist (,k (keys *fields*))
       (setf (. [*fields* ,k] value) ""))))

(defmacro input (caption id &rest body)
  (let ((f (gensym)))
    `(table
      (tr (td (html ,caption
                    (attr (className "caption")))))
      (tr (td (let ((,f (relnode "input"
                                 (attr (className "input")
                                       (id ,id)))))
                (setf [*fields* ,id] ,f))))
      ,@body)))

(defmacro password (caption id &rest body)
  (let ((f (gensym)))
    `(table
      (tr (td (html ,caption
                    (attr (className "caption")))))
      (tr (td (let ((,f (relnode "input"
                                 (attr (className "input")
                                       (type "password")
                                       (id ,id)))))
                (setf [*fields* ,id] ,f))))
      ,@body)))

(defun load-css (url)
  (let ((s (create-element "link")))
    (setf (. s rel) "stylesheet")
    (setf (. s href) url)
    (setf (. s type) "text/css")
    (append-child (. document head) s)))

(defun display (x) (alert x))
