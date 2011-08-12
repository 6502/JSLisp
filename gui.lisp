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

(defun dragging (div x0 y0)
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
                 (let* ((x (. event clientX))
                        (y (. event clientY))
                        (dx (- x x0))
                        (dy (- y y0)))
                   (funcall (. event preventDefault))
                   (set-style div
                              px/left (+ (. div offsetLeft) dx)
                              px/top (+ (. div offsetTop) dy))
                   (setf x0 x)
                   (setf y0 y)))
    (set-handler cover onmouseup
                 (hide cover))
    (show cover)))

(defun window (x0 y0 w h title)
  (let ((window (create-element "div")))
    (set-style window
               position "absolute"
               px/left x0
               px/top y0
               px/width w
               px/height h
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
                   backgroundColor "#6389b7"
                   borderBottom "1px solid #000000"
                   color "#FFFFFF"
                   fontFamily "Arial"
                   px/fontSize 16
                   fontWeight "bold"
                   textAlign "center")
        (setf (. title-bar innerHTML) title)
        (append-child window title-bar)
        (set-handler title-bar onmousedown
                     (funcall (. event preventDefault))
                     (funcall (. event stopPropagation))
                     (append-child (. document body) window)
                     (dragging window
                               (. event clientX)
                               (. event clientY)))))
    (set-handler window onmousedown
                 (hide window)
                 (display (+ "Closed window " (str-value title))))
    window))

(show (window 100 100 400 200 "This is a test"))
(show (window 150 150 600 300 "This is another test"))
