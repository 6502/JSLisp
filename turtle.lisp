(load (http-get "fsg.lisp"))

(defvar *turtle* (create-element "img"))
(setf *turtle*.src "turtle.png")
(setf *turtle*.style.position "absolute")
(setf *turtle*.style.WebkitTransformOrigin "21px 16px")
(append-child document.body *turtle*)

(defvar *target-x* 200)
(defvar *target-y* 200)
(defvar *target-angle* 0)
(defvar *turtle-x* 199)
(defvar *turtle-y* 200)
(defvar *turtle-angle* 0)
(defvar *turtle-speed* 0.1)
(defvar *turtle-last-update* (clock))
(defvar *turtle-commands* (list))
(defvar *turtle-pen* null)

(set-interval (lambda (&rest args)
                (let* ((t (clock))
                       (dt (- t *turtle-last-update*))
                       (moved false)
                       (oldx *turtle-x*)
                       (oldy *turtle-y*))
                  (setf *turtle-last-update* t)
                  (if (/= *turtle-angle* *target-angle*)
                      (let ((da (* *turtle-speed* dt)))
                        (setf moved true)
                        (if (< *target-angle* *turtle-angle*)
                            (when (< (decf *turtle-angle* da) *target-angle*)
                              (setf *turtle-angle* *target-angle*))
                            (when (> (incf *turtle-angle* da) *target-angle*)
                              (setf *turtle-angle* *target-angle*))))
                      (let* ((dx (- *target-x* *turtle-x*))
                             (dy (- *target-y* *turtle-y*))
                             (L (sqrt (+ (* dx dx) (* dy dy))))
                             (s (* *turtle-speed* dt)))
                        (when (> L 0)
                          (setf moved true)
                          (if (> s L)
                              (progn
                                (setf *turtle-x* *target-x*)
                                (setf *turtle-y* *target-y*))
                              (let ((k (/ s L)))
                                (incf *turtle-y* (* k dy))
                                (incf *turtle-x* (* k dx)))))))
                  (if moved
                      (progn
                        (setf *turtle*.style.WebkitTransform
                              ~"translatex({(- *turtle-x* 21)}px) translatey({(- *turtle-y* 16)}px) rotate({*turtle-angle*}deg)")
                        (when *turtle-pen*
                          (begin-path)
                          (move-to oldx oldy)
                          (line-to *turtle-x* *turtle-y*)
                          (stroke-style *turtle-pen*)
                          (stroke)))
                      (when (> (length *turtle-commands*) 0)
                        (funcall (first (splice *turtle-commands* 0 1)))))))
              20)

(defmacro defcommand (name args &rest body)
  (let ((doc (list)))
    (when (string? (first body))
      (setf doc (splice body 0 1)))
    `(defun ,name ,args ,@doc (push (lambda () ,@body) *turtle-commands*) 'Ok)))

(defcommand left (x)
  "Turns the turtle x degrees to the left"
  (decf *target-angle* x))

(defcommand right (x)
  "Turns the turtle x degrees to the right"
  (incf *target-angle* x))

(defcommand move (x)
  "Advances the turtle the specified number of pixels"
  (let ((a (* (/ pi 180) *target-angle*)))
    (incf *target-x* (* x (cos a)))
    (incf *target-y* (* x (sin a)))))

(defcommand up ()
  "Stops leaving a colored trail"
  (setf *turtle-pen* null))

(defmacro colors (&rest colors)
  `(progn ,@(let ((res (list)))
              (dolist (c colors)
                (push `(defcommand ,(second c) ()
                         ,~"Starts leaving a {(second c)} trail"
                         (setf *turtle-pen* ,(first c)))
                      res))
              res)))

(colors ("#000000" black)
        ("#FF0000" red)
        ("#00FF00" green)
        ("#FFFF00" yellow)
        ("#0000FF" blue)
        ("#FF00FF" magenta)
        ("#00FFFF" cyan)
        ("#FFFFFF" white)
        ("#888888" gray))
