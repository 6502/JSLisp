(defstruct transition
  begin end old new)

(defun smooth-interp (a b t)
  "Returns a smoothly interpolated value between a and b when t varies from 0 to 1"
  (cond
    ((<= t 0) a)
    ((>= t 1) b)
    (true (let ((w (- (* 3 t t) (* 2 t t t))))
            (+ (* a (- 1 w))
               (* b w))))))

(defun var (value duration)
  "Creates an animated variable closure supporting value: set-value: set-duration: messages"
  (let ((transitions (list)))
    (labels ((fast-forward-to (now)
               (do ()
                   ((or (empty transitions)
                        (> (transition-end (first transitions)) now)))
                 (setf value (transition-new (first transitions)))
                 (splice transitions 0 1))))
      (lambda (msg &rest args)
        (cond
          ((= msg set-value:)
           (let ((now (clock))
                 (new-value (first args)))
             (fast-forward-to now)
             (push (if (empty transitions)
                       (make-transition begin: now
                                        end: (+ now duration)
                                        old: value
                                        new: new-value)
                       (let ((lt (last transitions)))
                         (make-transition begin: (transition-end lt)
                                          end: (+ (transition-end lt) duration)
                                          old: (transition-new lt)
                                          new: new-value)))
                   transitions)
             new-value))
          ((= msg set-duration:)
           (setf duration (first args)))
          ((= msg value:)
           (let ((now (clock)))
             (fast-forward-to now)
             (unless (empty transitions)
               (let* ((ft (first transitions))
                      (t (/ (- now (transition-begin ft))
                            (- (transition-end ft)
                               (transition-begin ft)))))
                 (setf value (smooth-interp
                              (transition-old ft)
                              (transition-new ft)
                              t))))
             value)))))))

(defmacro var-access (x)
  "Current value of an animated variable"
  `(funcall ,x value:))

(defmacro set-var-access (x y)
  "Sets long-term value of an animated variable by queueing a transition for it"
  `(funcall ,x set-value: ,y))

(defmacro defanimated (name value duration)
  "Defines a global variable as animated"
  `(progn
     (defvar ,(intern ~"*animated-{name}*") (var ,value ,duration))
     (define-symbol-macro ,name (var-access ,(intern ~"*animated-{name}*")))))

(defmacro animated-let (vars &rest body)
  "Evaluates body forms after lexically binding specified vars to animated values. Each element of vars is assumed to be a list (name start-value transition-duration)."
  (let ((animated (map (lambda (x) (gensym))
                       (range (length vars)))))
    `(let (,@(let ((res (list))
                   (i 0))
               (dolist (v vars)
                 (push `(,(aref animated i)
                          (var ,(second v) ,(third v)))
                       res)
                 (incf i))
               res))
       (symbol-macrolet (,@(let ((res (list))
                                 (i 0))
                             (dolist (v vars)
                               (push `(,(first v)
                                        (var-access ,(aref animated i)))
                                     res)
                               (incf i))
                             res))
         ,@body))))
