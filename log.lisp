(import * from gui)
(import * from layout)

(defun timestamp (x)
  (labels ((str (value n)
             (let ((res ~"0000{value}"))
               (slice res (- (length res) n)))))
    (let ((YYYY (str (funcall (. x getFullYear)) 4))
          (MM (str (1+ (funcall (. x getMonth))) 2))
          (DD (str (funcall (. x getDate)) 2))
          (hh (str (funcall (. x getHours)) 2))
          (mm (str (funcall (. x getMinutes)) 2))
          (ss (str (funcall (. x getSeconds)) 2))
          (mss (str (funcall (. x getMilliseconds)) 3)))
      ~"{YYYY}-{MM}-{DD} {hh}:{mm}:{ss}.{mss}")))

(defvar *log* (list))
(defvar *logwindow* null)
(defvar *severity-colors* (list "#000000"
                                "#800080"
                                "#C00000"
                                "#FF0000"))

(defconstant INFO    0)
(defconstant WARNING 1)
(defconstant ERROR   2)
(defconstant FATAL   3)

(defun add-row (severity msg)
  (let ((x (create-element "div")))
    (set-style x
               color (aref *severity-colors* severity)
               whiteSpace "nowrap"
               fontFamily "Arial"
               px/fontSize 16
               fontWeight "bold"
               px/padding 1)
    (setf (. x textContent) msg)
    (append-child *logwindow*.data x)))

(defun write (msg &key (severity 0))
  (let ((x (+ (timestamp (js-code "(new Date)")) " -- " msg)))
    (push (list severity x)
          *log*)
    (when *logwindow*
      (add-row severity x))))

(defvar *x0* 100)
(defvar *y0* 100)
(defvar *width* 600)
(defvar *height* 400)

(defun show ()
  (unless *logwindow*
    (let* ((scroll (create-element "div"))
           (clear (button "Clear"
                          (lambda ()
                            (setf *log* (list))
                            (do ()
                                ((not (. scroll firstChild)))
                              (remove-child scroll (. scroll firstChild))))))
           (layout (border 8
                     (V spacing: 8
                       (dom scroll)
                       size: 30
                       (H null size: 80 (dom clear) size: undefined null)))))
      (setf *logwindow* (window *x0* *y0* *width* *height*
                                title: "Log window"))
      (set-style scroll
                 position "absolute"
                 backgroundColor "#EEE"
                 overflow "auto")
      (set-style *logwindow*.client
                 overflow "hidden")
      (append-child *logwindow*.client scroll)
      (append-child *logwindow*.client clear)
      (setf *logwindow*.data scroll)
      (map (lambda (x) (apply #'add-row x)) *log*)
      (setf *logwindow*.resize-cback
            (lambda (x0 y0 x1 y1)
              (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
      (setf *logwindow*.close-cback
            (lambda ()
              (setf *x0* (. *logwindow*.frame offsetLeft))
              (setf *y0* (. *logwindow*.frame offsetTop))
              (setf *width* (. *logwindow*.frame offsetWidth))
              (setf *height* (. *logwindow*.frame offsetHeight))
              (setf *logwindow* null)))
      (show-window *logwindow*))))

(export write show *log*
        INFO WARNING ERROR FATAL)
