(import * from gui)

(setf #'display #'alert)

(defvar *in* "scale(0.25,0.25)")
(defvar *on* "scale(1,1)")
(defvar *out* "scale(4,4)")
(defvar *background*)
(defvar *width*)
(defvar *height*)

(defun next-slide ())
(defun prev-slide ())
(defun fullview ())

(defun slide (content dir)
  (let ((slide (create-element "div")))
    (set-style slide
               position "absolute"
               opacity 0.0
               WebkitTransform (if (= dir 1) *in* *out*)
               MozTransform (if (= dir 1) *in* *out*)
               px/left 0
               px/top 0
               px/right 0
               px/bottom 0
               WebkitTransition "all 0.5s ease-in-out"
               MozTransition "all 0.5s ease-in-out"
               transition "all 0.5s ease-in-out")
    (append-child slide content)
    (append-child (. document body) slide)
    (set-handler slide oncontextmenu
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation)))
    (set-handler slide onmousedown
                 (funcall (. event preventDefault))
                 (funcall (. event stopPropagation))
                 (cond
                   ((= event.button 0) (next-slide))
                   ((= event.button 1) (fullview))
                   ((= event.button 2) (prev-slide))))
    slide))

(defvar *current-slide*)

(defun show-slide (x dir)
  (when *current-slide*
    (let ((old *current-slide*))
      (set-style *current-slide*
                 opacity 0.0
                 MozTransform (if (= dir 1) *out* *in*)
                 WebkitTransform (if (= dir 1) *out* *in*))
      (set-timeout (lambda () (remove-child (. document body) old))
                   1000))
    (setf *current-slide* null))
  (when x
    (setf *current-slide* (slide x dir))
    (set-timeout (lambda ()
                   (set-style *current-slide*
                              opacity 1.0
                              MozTransform *on*
                              WebkitTransform *on*))
                 1)))

(defobject node ((parent null)
                 (title "")
                 (content "")
                 (children (list))
                 (div null)
                 (skip false)))

(defvar *root* null)
(defvar *sequence* (list))
(defvar *index* 0)

(defun fix (x)
  (setf x (replace x "&" "&amp;"))
  (setf x (replace x "<" "&lt;"))
  (setf x (replace x ">" "&gt;"))
  (setf x (replace x "\"" "&quot;"))
  (setf x (replace x "\\|" "<br/>"))
  (replace x "\\[(.*?)\\]"
           "<span style=\"font-family:'Courier New',monospace; font-weight:bold; color:#008000\">$1</span>"))

(defun build-slide (node)
  (let ((div (create-element "div"))
        (ul (create-element "ul")))
    (set-style div
               fontFamily "Arial,Helvetica,sans-serif")
    (let* ((title node.title)
           (i (index " / " title)))
      (if (>= i 0)
          (let ((h1 (create-element "h1")))
            (append-child div h1)
            (set-style h1 textAlign "center")
            (setf h1.innerHTML (fix (slice title 0 i)))
            (let ((h3 (create-element "h3")))
              (set-style h3 textAlign "center")
              (setf h3.innerHTML (fix (slice title (+ i 3))))
              (append-child div h3)))
          (let ((h2 (create-element "h2")))
            (append-child div h2)
            (setf h2.innerHTML (fix node.title)))))
    (append-child div ul)
    (dolist (x node.children)
      (let ((li (create-element "li")))
        (append-child ul li)
        (setf li.innerHTML (fix x.content))
        (when (length x.children)
          (let ((x x))
            (set-style li cursor "pointer")
            (set-handler li onmouseover
                         (set-style li backgroundColor "#C0FFC0"))
            (set-handler li onmouseout
                         (set-style li backgroundColor "transparent"))
            (set-handler li onmousedown
                         (when (/= event.button 2)
                           (event.stopPropagation)
                           (event.preventDefault)
                           (show-slide x.div 1)))))))
    div))

(defun rescale-slides ()
  (let ((maxw 0)
        (maxh 0))
    (labels ((measure (x)
               (when x.div
                 (set-style x.div
                            position "absolute"
                            whiteSpace "nowrap"
                            px/top 0
                            px/left 0)
                 (append-child document.body x.div)
                 (setf maxw (max maxw x.div.offsetWidth))
                 (setf maxh (max maxh x.div.offsetHeight))
                 (remove-child document.body x.div))
               (dolist (y x.children)
                 (measure y)))
             (rescale (x sf)
               (when x.div
                 (append-child document.body x.div)
                 (let ((w x.div.offsetWidth)
                       (h x.div.offsetHeight))
                   (set-style x.div
                              px/left (/ (- *width* (* sf w)) 2)
                              px/top (/ (- *height* (* sf h)) 2)
                              WebkitTransformOrigin "0% 0%"
                              MozTransformOrigin "0% 0%"
                              WebkitTransform ~"scale({sf},{sf})"
                              MozTransform ~"scale({sf},{sf})")
                   (remove-child document.body x.div)
                   (let ((container (create-element "div")))
                     (set-style container
                                position "absolute"
                                px/left 0
                                px/top 0
                                px/width *width*
                                px/height *height*)
                     (append-child container x.div)
                     (setf x.div container))))
               (dolist (y x.children)
                 (rescale y sf))))
      (measure *root*)
      (rescale *root* (* 1.2 (min (/ *width* maxw)
                                  (/ *height* maxh)))))))

(defun next-slide ()
  (when (< *index* (1- (length *sequence*)))
    (incf *index*)
    (show-slide (aref *sequence* *index*).div 1)))

(defun prev-slide ()
  (when (> *index* 0)
    (decf *index*)
    (show-slide (aref *sequence* *index*).div -1)))

(defun load-slides ()
  (let ((lines (filter (lambda (x) (/= x ""))
                       (split (http-get "slides.txt") "\n")))
        (i 0))
    (labels ((L ()
                (aref lines i))
             (level ()
                    (do ((i 0 (1+ i)))
                        ((/= (aref (L) i) " ") i)))
             (node (parent)
                   (let* ((level (level))
                          (content (slice (L) level))
                          (title content))
                     (let ((t-start (index " {" content))
                           (t-end (index "}" content)))
                       (when (and (>= t-start 0)
                                  (= t-end (1- (length content))))
                         (setf title (slice content (+ 2 t-start) t-end))
                         (setf content (slice content 0 t-start)))
                       (let ((node (new-node parent title content)))
                         (incf i)
                         (do () ((or (>= i (length lines))
                                     (<= (level) level))
                                   (setf node.div (build-slide node))
                                   node)
                           (push (node node) node.children)))))))
      (do ((root (new-node)))
          ((>= i (length lines)) root)
        (push (node root) root.children)))))

(defun start ()
  (setf (. document body style overflow) "hidden")
  (setf *background* (create-element "div"))
  (set-style *background*
             position "absolute"
             px/left 0
             px/top 0
             px/right 0
             px/bottom 0
             background "#FFFFF0")
  (append-child (. document body) *background*)
  (setf *width* *background*.offsetWidth)
  (setf *height* *background*.offsetHeight))

(defun fullview ()
  (let ((w (window 10 10
                   (- *background*.offsetWidth 20)
                   (- *background*.offsetHeight 20)
                   :title "Slides"))
        (full-list (list)))
    (show-slide null -1)
    (set-style (window-client w)
               overflow "hidden"
               backgroundColor "#EEEEEE")
    (set-style (window-frame w)
               backgroundColor "#EEEEEE")
    (labels ((collect (x)
               (let ((div x.div)
                     (glass (create-element "div"))
                     (container (create-element "div")))
                 (append-child container div)
                 (append-child container glass)
                 (set-style glass
                            position "absolute"
                            px/left 0
                            px/top 0
                            width "100%"
                            height "100%"
                            backgroundColor (if x.skip
                                                "rgba(0,0,0,0.25)"
                                                "rgba(0,0,0,0)"))
                 (set-handler glass onmousedown
                              (funcall event.preventDefault)
                              (funcall event.stopPropagation)
                              (setf x.skip (not x.skip))
                              (set-style glass backgroundColor (if x.skip
                                                                   "rgba(0,0,0,0.25)"
                                                                   "rgba(0,0,0,0)")))
                 (push container full-list)
                 (map #'collect
                      (filter (lambda (x) (length x.children))
                              x.children)))))
      (map #'collect *root*.children))
    (dolist (x full-list)
      (append-child (window-client w) x))
    (setf (window-resize-cback w)
          (lambda (x0 y0 x1 y1)
            (let* ((n (length full-list))
                   (ww (- x1 x0))
                   (hh (- y1 y0))
                   (rc (1+ (floor (sqrt (1- n)))))
                   (w1 (/ ww rc))
                   (h1 (/ hh rc)))
              (dotimes (i (length full-list))
                (let ((div (aref full-list i)))
                  (set-style div
                             position "absolute"
                             px/left (* w1 (% i rc))
                             px/top (* h1 (floor (/ i rc)))
                             px/width (- ww rc)
                             px/height (- hh rc)
                             backgroundColor "#FFFFFF"
                             overflow "hidden"
                             WebkitTransform ~"scale({(/ 1 rc)},{(/ 1 rc)})"
                             MozTransform ~"scale({(/ 1 rc)},{(/ 1 rc)})"
                             WebkitTransformOrigin "0% 0%"
                             MozTransformOrigin "0% 0%"))))))
    (setf (window-close-cback w)
          (lambda ()
            (setf *sequence* (list))
            (labels ((collect (x)
                       (unless x.skip
                         (push x *sequence*))
                       (dolist (y (filter (lambda (x)
                                            (length x.children))
                                          x.children))
                         (collect y))))
              (dolist (x *root*.children)
                (collect x))
              (setf *index* 0)
              (show-slide (aref *sequence* *index*).div 1))))
    (show-window w)))

(defun main ()
  (start)
  (setf *root* (load-slides))
  (rescale-slides)
  (fullview))

(main)