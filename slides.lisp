(import * from gui)

(defvar *in* "scale(0.25,0.25)")
(defvar *on* "scale(1,1)")
(defvar *out* "scale(4,4)")

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
               px/padding 32
               px/margin 0
               px/fontSize 32
               fontFamily "Arial"
               fontWeight "bold")
    (let ((table (create-element "table"))
          (tr (create-element "tr"))
          (td (create-element "td")))
      (set-style table
                 height "100%"
                 marginLeft "auto"
                 marginRight "auto")
      (set-style td
                 verticalAlign "middle")
      (append-child table tr)
      (append-child tr td)
      (append-child td content)
      (append-child slide table)
      (append-child (. document body) slide)
      (set-style slide
                 WebkitTransition "all 0.5s ease-in-out"
                 MozTransition "all 0.5s ease-in-out"
                 transition "all 0.5s ease-in-out")
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
      slide)))

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
                 (skip false)))

(defvar *root* null)
(defvar *sequence* (list))
(defvar *index* 0)

(defun fix (x)
  (setf x (replace x "&" "&amp;"))
  (setf x (replace x "<" "&lt;"))
  (setf x (replace x ">" "&gt;"))
  (setf x (replace x "\"" "&quot;"))
  (replace x "\\[(.*?)\\]"
           "<span style=\"font-family:Courier New; color:#008000\">$1</span>"))

(defun build-slide (node)
  (let ((div (create-element "div"))
        (ul (create-element "ul")))
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
                         (set-style li color "#0000FF"))
            (set-handler li onmouseout
                         (set-style li color "#000000"))
            (set-handler li onmousedown
                         (when (/= event.button 2)
                           (event.stopPropagation)
                           (event.preventDefault)
                           (show-slide (build-slide x) 1)))))))
    div))

(defun next-slide ()
  (when (< *index* (1- (length *sequence*)))
    (incf *index*)
    (show-slide (build-slide (aref *sequence* *index*)) 1)))

(defun prev-slide ()
  (when (> *index* 0)
    (decf *index*)
    (show-slide (build-slide (aref *sequence* *index*)) -1)))

(defun load-slides ()
  (let ((lines (split (http-get "slides.txt") "\n"))
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
                                     (<= (level) level)) node)
                           (push (node node) node.children)))))))
      (do ((root (new-node)))
          ((>= i (length lines)) root)
        (push (node root) root.children)))))

(defun start ()
  (setf (. document body style overflow) "hidden")

  (let ((background (create-element "div")))
    (set-style background
               position "absolute"
               px/left 0
               px/top 0
               px/right 0
               px/bottom 0
               background "#FFFFF0")
    (append-child (. document body) background)))

(defun fullview ()
  (let ((w (window 100 100 800 600 :title "Slides"))
        (full-list (list)))
    (show-slide null 1)
    (set-style (window-client w)
               overflow "hidden"
               backgroundColor "#EEEEEE")
    (set-style (window-frame w)
               backgroundColor "#EEEEEE")
    (labels ((collect (x)
               (when (length x.children)
                 (let ((div (build-slide x))
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
                   (map #'collect x.children)))))
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
                             px/width (- (/ ww 2) rc)
                             px/height (- (/ hh 2) rc)
                             backgroundColor "#FFFFFF"
                             overflow "hidden"
                             WebkitTransform ~"scale({(/ 1 rc 0.5)},{(/ 1 rc 0.5)})"
                             MozTransform ~"scale({(/ 1 rc 0.5)},{(/ 1 rc 0.5)})"
                             WebkitTransformOrigin "0% 0%"
                             MozTransformOrigin "0% 0%"))))))
    (setf (window-close-cback w)
          (lambda ()
            (setf *sequence* (list))
            (labels ((collect (x)
                       (when (length x.children)
                         (unless x.skip
                           (push x *sequence*))
                         (dolist (y x.children)
                           (collect y)))))
              (dolist (x *root*.children)
                (collect x))
              (setf *index* 0)
              (show-slide (build-slide (aref *sequence* *index*)) 1))))
    (show-window w)))

(defun main ()
  (setf *root* (load-slides))
  (start)
  (fullview))

(main)