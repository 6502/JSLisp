(import * from gui)

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

(defobject node ((title "")
                 (content (list))
                 (div null)
                 (skip false)))

(defobject bullet (text))
(defobject para (text))
(defobject title (text))

(defvar *full-list* (list))
(defvar *sequence* (list))
(defvar *index* 0)

(defun fix (x)
  (setf x (replace x "&" "&amp;"))
  (setf x (replace x "<" "&lt;"))
  (setf x (replace x ">" "&gt;"))
  (setf x (replace x "\"" "&quot;"))
  (setf x (replace x "\\|" "<br/>"))
  (setf x (replace x "\\{(.*?)\\}"
                   "<b>$1</b>"))
  (replace x "\\[(.*?)\\]"
           "<span style=\"font-family:'Courier New',monospace; font-weight:bold; color:#008000\">$1</span>"))

(defun build-slide (node)
  (let ((div (create-element "div"))
        (ul null))
    (set-style div
               fontFamily "Arial,Helvetica,sans-serif")
    (dolist (c node.content)
      (cond
        ((title? c)
         (setf ul null)
         (let ((h1 (create-element "h1")))
           (set-style h1
                      textAlign "center"
                      fontSize "200%"
                      fontWeight "bold")
           (setf h1.innerHTML (fix c.text))
           (append-child div h1)))
        ((bullet? c)
         (unless ul
           (setf ul (create-element "ul"))
           (append-child div ul))
         (let ((li (create-element "li")))
           (setf li.innerHTML (fix c.text))
           (append-child ul li)))
        ((para? c)
         (setf ul null)
         (let ((p (create-element "p")))
           (setf p.innerHTML (fix c.text))
           (append-child div p)))
        (true (error "Unknown content type"))))
    div))

(defun load-slides (name)
  (let ((lines (split (http-get name) "\n"))
        (i 0)
        (slides (list)))
    (do ((L (aref lines (1- (incf i)))))
        ((> i (length lines)))
      (labels ((next ()
                 (setf L (aref lines (1- (incf i))))))
        (cond
          ((= L "")
           (next))
          ((= (aref L 0) "*")
           (push (new-node (slice L 1)) slides)
           (next))
          ((= (slice L 0 2) "- ")
           (let ((text (slice L 2)))
             (next)
             (do ()
                 ((/= (slice L 0 2) "  ")
                  (push (new-bullet text)
                        (last slides).content))
               (incf text (+ "|" (slice L 2)))
               (next))))
          ((= (slice L 0 2) "= ")
           (push (new-title (slice L 2))
                 (last slides).content)
           (next))
          (true
           (do ((text ""))
               ((or (> i (length lines)) (= L ""))
                (push (new-para text)
                      (last slides).content))
             (incf text (+ "|" L))
             (next))))))
    (dolist (x slides)
      (setf x.div (build-slide x)))
    slides))

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
                 (remove-child document.body x.div)))
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
                     (setf x.div container))))))
      (map #'measure *full-list*)
      (dolist (x *full-list*)
        (rescale x (* 0.9 (min (/ *width* maxw)
                               (/ *height* maxh))))))))

(defun next-slide ()
  (when (< *index* (1- (length *sequence*)))
    (incf *index*)
    (show-slide (aref *sequence* *index*).div 1)))

(defun prev-slide ()
  (when (> *index* 0)
    (decf *index*)
    (show-slide (aref *sequence* *index*).div -1)))

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
    (dolist (x *full-list*)
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
        (let ((x x))
          (set-handler glass onmousedown
                       (funcall event.preventDefault)
                       (funcall event.stopPropagation)
                       (setf x.skip (not x.skip))
                       (set-style glass backgroundColor (if x.skip
                                                            "rgba(0,0,0,0.25)"
                                                            "rgba(0,0,0,0)"))))
        (push container full-list)
        (append-child (window-client w) container)))
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
            (setf *sequence* (filter (lambda (x) (not x.skip))
                                     *full-list*))
            (setf *index* 0)
            (show-slide (aref *sequence* *index*).div 1)))
    (show-window w)))

(defun main ()
  (start)
  (setf *full-list* (load-slides "slides2.txt"))
  (rescale-slides)
  (fullview))

(main)