(import * from gui)

(defvar *in* "scale(0.25,0.25)")
(defvar *on* "scale(1,1)")
(defvar *out* "scale(4,4)")

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
                   (if (= (. event button) 2)
                       (prev-slide)
                       (next-slide)))
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
                   1000)))
  (setf *current-slide* (slide x dir))
  (set-timeout (lambda ()
                 (set-style *current-slide*
                            opacity 1.0
                            MozTransform *on*
                            WebkitTransform *on*))
               1))

(defobject node ((parent null)
                 (title "")
                 (content "")
                 (children (list))))

(defvar *current-node*)
(defvar *root*)
(defvar *history* (list))

(defun next (x)
  (let ((f (aref x.parent.children
                 (1+ (index x x.parent.children)))))
    (if (and f (length f.children))
        f
        (if (/= x.parent *root*) x.parent))))

(defun next-slide ()
  (let ((x (next *current-node*)))
    (when x
      (push *current-node* *history*)
      (setf *current-node* x)
      (show-slide (build-slide *current-node*) 1))))

(defun prev-slide ()
  (when (length *history*)
    (setf *current-node* (pop *history*))
    (show-slide (build-slide *current-node*) -1)))

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
          (let ((x x)
                (star (create-element "span")))
            (append-child li star)
            (setf star.innerHTML "<sup>*</sup>")
            (set-style star
                       cursor "pointer"
                       color "#0000FF")
            (set-handler star onmousedown
                         (when (/= event.button 2)
                           (event.stopPropagation)
                           (event.preventDefault)
                           (push *current-node* *history*)
                           (setf *current-node* x)
                           (show-slide (build-slide x) 1)))))))
    div))

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

(setf (. document body style overflow) "hidden")

(let ((background (create-element "div")))
  (set-style background
             position "absolute"
             px/left 0
             px/top 0
             px/right 0
             px/bottom 0
             background "#FFFFE0")
  (append-child (. document body) background))

(setf *root* (load-slides))
(setf *current-node* (first *root*.children))
(show-slide (build-slide *current-node*) 1)
