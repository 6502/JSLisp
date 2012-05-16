(import * from gui)

(defun debug (func &rest args)
  (let ((msg ~"{func}")
        (sep "?"))
    (dolist (x args)
      (incf msg ~"{sep}{(uri-encode x)}")
      (setf sep "&"))
    (http-get ~"http://127.0.0.1:1337/{msg}")))

(setf cw (let ((x (create-element "span")))
           (set-style x
                      fontFamily "monospace"
                      %/fontSize "120")
           (setf x.textContent "x")
           (append-child document.body x)
           (let ((width x.offsetWidth))
             (remove-child document.body x)
             width)))

(defun source-window (name)
  (let ((source (split (http-get name) "\n"))
        (w (window 100 100 800 600 :title name))
        (selection-markers (list))
        (lines (list)))
    (dotimes (i (length source))
      (let ((line (create-element "div"))
            (num (create-element "span"))
            (text (create-element "span"))
            (select (create-element "div")))
        (set-style select
                   position "absolute"
                   backgroundColor "rgba(0,255,0,0.25)"
                   px/left 0
                   px/top 0
                   px/width 0
                   px/bottom 0)
        (set-style num
                   textAlign "right"
                   fontFamily "monospace"
                   %/fontSize "120"
                   fontWeight "bold"
                   color "#008"
                   display "inline-block"
                   paddingRight "4px"
                   px/width 50)
        (set-style text
                   position "relative"
                   fontFamily "monospace"
                   %/fontSize "120"
                   whiteSpace "pre")
        (set-style line
                   whiteSpace "nowrap")
        (setf num.textContent (+ i 1))
        (setf text.textContent (aref source i))
        (append-child text select)
        (append-child line num)
        (append-child line text)
        (append-child (window-client w) line)
        (push select selection-markers)
        (push line lines)))
    (setf (window-data w) (list lines selection-markers))
    (show-window w)
    w))

(defun set-selection (w location)
  (let ((from-line (1- (second location)))
        (from-col (1- (third location)))
        (to-line (1- (fourth location)))
        (to-col (1- (fifth location)))
        (lines (first (window-data w)))
        (markers (second (window-data w))))
    (setf (window-client w).scrollTop
          (max 0 (- (aref lines from-line).offsetTop
                    (/ (window-client w).offsetHeight 2))))
    (dotimes (i (length markers))
      (let ((m (aref markers i)))
        (cond
          ((or (< i from-line) (> i to-line))
           (set-style m
                      px/left 0
                      right "auto"
                      px/width 0))
          ((and (= i from-line) (< i to-line))
           (set-style m
                      px/left (* cw from-col)
                      width "auto"
                      px/right 0))
          ((and (= i to-line) (> i from-line))
           (set-style m
                      px/left 0
                      right "auto"
                      px/width (* cw to-col)))
          ((and (> i from-line) (< i to-line))
           (set-style m
                      px/left 0
                      width "auto"
                      px/right 0))
          (true
           (set-style m
                      px/left (* cw from-col)
                      right "auto"
                      px/width (* cw (- to-col from-col)))))))))

(defun main-window ()
  (let* ((w (window 300 640 400 100 :title "Debugger"))
         (step (button "Step" (lambda () (debug 'step))))
         (continue (button "Continue" (lambda () (debug 'continue))))
         (source null)
         (source-filename null)
         (layout (:H :border 8 :spacing 8
                     (:Hdiv step)
                     (:Hdiv continue)))
         (last-info null))
    (append-child (window-client w) step)
    (append-child (window-client w) continue)
    (setf (window-resize-cback w)
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (set-interval (lambda ()
                    (let ((x (debug 'info)))
                      (when (/= x last-info)
                        (setf last-info x)
                        (when (/= x "null")
                          (setf x (split x ","))
                          (let ((filename (first x))
                                (from-line (parse-value (second x)))
                                (from-col (parse-value (third x)))
                                (to-line (parse-value (fourth x)))
                                (to-col (parse-value (fifth x))))
                            (when (/= source-filename filename)
                              (setf source-filename filename)
                              (when source
                                (hide source)
                                (setf source null))
                              (setf source (source-window filename)))
                            (set-selection source (list filename from-line from-col to-line to-col)))))))
                  100)
    (show-window w)))

(main-window)