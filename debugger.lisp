(import * from gui)
(import * from chatclient)

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
  (let* ((file (http-get name))
         (source (split file "\n"))
         (w (window 100 100 800 600 title: name))
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
        (append-child w.client line)
        (push select selection-markers)
        (push line lines)))
    (setf w.data (list lines selection-markers file))
    (show-window w)
    w))

(defun set-location (w from-char to-char)
  (let* ((lines (first w.data))
         (markers (second w.data))
         (file (third w.data))
         (from-line (1- (length (replace (slice file 0 from-char) "[^\\n]" ""))))
         (to-line (1- (length (replace (slice file 0 to-char) "[^\\n]" ""))))
         (from-col 0)
         (to-col 100))
    (setf w.client.scrollTop
          (max 0 (- (aref lines from-line).offsetTop
                    (/ w.client.offsetHeight 2))))
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

(defvar *source-windows* #())

(defun location (filename from-char to-char)
  (let ((w (aref *source-windows* filename)))
    (unless w
      (setf w (source-window filename))
      (setf w.close-cback
            (lambda ()
              (setf (aref *source-windows* filename) null)))
      (setf (aref *source-windows* filename) w))
    (show-window w)
    (set-location w from-char to-char)))

(defun debug-cmd-error (x)
  (display ~"Debug-cmd-error: {x}"))

(defun debugged (x)
  (send "http://127.0.0.1:1337" "debugged" x))

(defun step ()
  (debugged 'cont))

(defun continue ()
  (debugged '(setf *debugger* false)))

(defmacro remote (x)
  (debugged `(send-debugger (list 'display (local-eval ',x)))))

(receive "http://127.0.0.1:1337" "debugger" #'eval)