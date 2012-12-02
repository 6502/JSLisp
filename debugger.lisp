(import * from gui)
(import * from layout)
(import * from chatclient)

;;
;; DEBUGGER ==> DEBUGGED_1 (async)
;; -----------------------
;; add-breakpoint (source from to)
;; remove-breakpoint (source from to)
;; add-watch (expr)
;; remove-watch (expr)
;;
;; DEBUGGER ==> DEBUGGED
;; ---------------------
;; step
;; continue
;; evaluate
;;
;; DEBUGGED ==> DEBUGGER
;; ---------------------
;; debug-cmd-error (x)
;; stopped (stack watches)
;; exception (err stack watches)
;; running
;;

(setf cw (let ((x (create-element "span")))
           (set-style x
                      fontFamily "monospace"
                      %/fontSize "120")
           (setf x.textContent "x")
           (append-child document.body x)
           (let ((width x.offsetWidth))
             (remove-child document.body x)
             width)))

(defun debugged (x)
  (send "http://127.0.0.1:1337" "debugged" x))

(defun debugged_i (x)
  (send "http://127.0.0.1:1337" "debugged_i" x))

(defvar *source-windows* #())
(defvar *debugger-window*)

(defun set-location (w from-char to-char &optional (scroll true))
  (let* ((lines (first w.data))
         (markers (second w.data))
         (file (third w.data))
         (before-lines (split (slice file 0 from-char) "\n"))
         (from-line (1- (length before-lines)))
         (from-col (length (or (last before-lines) "")))
         (after-lines (split (slice file 0 to-char) "\n"))
         (to-line (1- (length after-lines)))
         (to-col (length (or (last after-lines) ""))))
    (when scroll
      (setf w.client.scrollTop
            (max 0 (- (aref lines from-line).offsetTop
                      (/ w.client.offsetHeight 2)))))
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
                   cursor "pointer"
                   whiteSpace "pre")
        (set-style line
                   whiteSpace "nowrap")
        (setf num.textContent (+ i 1))
        (setf text.textContent (aref source i))
        (append-child text select)
        (append-child line num)
        (append-child line text)
        (append-child w.client line)
        (set-handler text onmousedown
          (event.stopPropagation)
          (event.preventDefault)
          (let* ((x (first (relative-pos event text)))
                 (cw (/ text.offsetWidth (length text.textContent)))
                 (ci (max 0 (floor (/ x cw))))
                 (p (+ (length (join (slice source 0 (1- (atoi num.textContent))) "\n"))
                       ci)))
            (do () ((or (<= p 0) (= (aref file p) "(")))
              (decf p))
            (when (= (aref file p) "(")
              (do ((x (1+ p) (1+ x))
                   (bal 0))
                  ((or (>= x (length file))
                       (and (= (aref file x) ")")
                            (= bal 0)))
                     (when (= (aref file x) ")")
                       (*debugger-window*.add-breakpoint name p (1+ x))
                       (set-location w p (1+ x) false)
                       (set-timeout (lambda ()
                                      (set-location w p p false))
                                    500)))
                (cond
                  ((= (aref file x) "(")
                   (incf bal))
                  ((= (aref file x) ")")
                   (decf bal)))))))
        (push select selection-markers)
        (push line lines)))
    (setf w.data (list lines selection-markers file))
    (show-window w)
    w))

(defun show-source (filename from-char to-char)
  (let ((w (aref *source-windows* filename)))
    (unless w
      (setf w (source-window filename))
      (setf w.close-cback
            (lambda ()
              (setf (aref *source-windows* filename) null)))
      (setf (aref *source-windows* filename) w))
    (show-window w)
    (set-location w from-char to-char)))

(defun main ()
  (let** ((w (window 0 0 400 400 title: "Debugger"))
          (step (add-widget w (button "step" #'step)))
          (cont (add-widget w (button "cont" #'continue)))
          (load (add-widget w (button "load" #'load)))
          (br-label (add-widget w (label "breakpoints")))
          (breakpoints (add-widget w (set-style (create-element "div")
                                                fontFamily "monospace"
                                                border "solid 1px #CCCCCC"
                                                fontWeight "bold"
                                                whiteSpace "pre"
                                                overflow "auto")))
          (w-label (add-widget w (label "watches")))
          (watches (add-widget w (set-style (create-element "div")
                                            fontFamily "monospace"
                                            border "solid 1px #CCCCCC"
                                            whiteSpace "pre"
                                            overflow "auto")))
          (s-label (add-widget w (label "call stack")))
          (cstack (add-widget w (set-style (create-element "div")
                                           fontFamily "monospace"
                                           border "solid 1px #CCCCCC"
                                           whiteSpace "pre"
                                           overflow "auto")))
          (#'step ()
            (debugged 'step))
          (#'continue ()
            (debugged `(setf *debugger* false)))
          (#'load ()
            (let ((fname (prompt "Filename")))
              (when fname
                (show-source fname 0 0))))
          (#'setcontext (stack ww)
            (setf cstack.textContent "")
            (setf watches.textContent "")
            (dolist ((file from to) (reverse stack))
              (let ((sdiv (create-element "div")))
                (setf sdiv.textContent ~"{file}:{from}-{to}")
                (append-child cstack sdiv)
                (setf sdiv.onclick (lambda ()
                                     (show-source file from to)))))
            (dolist (w ww)
              (let ((wdiv (create-element "div")))
                (setf wdiv.textContent w)
                (append-child watches wdiv))))
          (#'add-breakpoint (file from to)
            (debugged_i `(add-breakpoint ',(list file from to)))
            (let ((r (create-element "div")))
              (setf r.textContent ~"{file}:{from}-{to}")
              (append-child breakpoints r)
              (setf r.onclick (lambda ()
                                (remove-child breakpoints r)
                                (debugged_i `(remove-breakpoint ',(list file from to))))))))
    (set-layout w (V border: 8 spacing: 8
                     size: 40
                     (H (dom step) (dom cont) (dom load))
                     size: 80
                     (V spacing: 0 size: 15
                        (dom br-label)
                        size: undefined
                        (dom breakpoints))
                     size: undefined
                     (H (V spacing: 0 size: 15
                           (dom w-label)
                           size: undefined
                           (dom watches))
                        (V spacing: 0 size: 15
                           (dom s-label)
                           size: undefined
                           (dom cstack)))))
    (setf *debugger-window* w)
    (setf w.add-breakpoint #'add-breakpoint)
    (receive "http://127.0.0.1:1337" "debugger"
             (lambda (x)
               (case (and x (first x))
                 ('debug-cmd-error
                    (alert (second x)))
                 ('stopped
                    (let (((stack watches) (rest x)))
                      (setcontext stack watches)
                      (apply #'show-source (last stack))
                      (setf w.titlebar.textContent "Stopped!")))
                 ('exception
                    (let (((err stack watches) (rest x)))
                      (setcontext stack watches)
                      (apply #'show-source (last stack))
                      (setf w.titlebar.textContent ~"Exception: {err}")))
                 ('running
                    (setf w.titlebar.textContent "... running ...")
                    (setcontext (list) (list))))))
    (show-window w)))

(main)