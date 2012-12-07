(import * from gui)
(import * from layout)
(import * from graphics)
(import * from editor)
(import (mode) from editor-lispmode)
(import ilisp)
(import * from rpc-client)
(import (hash) from crypto)

(defvar *ilisp*)

(defun customize-styles (styles &optional cback)
  (let** ((w (window 0 0 600 (+ 100 (* 48 (length (keys styles))))
                     title: "Syntax highlight"))
          (styles-area (V spacing: 8))
          (widgets #())
          (ok (add-widget w (button "OK" #'ok)))
          (cancel (add-widget w (button "Cancel" #'cancel)))
          (layout (V border: 8 spacing: 8
                     styles-area
                     size: 30
                     (H :filler:
                        (dom ok)
                        (dom cancel)
                        :filler:)))
          (#'ok ()
            (dolist (k (keys styles))
              (let (((color background underline) (map #'text (aref widgets k))))
                (setf (aref styles k "color") (or color undefined))
                (setf (aref styles k "background") (or background undefined))
                (setf (aref styles k "underline") (or underline undefined))))
            (hide-window w)
            (when cback (funcall cback true)))
          (#'cancel ()
            (hide-window w)
            (when cback (funcall cback false))))
    (set-layout w layout)
    (dolist (k (keys styles))
      (let ((label (add-widget w (set-style (create-element "div")
                                            whiteSpace "pre"
                                            fontFamily "Arial"
                                            textAlign "right"
                                            fontWeight "bold")))
            (color (add-widget w (css-color-input "color")))
            (background (add-widget w (css-color-input "background")))
            (underline (add-widget w (css-color-input "underline"))))
        (setf label.textContent k)
        (setf (text color) (or (aref styles k "color") ""))
        (setf (text background) (or (aref styles k "background") ""))
        (setf (text underline) (or (aref styles k "underline") ""))
        (color.update-style)
        (background.update-style)
        (underline.update-style)
        (add-element styles-area
                     size: 40
                     (H spacing: 8
                        (V :filler: size: 20 (dom label))
                        (dom color)
                        (dom background)
                        (dom underline)))
        (setf (aref widgets k) (list color background underline))))
    (show-window w center: true)))

(defun src-tab (name content)
  (let ((editor (editor name
                        content
                        (if (or (= (slice name -5) ".lisp")
                                (= name "*scratch*"))
                            mode
                            undefined))))
    (set-style editor
               position "absolute")
    (setf editor.ilisp-exec
          (lambda ()
            (let ((lines (editor.lines))
                  (row (first (editor.pos)))
                  (txt (editor.selection)))
              (when (and (= txt "") mode.toplevel-sexpr)
                (setf txt (mode.toplevel-sexpr lines row)))
              (when (/= txt "")
                (*ilisp*.send "lisp" txt)))))
    editor))

(defun inferior-lisp ()
  (let** ((container (set-style (create-element "div")
                                position "absolute"))
          (inspect (append-child container (button "Inspect" #'inspect)))
          (reset (append-child container (button "Reset" #'reset)))
          (clear (append-child container (button "Clear" #'clear)))
          (ilisp (ilisp:new #'reply))
          (#'inspect ()
                     (mode.inspect-ilisp ilisp))
          (#'reply (msg)
                   (when (= msg "\"ready\"")
                     (inspect))
                   (ilisp.send "javascript"
                               (+ "output(f$$str_value(f$$json_parse$42_("
                                  (json msg)
                                  "))+\"\\n\")")))
          (#'reset ()
                   (ilisp.reset))
          (#'clear ()
                   (ilisp.send "javascript"
                               "repl.value=\"\""))
          (layout (V border: 8 spacing: 8
                     (dom ilisp.iframe)
                     size: 30
                     (H :filler:
                        size: 80
                        (dom reset)
                        (dom clear)
                        (dom inspect)
                        :filler:))))
    (setf ilisp.clear #'clear)
    (append-child container ilisp.iframe)
    (set-style ilisp.iframe
               position "absolute"
               border "solid 1px #CCCCCC"
               px/padding 0
               px/margin -1
               opacity 1)
    (setf container."data-resize"
          (lambda (x0 y0 x1 y1)
            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (setf container.ilisp ilisp)
    container))

(defvar *user*)
(defvar *secret*)
(defvar *session-id*)

(rpc:defun login (user-name))
(rpc:defun rget-file (user-name session-id filename authcode))
(rpc:defun rput-file (user-name session-id filename content authcode))
(rpc:defun rlist-files (user-name session-id path authcode))
(rpc:defun rping (user-name session-id authcode))

(defun get-file (name)
  (rget-file *user* *session-id* name
             (hash (+ *session-id* *secret* (json* name)))))

(defun put-file (name content)
  (when (rput-file *user* *session-id* name content
                   (hash (+ *session-id* *secret* (json* (list name content)))))
    (baloon ~"{(htm name)} saved")))

(defun list-files (path)
  (rlist-files *user* *session-id* path
               (hash (+ *session-id* *secret* (json* path)))))

(defun ping ()
  (rping *user* *session-id* (hash (+ *session-id* *secret* "null"))))

(defun file-browser (cback)
  (let** ((w (set-style (create-element "div")
                        position "absolute"))
          (user (append-child w (input "username")))
          (password (append-child w (input "password")))
          (current-path (append-child w (input "current path")))
          (files (append-child w (set-style (create-element "div")
                                            position "absolute"
                                            border  "solid 1px #000000"
                                            overflow "auto"
                                            px/margin -2
                                            px/padding 2)))
          (last-search null)
          (filelist (list))
          (#'pathexp (s)
            (let* ((last-sep (last-index "/" s))
                   (base (if (= last-sep -1) "./" (slice s 0 (1+ last-sep))))
                   (name (slice s (1+ last-sep))))
              (list base name)))
          (#'filter ()
            (let ((search (slice (text current-path)
                                 0 current-path.lastChild.selectionStart)))
              (when (/= last-search search)
                (setf last-search search)
                (let (((base name) (pathexp last-search)))
                  (setf files.textContent "")
                  (setf filelist (list))
                  (dolist (f (or (list-files base) ""))
                    (when (and (/= (last f) "~")
                               (/= (first f) ".")
                               (= (slice f 0 (length name)) name))
                      (push f filelist)
                      (let ((d (append-child files (set-style (create-element "div")
                                                              px/fontSize 18
                                                              fontFamily "monospace"
                                                              whiteSpace "pre"
                                                              px/padding 2))))
                        (setf d.textContent f))))
                  (when (and (> (length filelist) 0)
                             (= (length (text current-path))
                                current-path.lastChild.selectionStart
                                current-path.lastChild.selectionEnd))
                    (let ((i (length (first filelist))))
                      (dolist (x filelist)
                        (do () ((= (slice (first filelist) 0 i)
                                   (slice x 0 i)))
                          (decf i)))
                      (when (> i (length name))
                        (when (= base "./")
                          (setf base ""))
                        (setf (text current-path) (+ base (slice (first filelist) 0 i)))
                        (current-path.lastChild.setSelectionRange
                          (+ (length base) (length name))
                          (+ (length base) i)))))))))
          (#'enter ()
            (funcall cback (text current-path)))
          (layout (V border: 8 spacing: 8
                     size: 40
                     (H size: 120
                        (dom user)
                        (dom password)
                        size: undefined
                        (dom current-path))
                     size: undefined
                     (dom files))))
    (setf current-path.onkeydown
          (lambda (event)
            (when (= event.which 13)
              (event.stopPropagation)
              (event.preventDefault)
              (if (/= current-path.lastChild.selectionStart
                      current-path.lastChild.selectionEnd)
                  (current-path.lastChild.setSelectionRange
                    (length (text current-path))
                    (length (text current-path)))
                  (enter)))))
    (setf password.lastChild.type "password")
    (setf password.lastChild.onblur (lambda ()
                                      (setf *user* (text user))
                                      (setf *secret* (hash (text password)))
                                      (setf *session-id* (login *user*))))
    (set-interval #'filter 250)
    (setf w."data-resize" (lambda (x0 y0 x1 y1)
                            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (setf w.focus (lambda ()
                    ((cond
                       ((= (text user) "") user)
                       ((= (text password) "") password)
                       (true current-path)).lastChild.focus)))
    w))

(defun main ()
  (set-style document.body
             fontFamily "Droid Serif"
             px/border 0
             px/margin 0)
  (let** ((w (set-style (append-child document.body
                                      (create-element "div"))
                        position "absolute"
                        overflow "hidden"
                        px/left 0
                        px/right 0
                        px/top 0
                        px/bottom 0))
          (last-width 0)
          (last-height 0)
          (sources (tabbed))
          (ilisp (inferior-lisp))
          (doc (set-style (create-element "div")
                          position "absolute"
                          overflow "auto"))
          (hs (set-style (h-splitter ilisp doc)
                         position "absolute"))
          (vs (append-child w (v-splitter sources hs split: 80)))
          (zoom false)
          (zrun "")
          (#'show-doc (x)
            (setf x (json-parse x))
            (when (and x (list? (first x)) (string? (first (first x))))
              (let ((location (second (first x))))
                (setf x (first (first x)))
                (setf x (replace x "&" "&amp;"))
                (setf x (replace x "<" "&lt;"))
                (setf x (replace x ">" "&gt;"))
                (setf x (replace x "\"" "&quot;"))
                (setf x (replace x "\\n" "<br/>"))
                (setf x (replace x "\\[\\[((.|[\\n])*?)\\]\\]"
                                 "<pre style=\"color:#008;\
                                 font-weight:bold;\
                                 font-size:110%\">$1</pre>"))
                (setf x (replace x "\\[(.*?)\\]"
                                 "<span style=\"font-weight:bold;\
                                 font-family:monospace;\
                                 color:#008\">$1</span>"))
                (setf x (replace x "{{(.*?)}}"
                                 "<a href=\"javascript:showdoc('$1')\">\
                                 <span style=\"font-weight:bold;\
                                 text-decoration:underline;\
                                 font-family:monospace;\
                                 color:#00F\">$1</span></a>"))
                (setf doc.innerHTML (+ x "<br/>"))
                (when location
                  (let ((btn (create-element "input")))
                    (setf btn.type "button")
                    (setf btn.value "View source")
                    (setf btn.onclick (lambda ()
                                        (view-source location)))
                    (append-child doc btn))))))
          (#'view-source ((file c0 c1))
            (declare (ignorable c1))
            (let ((content (get-file file)))
              (let ((editor (src-tab file content))
                    (line (1- (length (split (slice content 0 c0) "\n")))))
                (editor.set-pos (max 0 (- line 10)) 0
                                (max 0 (- line 10)) 0)
                (set-timeout (lambda () (editor.set-pos line 0 line 0))
                             100)
                (sources.add file editor true)
                (sources.select 0)
                (sources.prev))))
          (#'resize ()
            (when (or (/= last-width (screen-width))
                      (/= last-height (screen-height)))
              (setf last-width (screen-width))
              (setf last-height (screen-height))
              (set-coords (dom vs) 8 8 (- last-width 8) (- last-height 8))))
          (#'doc-lookup (name)
            (*ilisp*.send
             "lisp"
             ~"(let ((f (intern {(json name)} undefined true)))
                 (when f
                   (let ((f (or (symbol-function f) (symbol-macro f))))
                     (if f (list (documentation f) f.location)))))"
             #'show-doc))
          (#'zoom ()
            (setf zoom (not zoom))
            (vs.partition (if zoom 0 80))
            (hs.partition (if zoom 100 50))
            (if zoom
                (*ilisp*.focus)
                (when (sources.current).focus
                  ((sources.current).focus)))))

    (set-interval #'resize 100)
    (resize)

    (setf (js-code "window").unzoom #'zoom)

    ;; session keep-alive
    (set-interval (lambda () (when *secret* (ping)))
                  5000)

    (setf (js-code "window").showdoc #'doc-lookup)
    (setf *ilisp* ilisp.ilisp)

    (sources.add
      "+"
      (file-browser
        (lambda (f)
          (let ((editor (src-tab f (or (get-file f) ""))))
            (sources.add f editor true)
            (setf editor.remove
                  (lambda ()
                    (if (editor.modified)
                        (progn
                          (message-box
                            "<h2>File changes not saved. Quit anyway?</h2>
                             There are changes to the current file that have
                             not been saved yet back to server (key: ctrl-W).<br/><br/>
                             If you close this tab these changes will be lost."
                            title: "Abandon edit warning"
                            buttons: (list "Yes" "No")
                            default: "No"
                            modal: true
                            cback: (lambda (reply)
                                     (when (= reply "Yes")
                                       (editor.clear-modified)
                                       (sources.remove (sources.page-index editor)))))
                          false)
                        true)))
            (sources.select 0)
            (sources.prev)))))

    (sources.add
      "*scratch*"
      (src-tab "*scratch*" "")
      true)
    (sources.select 1)

    (set-timeout (lambda () ((sources.current).focus)) 10)

    (document.body.addEventListener
     "keydown"
     (lambda (event)
       (let ((stop true))
         (cond
           ((and (or event.altKey event.metaKey) (= event.which #.(char-code "Z")))
            (*ilisp*.clear))
           ((and (or event.altKey event.metaKey) (= event.which #.(char-code "R")))
            (*ilisp*.reset))
           ((and event.ctrlKey (= event.which #.(char-code "I")))
            (mode.inspect-ilisp *ilisp*)
            (set-timeout (lambda ()
                           (when (sources.current).refresh
                             ((sources.current).refresh)))
                         100))
           ((and event.ctrlKey (= event.which #.(char-code "W")))
            (when (and (> (sources.current-index) 0)
                       (put-file ((sources.current).name)
                                 ((sources.current).buffer)))
              ((sources.current).clear-modified)))
           ((and event.ctrlKey (= event.which #.(char-code "Q")))
            (when (> (sources.current-index) 0)
              (sources.remove (sources.current-index))))
           ((and event.ctrlKey (= event.which #.(char-code "K")))
            (when ((sources.current).selection)
              (setf zrun ((sources.current).selection)))
            (zoom)
            (when zrun
              (*ilisp*.send "lisp" zrun)))
           ((and event.ctrlKey (= event.which #.(char-code "O"))
                 mode.styles)
            (customize-styles mode.styles
                              (lambda (res)
                                (when (and res
                                           (sources.current).refresh)
                                  ((sources.current).refresh)))))
           ((and event.ctrlKey (= event.which 39))
            (sources.next))
           ((and event.ctrlKey (= event.which 37))
            (sources.prev))
           ((and event.ctrlKey (= event.which 13))
            (when event.shiftKey
              (zoom))
            ((sources.current).ilisp-exec))
           (true (setf stop false)))
         (when stop
           (event.stopPropagation)
           (event.preventDefault))))
     true)

    (set-interval
     (let ((last-lookup ""))
       (lambda ()
         (when (and (sources.current) (sources.current).pos)
           (let* (((row col) ((sources.current).pos))
                  (lines ((sources.current).lines))
                  (text (aref lines row).text))
             (do ((c col (1- c)))
                 ((or (= c 0)
                      (= (aref text (1- c)) "("))
                    (let ((name (slice text c col)))
                      (when (/= name last-lookup)
                        (setf last-lookup name)
                        (doc-lookup name)))))))))
     100)))

(defvar *deploy-prefix* "")
(setf *deploy-prefix* (replace *deploy-prefix* "</head>" "<title>JsLisp IDE</title></head>"))

(main)
