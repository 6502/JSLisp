(import * from gui)
(import * from layout)
(import * from graphics)
(import * from editor)
(import (mode) from editor-lispmode)
(import ilisp)
(import * from rpc-client)
(import (hash) from crypto)
(import * from guieditor)

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
          (first null)
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
        (unless first
          (setf first color))
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
    (when first
      (focus first))
    (show-window w center: true)))

(defun src-tab (name content)
  (let ((editor (editor name
                        content
                        (if (or (= (slice name -5) ".lisp")
                                (= name "*scratch*"))
                            mode
                            undefined)))
        (basename (let ((i (last-index "." name)))
                    (if (>= i 0)
                        (slice name 0 i)
                        name))))
    (set-style editor
               position "absolute")
    (setf editor.ilisp-exec
          (lambda (&optional expr)
            (let ((lines (editor.lines))
                  (row (first (editor.pos)))
                  (txt (or expr (editor.selection))))
              (when (and (= txt "") mode.toplevel-sexpr)
                (setf txt (mode.toplevel-sexpr lines row)))
              (when (/= txt "")
                (*ilisp*.send "quiet-lisp" ~"(in-module {(json basename)})")
                (*ilisp*.send "lisp" txt)))))
    editor))

(defun inferior-lisp ()
  (let** ((container (set-style (create-element "div")
                                position "absolute"))
          (ilisp (ilisp:new #'reply))
          (#'inspect ()
            (mode.inspect-ilisp ilisp))
          (#'reply (msg)
            (if (and (string? msg)
                     (= (slice msg 0 11) "[\"ctxmenu:\""))
                (let (((x y) (rest (json-parse msg)))
                      ((x0 y0) (element-pos container)))
                  (contextmenu (list (+ x x0) (+ y y0))))
                (progn
                  (when (= msg "\"ready\"")
                    (ilisp.send "javascript"
                                "repl.value=\";; JsLisp Ready.\\n\";")
                    (inspect)))))
          (#'reset ()
            (ilisp.reset))
          (#'clear ()
            (ilisp.send "javascript"
                        "repl.value=\"\""))
          (#'contextmenu (pos)
            (menu `(("Reset inferior lisp (Alt-R)" ,#'reset)
                    ("Clear window (Alt-Z)" ,#'clear))
                  pos))
          (layout (V border: 8 (dom ilisp.iframe))))
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
    (set-handler container oncontextmenu
      (event.preventDefault)
      (event.stopPropagation)
      (contextmenu (event-pos event)))
    container))

(defvar *user*)
(defvar *secret*)
(defvar *session-id*)

(rpc:defun login (user-name))
(rpc:defun rload-file (user-name session-id filename authcode))
(rpc:defun rsave-file (user-name session-id filename content authcode))
(rpc:defun rlist-files (user-name session-id path authcode))
(rpc:defun rping (user-name session-id authcode))
(rpc:defun rbuild (user-name session-id source mode authcode))
(rpc:defun rbuild-check (user-name session-id id authcode))
(rpc:defun rterminal (user-name session-id authcode))
(rpc:defun rterminal-send (user-name session-id id x authcode))
(rpc:defun rterminal-receive (user-name session-id id authcode))
(rpc:defun rterminal-detach (user-name session-id id authcode))
(rpc:defun rset-user-secret (user-name session-id newsecret authcode))
(rpc:defun rupdate-user (user-name session-id user secret permissions authcode))
(rpc:defun rremove-user (user-name session-id user authcode))
(rpc:defun rlist-users (user-name session-id authcode))

(defun load-file (name)
  (rload-file *user* *session-id* name
              (hash (+ *session-id* *secret* (json* name)))))

(defun save-file (name content)
  (when (rsave-file *user* *session-id* name content
                    (hash (+ *session-id* *secret* (json* (list name content)))))
    (baloon ~"{(htm name)} saved")))

(defun list-files (path)
  (rlist-files *user* *session-id* path
               (hash (+ *session-id* *secret* (json* path)))))

(defun ping ()
  (rping *user* *session-id* (hash (+ *session-id* *secret* "null"))))

(defun build (source mode)
  (rbuild *user* *session-id* source mode
          (hash (+ *session-id* *secret*
                   (json* (list source mode))))))

(defun build-check (id)
  (rbuild-check *user* *session-id* id
                (hash (+ *session-id* *secret*
                         (json* id)))))

(defun new-terminal ()
  (rterminal *user* *session-id* (hash (+ *session-id* *secret* "null"))))

(defun terminal-send (id x)
  (rterminal-send *user* *session-id* id x (hash (+ *session-id* *secret* (json* (list id x))))))

(defun terminal-receive (id)
  (rterminal-receive *user* *session-id* id (hash (+ *session-id* *secret* (json* id)))))

(defun terminal-detach (id)
  (rterminal-detach *user* *session-id* id (hash (+ *session-id* *secret* (json* id)))))

(defun set-user-secret (newsecret)
  (rset-user-secret *user* *session-id* newsecret (hash (+ *session-id* *secret* (json* newsecret)))))

(defun update-user (user secret permissions)
  (rupdate-user *user* *session-id* user secret permissions
                (hash (+ *session-id* *secret*
                         (json* (list user secret permissions))))))

(defun remove-user (user)
  (rremove-user *user* *session-id* user (hash (+ *session-id* *secret* (json* user)))))

(defun list-users ()
  (rlist-users *user* *session-id* (hash (+ *session-id* *secret* "null"))))

(defun set-new-password ()
  (let** ((w (window 0 0 280 190 title: "Set new password"))
          (p1 (add-widget w (input "new password")))
          (p2 (add-widget w (input "repeat new password")))
          (ok (add-widget w (button "OK" #'ok)))
          (cancel (add-widget w (button "Cancel" #'cancel)))
          (#'cancel ()
            (hide-window w))
          (#'ok ()
            (if (/= (text p1)
                    (text p2))
                (baloon "The two password are not identical")
                (progn
                  (set-user-secret (hash (text p1)))
                  (baloon "Password has been updated")
                  (hide-window w)))))
    (set-layout w (V border: 8 spacing: 8
                     size: 40
                     (dom p1)
                     (dom p2)
                     :filler:
                     size: 30
                     (H :filler:
                        size: 80
                        (dom ok)
                        (dom cancel)
                        :filler:)))
    (setf (node p1).type "password")
    (setf (node p2).type "password")
    (focus p1)
    (show-window w center: true modal: true)))

(defun edit-user (user-name user-permissions)
  (let** ((new-user (= user-name "<new>"))
          (w (window 0 0 400 312 title: (if new-user "New user" "Edit user")))
          (name (add-widget w (input "name")))
          (p1 (add-widget w (input "set new password")))
          (p2 (add-widget w (input "repeat new password")))
          (permissions (add-widget w (group "permissions")))
          (admin-priv (add-widget w (checkbox "Administrator")))
          (read-priv (add-widget w (checkbox "Read")))
          (write-priv (add-widget w (checkbox "Write")))
          (list-priv (add-widget w (checkbox "List")))
          (terminal-priv (add-widget w (checkbox "Terminal")))
          (save (add-widget w (button "Save" #'save)))
          (delete (add-widget w (button "Delete" #'delete)))
          (#'save ()
            (cond
              ((and (or (text p1) (text p2))
                    (/= (text p1) (text p2)))
               (baloon "The two passwords are different"))
              ((and new-user (not (text p1)))
               (baloon "The password is mandatory"))
              (true
                (update-user (text name)
                             (if (text p1) (hash (text p1)) null)
                             (append (if (checked admin-priv) '("admin") '())
                                     (if (checked read-priv) '("read") '())
                                     (if (checked write-priv) '("write") '())
                                     (if (checked list-priv) '("list") '())
                                     (if (checked terminal-priv) '("terminal") '())))
                (baloon "User data updated")
                (hide-window w))))
          (#'delete ()
            (unless new-user
              (remove-user (text name))
              (baloon "User removed")
              (hide-window w))))
    (unless new-user
      (setf (text name) user-name)
      (setf (node name).disabled "disabled"))
    (setf (node p1).type "password")
    (setf (node p2).type "password")
    (setf (checked admin-priv) (find "admin" user-permissions))
    (setf (checked read-priv) (find "read" user-permissions))
    (setf (checked write-priv) (find "write" user-permissions))
    (setf (checked list-priv) (find "list" user-permissions))
    (setf (checked terminal-priv) (find "terminal" user-permissions))

    (set-layout w (V border: 8 spacing: 8
                     size: 42
                     (dom name)
                     (H (dom p1) (dom p2))
                     size: undefined
                     (dom permissions
                          (V border: 12
                             (dom admin-priv)
                             (dom read-priv)
                             (dom write-priv)
                             (dom list-priv)
                             (dom terminal-priv)))
                     size: 30
                     (H :filler:
                        size: 80
                        (dom save)
                        (dom delete)
                        :filler:)))
    (if new-user
        (focus p1)
        (focus name))
    (show-window w center: true)))

(defun user-list ()
  (let** ((w (window 0 0 548 312 title: "User management"))
          (users (add-widget w (set-style (create-element "div")
                                          overflow "auto")))
          (#'user-item (name permissions)
            (let** ((row (append-child users
                                       (set-style (create-element "div")
                                                  fontFamily "monospace"
                                                  backgroundColor "#EEE"
                                                  px/padding 4
                                                  px/margin 4
                                                  px/fontSize 16
                                                  cursor "pointer"
                                                  fontWeight "bold")))
                    (fname (append-child row (set-style (create-element "div")
                                                        display "inline-block"
                                                        %/width 30)))
                    (fpermissions (append-child row (set-style (create-element "div")
                                                               display "inline-block"
                                                               %/width 70))))
              (setf fname.textContent name)
              (setf fpermissions.textContent permissions)
              (set-handler row onmousedown
                (event.preventDefault)
                (event.stopPropagation)
                (edit-user name permissions)
                (hide-window w)))))
    (set-layout w (V border: 8 spacing: 8
                     (dom users)))
    (let ((ud (list-users)))
      (dolist (name (keys ud))
        (user-item name (aref ud name).permissions)))
    (user-item "<new>" "<new>")
    (show-window w center: true)))

(defun file-browser (cback)
  (let** ((w (set-style (create-element "div")
                        position "absolute"))
          (user (append-child w (input "username")))
          (password (append-child w (input "password")))
          (current-path (append-child w (input "current path")))
          (edit-users (append-child w (button "edit users" #'edit-users)))
          (files (append-child w (set-style (create-element "div")
                                            position "absolute"
                                            border  "solid 1px #000000"
                                            overflow "auto"
                                            px/margin -2
                                            px/padding 2)))
          (last-search null)
          (filelist (list))
          (#'edit-users ()
            (if (= *user* "admin")
                (user-list)
                (set-new-password)))
          (#'pathexp (s)
            (let* ((last-sep (last-index "/" s))
                   (base (if (= last-sep -1) "./" (slice s 0 (1+ last-sep))))
                   (name (slice s (1+ last-sep))))
              (list base name)))
          (#'filter ()
            (let ((search (slice (text current-path)
                                 0 (node current-path).selectionStart)))
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
                                                              cursor "pointer"
                                                              px/padding 2))))
                        (set-handler d onclick
                          (funcall cback (+ base f)))
                        (setf d.textContent f))))
                  (when (and (> (length filelist) 0)
                             (= (length (text current-path))
                                (node current-path).selectionStart
                                (node current-path).selectionEnd))
                    (let ((i (length (first filelist))))
                      (dolist (x filelist)
                        (do () ((= (slice (first filelist) 0 i)
                                   (slice x 0 i)))
                          (decf i)))
                      (when (> i (length name))
                        (when (= base "./")
                          (setf base ""))
                        (setf (text current-path) (+ base (slice (first filelist) 0 i)))
                        ((node current-path).setSelectionRange
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
                        (dom current-path)
                        size: 80
                        (V :filler: size: 24 (dom edit-users)))
                     size: undefined
                     (dom files))))
    (set-handler current-path onkeydown
      (when (= event.which 13)
        (event.stopPropagation)
        (event.preventDefault)
        (if (/= (node current-path).selectionStart
                (node current-path).selectionEnd)
            ((node current-path).setSelectionRange
             (length (text current-path))
             (length (text current-path)))
            (enter))))
    (setf (node password).type "password")
    (set-handler (node password) onblur
      (setf *user* (text user))
      (setf *secret* (hash (text password)))
      (setf *session-id* (login *user*)))
    (set-interval #'filter 250)
    (setf w."data-resize" (lambda (x0 y0 x1 y1)
                            (set-coords layout 0 0 (- x1 x0) (- y1 y0))))
    (setf w.focus (lambda ()
                    ((node (cond
                             ((= (text user) "") user)
                             ((= (text password) "") password)
                             (true current-path))).focus)))
    w))

(defvar *deploy-parms* #())

(defun deploy (fname)
  (let** ((w (window 0 0 372 193 title: "deploy"))
          (main-source (add-widget w (input "main source" autofocus: true)))
          (target (add-widget w (select "target" '("html" "node-js"))))
          (output-file (add-widget w (input "output file")))
          (#'start-build ()
            (let** ((id (build (text main-source)
                               (text target)))
                    (#'check ()
                      (let ((ans (build-check id)))
                        (if ans
                            (if (= (first ans) 0)
                                (progn
                                  (baloon "build ok")
                                  (save-file (text output-file)
                                             (join (second ans) ""))
                                  (when (length (third ans))
                                    (message-box (join (map #'htm (third ans)) "<br/>")
                                                 title: "build warnings")))
                                (progn
                                  (baloon "build error")
                                  (message-box (join (map #'htm (third ans)) "<br/>")
                                               title: "build error")))
                            (set-timeout #'check 100)))))
              (set-timeout #'check 100)))
          (ok (add-widget w (button "OK" (lambda ()
                                           (unless (aref *deploy-parms* fname)
                                             (setf (aref *deploy-parms* fname)
                                                   #((target "html")
                                                     (output-file ""))))
                                           (setf (aref *deploy-parms* fname).target
                                                 (text target))
                                           (setf (aref *deploy-parms* fname).output-file
                                                 (text output-file))
                                           (start-build)
                                           (hide-window w)))))
          (cancel (add-widget w (button "Cancel" (lambda ()
                                                   (hide-window w))))))
    (unless (aref *deploy-parms* fname)
      (setf (aref *deploy-parms* fname)
            #((target "html")
              (output-file ""))))
    (setf (text main-source) fname)
    (setf (text target) (aref *deploy-parms* fname).target)
    (setf (text output-file) (aref *deploy-parms* fname).output-file)
    (set-layout w (V border: 8 spacing: 8
                     size: 40
                     (H (dom main-source) size: 120 (dom target))
                     (dom output-file)
                     :filler:
                     size: 30
                     (H :filler: size: 80 (dom ok) (dom cancel) :filler:)))
    (show-window w center: true modal: true)))

(defun terminal ()
  (let** ((w (text-area "terminal"))
          (t (new-terminal))
          (i null)
          (exit-code null)
          (#'update ()
            (let (((ec output) (terminal-receive t)))
              (when output.length
                (setf (text w) (+ (text w) (join output "")))
                (setf (node w).scrollTop (node w).scrollHeight))
              (when (and (not (null? ec))
                         (null? exit-code))
                (setf exit-code ec)
                (set-style (node w) color "#CCCCCC")
                (clear-interval i)))))
    (setf w.focus (lambda () (focus w)))
    (set-style (node w)
               backgroundColor "#444444"
               color "#88FF88"
               whiteSpace "pre"
               px/fontSize 16)
    (setf (node w).wrap "off")
    (setf (node w).spellcheck false)
    (set-handler (node w) onkeydown
      (cond
        ((= event.which 8)
         (event.preventDefault)
         (event.stopPropagation)
         (when (null? exit-code) (terminal-send t (char 8)))
         (setf (text w) (slice (text w) 0 -1))
         (setf (node w).scrollTop (node w).scrollHeight)
         false)
        ((and event.ctrlKey (= event.which #.(char-code "V")))
         (let ((current (text w)))
           (set-timeout
             (lambda ()
               (let ((new (text w)))
                 (when (and (> (length new) (length current))
                            (= (slice new 0 (length current)) current)
                            (null? exit-code))
                   (terminal-send t (slice new (length current))))))
             100))
         true)
        (true true)))
    (set-handler (node w) onkeypress
      (when (and event.charCode (null? exit-code))
        (terminal-send t (char event.charCode))
        (setf (text w) (+ (text w) (char event.charCode)))
        (setf (node w).scrollTop (node w).scrollHeight)
        (event.preventDefault)
        (event.stopPropagation)
        (when (or (= event.charCode 13) ;; CR
                  (= event.charCode 4)) ;; Ctrl-D
          (set-timeout #'update 100))))
    (setf i (set-interval #'update 1000))
    (setf w.remove (lambda ()
                     (if (null? exit-code)
                         (progn
                           (message-box
                             "<h2>Process is active. Close anyway?</h2>
                              The process attached to this terminal is still
                              running. Are you sure you want to detach it?<br/>"
                             title: "Closing terminal warning"
                             buttons: (list "Yes" "No")
                             default: "No"
                             modal: true
                             cback: (lambda (reply)
                                      (when (= reply "Yes")
                                        (setf exit-code 0)
                                        (set-style (node w) color "#CCCCCC")
                                        (clear-interval i))))
                           false)
                         (progn
                           (terminal-detach t)
                           true))))
    (set-timeout #'update 100)
    w))

(defun main ()
  ;; Block ctrl-R at toplevel
  (set-handler document.body onkeydown
    (if (and event.ctrlKey
             (= event.which #.(char-code "R")))
        (progn
          (event.stopPropagation)
          (event.preventDefault)
          false)
        true))

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
          (splitv 100)
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
                    (set-handler btn onclick
                      (view-source location))
                    (append-child doc btn))))))
          (#'view-source ((file c0 c1))
            (declare (ignorable c1))
            (let ((content (load-file file)))
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
            (*ilisp*.send "quiet-lisp"
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
          (let ((editor (src-tab f (or (load-file f) ""))))
            (sources.add f editor true)
            (setf editor.remove?
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
            ((and (or event.altKey event.metaKey) (= event.which 13))
             (zoom)
             ((sources.current).ilisp-exec ((sources.current).buffer)))
            ((and (or event.altKey event.metaKey) (= event.which #.(char-code "I")))
             (let ((expr (prompt "Expression")))
               (when (strip expr)
                 ((sources.current).ilisp-exec expr))))
            ((and event.ctrlKey (= event.which #.(char-code "T")))
             (sources.add "*terminal*" (terminal) true)
             (sources.select 0)
             (sources.prev))
            ((and event.ctrlKey (= event.which #.(char-code "D")))
             (when (> (sources.current-index) 0)
               (if ((sources.current).modified)
                   (message-box "<h1>Current file not saved.</h1>
                                 The deployed program will be based on last saved version."
                                title: "Unsaved changes"
                                buttons: '("OK" "Cancel")
                                cback: (lambda (reply)
                                         (when (= reply "OK")
                                           (deploy ((sources.current).name)))))
                   (deploy ((sources.current).name)))))
            ((and event.ctrlKey (= event.which #.(char-code "W")))
             (when (and (> (sources.current-index) 0)
                        (save-file ((sources.current).name)
                                   ((sources.current).buffer)))
               ((sources.current).clear-modified)))
            ((and event.ctrlKey (= event.which #.(char-code "Q")))
             (when (> (sources.current-index) 0)
               (sources.remove (sources.current-index))))
            ((and event.ctrlKey (= event.which #.(char-code "K")))
             (when ((sources.current).selection)
               (setf zrun ((sources.current).selection)))
             (zoom)
             (when (and zoom zrun)
               ((sources.current).ilisp-exec zrun)))
            ((and event.ctrlKey (= event.which #.(char-code "G")))
             (when (> (sources.current-index) 0)
               (gui-editor (lambda (code)
                             (let (((r0 r1) ((sources.current).insert-text code)))
                               ((sources.current).set-pos r0 0 r1 0)
                               ((sources.current).indent-selection))))))
            ((and event.ctrlKey (= event.which #.(char-code "O"))
                  mode.styles)
             (customize-styles mode.styles
                               (lambda (res)
                                 (when (and res
                                            (sources.current).refresh)
                                   ((sources.current).refresh)))))
            ((= event.which 27)
             (vs.partition splitv)
             (setf splitv (- (+ 80 100) splitv))
             (setf stop false))
            ((and event.ctrlKey (= event.which 39))
             (sources.next))
            ((and event.ctrlKey (= event.which 37))
             (sources.prev))
            ((and event.ctrlKey (= event.which 13))
             (when event.shiftKey
               (zoom))
             ((sources.current).ilisp-exec)
             (mode.inspect-ilisp *ilisp*
                                 (lambda ()
                                   (when (sources.current).refresh
                                     ((sources.current).refresh)))))
            ((= event.which 112)
             (let** ((w (window 0 0 596 740 title: "Help"))
                     (help (add-widget w (set-style (create-element "div")
                                                    position "absolute"))))
               (setf help.innerHTML '#.(get-file "idehelp.html"))
               (set-layout w (dom help))
               (show-window w center: true)))
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
                   (text (aref lines row).text)
                   (c1 col))
              (do () ((or (= c1 0) (/= (aref text c1) " ")))
                (decf c1))
              (do () ((or (= c1 (length text)) (find (aref text c1) " ()[]{}")))
                (incf c1))
              (do ((c c1 (1- c)))
                ((or (= c 0)
                     (= (aref text (1- c)) "("))
                 (let ((name (slice text c c1)))
                   (when (/= name last-lookup)
                     (setf last-lookup name)
                     (doc-lookup name)))))))))
      100)))

(defvar *deploy-prefix* "")
(setf *deploy-prefix* (replace *deploy-prefix* "</head>" "<title>JsLisp IDE</title></head>"))

(main)
