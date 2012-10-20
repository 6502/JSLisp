(import * from gui)
(import * from layout)
(import * from editor)
(import * from rpc-client)
(import (hash) from crypto)
(import editor-cmode)
(import editor-lispmode)

(defun test-editor (name content)
  (let** ((w (window 0 0 640 480 title: name))
          (editor (add-widget w (editor name content
                                        (case (slice name (1+ (last-index "." name)))
                                          ("c" editor-cmode:mode)
                                          ("lisp" editor-lispmode:mode)
                                          (otherwise nullmode))))))
    (set-layout w (V border: 8 spacing: 8
                     (dom editor)))
    (show-window w center: true)))

(rpc:defun remote (user-name session-id x authcode))
(rpc:defun login (user-name))

(defvar *user*)
(defvar *secret*)
(defvar *session-id*)

(defun call-remote (x)
  (remote *user* *session-id* x
          (hash (+ *session-id* *secret* (json* x)))))

(defun get-file (name)
  (call-remote `(get-file ,name)))

(defun files (path)
  (call-remote `((node:require "fs").readdirSync ,path)))

(defun file-selector ()
  (let** ((w (window 0 0 640 400 title: "File selector"))
          (files (map (lambda (f)
                        (list f))
                      (sort
                       (filter (lambda (x)
                                 ((regexp "\\.(lisp|c)$").exec x))
                               (files ".")))))
          (filelist (add-widget w (table files
                                         rows: 25
                                         row-click: (lambda (row row-cells)
                                                      (declare (ignorable row-cells))
                                                      (test-editor
                                                       (first row)
                                                       (get-file (first row))))))))
    (set-layout w (V (dom filelist)))
    (show-window w center: true)))

(defun main ()
  (gui:login (lambda (user pass)
               (when user
                 (setf *user* user)
                 (setf *secret* (hash pass))
                 (setf *session-id* (login *user*))
                 (file-selector)))))

(main)