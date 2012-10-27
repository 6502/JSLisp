(import * from gui)

(defobject ilisp (id iframe cback))

(defvar *id* 0)

(defvar *instances* #())

(defun new (cback)
  (let ((ilisp (make-ilisp id: (incf *id*)
                           iframe: (create-element "iframe")
                           cback: cback)))
    (setf (aref *instances* ilisp.id) ilisp)
    (setf ilisp.iframe.src ~"ilisp.html?{ilisp.id}")
    (set-style ilisp.iframe
               position "absolute"
               px/right 0
               px/top 0
               px/width 1
               px/height 1
               opacity 0.1)
    (append-child document.body ilisp.iframe)

    (setf ilisp.close (lambda ()
                        (remove-key *instances* ilisp.id)
                        (remove-child document.body ilisp.iframe)))

    (setf ilisp.send (lambda (message)
                       (ilisp.iframe.contentWindow.postMessage message "*")))

    ilisp))

(setf (js-code "window").onmessage
      (lambda (event)
        (let ((x (aref *instances* event.data.id)))
          (when x
            (funcall x.cback (json-parse* event.data.reply))))))
