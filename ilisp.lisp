(import * from gui)

(defobject ilisp (id iframe reqs default-cback))

(defvar *id* 0)
(defvar *req* 0)

(defvar *instances* #())

(defvar *cback-installed* false)

(defun new (default-cback)
  (unless *cback-installed*
    (setf *cback-installed* true)
    (setf (js-code "window").onmessage
          (lambda (event)
            (let ((x (aref *instances* event.data.id)))
              (when x
                (let ((cback (or (aref x.reqs event.data.req) x.default-cback)))
                  (remove-key x.reqs event.data.req)
                  (funcall cback event.data.reply)))))))
  (let ((ilisp (make-ilisp id: (incf *id*)
                           iframe: (create-element "iframe")
                           reqs: #()
                           default-cback: default-cback)))
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

    (setf ilisp.send (lambda (type message &optional cback)
                       (let ((req (incf *req*)))
                         (setf (aref ilisp.reqs req) cback)
                         (ilisp.iframe.contentWindow.postMessage
                          #((type type)
                            (text message)
                            (req req))
                          "*"))))

    (setf ilisp.reset (lambda ()
                        (setf ilisp.iframe.src ~"ilisp.html?{ilisp.id}")))

    (setf ilisp.focus (lambda ()
                        (ilisp.iframe.focus)))

    ilisp))
