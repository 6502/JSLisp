(defun start-server (address port handler)
  (let* ((http (js-code "require('http')"))
         (server (funcall (. http createServer) handler)))
    (funcall (. server listen) port address)))

(defun reply (response content-type msg)
  (funcall (. response writeHead) 200 (js-object "Content-type" content-type))
  (funcall (. response end) msg))

(defvar *current-location* null)
(defvar *pending-response* null)
(defvar *pending-reply* null)

(defun idler ()
  (when *pending-response*
    (reply *pending-response* "text/plain" "0")
    (setf *pending-response* null)))

(set-interval #'idler 10000)

(defun send (msg)
  (if *pending-response*
      (progn
        (reply *pending-response* "text/plain" msg)
        (setf *pending-response* null))
      (setf *pending-reply* msg)))

(defun handler (request response)
  (let ((url (. request url))
        (parms ""))
    (when (find "?" url)
      (let ((i (index "?" url)))
        (setf parms (slice url (1+ i)))
        (setf url (slice url 0 i))))
    (let ((f (symbol-function (intern (slice url 1)))))
      (if f
          (apply f (append (list response)
                           (map #'uri-decode (split parms "&"))))
          (let ((content (try (get-file (+ "." url) null)
                              (str-value *exception*)))
                (ctype (cond
                         ((find ".html" url)
                          "text/html")
                         ((find ".css" url)
                          "text/css")
                         ((find ".js" url)
                          "text/javascript")
                         ((find ".jpg" url)
                          "image/jpeg")
                         ((find ".png" url)
                          "image/png")
                         (true "text/plain"))))
            (reply response ctype content))))))

(defun cmd (response location)
  (setf *current-location* location)
  (setf *pending-response* response)
  (when *pending-reply*
    (send *pending-reply*)
    (setf *pending-reply* null)))

(defvar *data-wait* null)

(defun data (response msg)
  (when *data-wait*
    (reply *data-wait* "text/plain" msg)
    (setf *data-wait* null))
  (reply response "text/plain" "ok"))

(defun info (response)
  (reply response "text/plain" (+ "" *current-location*)))

(defun getdata (response expr)
  (send ~"f$$http_get(\"http://127.0.0.1:1337/data?\"+encodeURIComponent(safe(ee,\"{expr}\")));")
  (setf *data-wait* response))

(defun step (response)
  (send "cont")
  (setf *current-location* null)
  (reply response "text/plain" "ok"))

(start-server "127.0.0.1" 1337
              #'handler)
