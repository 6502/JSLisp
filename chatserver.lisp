(defun start-server (address port handler)
  (let* ((http (js-code "require('http')"))
         (server (funcall (. http createServer) handler)))
    (funcall (. server listen) port address)))

(defun reply (response content-type msg)
  (funcall (. response writeHead) 200 #((Content-type content-type)))
  (funcall (. response end) msg))

(defobject channel (name
                    (n0 0)
                    (max 100)
                    (messages (list))
                    (listeners (list))))

(defun add-message (channel message)
  (display ~"{channel}: + {(str-value message)}")
  (push message channel.messages)
  (let ((xs (- (length channel.messages) channel.max)))
    (when (> xs 0)
      (incf channel.n0 xs)
      (setf channel.messages (slice channel.messages xs)))))

(defvar *channels* #())

(defun channel (x)
  "Returns a channel given its name [x] or creates a new channel"
  (or (aref *channels* x)
      (setf (aref *channels* x) (new-channel x))))

(defun send (response channel-name message)
  "Send a [message] to the specified [channel]"
  (let ((channel (channel channel-name)))
    (add-message channel message)
    (let ((id (+ channel.n0 (length channel.messages) -1)))
      (reply response "text/plain" ~"OK:{id}")
      (dolist (x channel.listeners)
        (reply x "text/plain" ~"{id}:{message}"))
      (setf channel.listeners (list)))))

(defun receive (response channel-name after)
  "Receive all messages from [channel] after the specified number"
  (let* ((channel (channel channel-name))
         (messages channel.messages)
         (skip (max 0 (- (parse-value after) channel.n0))))
    (if (< skip (length messages))
        (let ((reply (list)))
          (do ((id (+ channel.n0 skip) (1+ id))
               (i skip (1+ i)))
              ((>= i (length messages))
                 (reply response "text/plain" (join reply "\n")))
            (push ~"{id}:{(aref messages i)}" reply)))
        (push response channel.listeners))))

(defun handler (request response)
  (let ((url (. request url))
        (parms "")
        (data ""))
    (when (find "?" url)
      (let ((i (index "?" url)))
        (setf parms (slice url (1+ i)))
        (setf url (slice url 0 i))))
    (let ((f (symbol-function (intern (slice url 1)))))
      (if f
          (if (= request.method "POST")
              (progn
                ;; A "POST" request requires data collection
                (request.on "data" (lambda (chunk)
                                     (incf data chunk)))
                (request.on "end" (lambda ()
                                    ;; Pass received data as last parameter
                                    (apply f (append (list response)
                                                     (map #'uri-decode (split parms "&"))
                                                     data)))))
              ;; "GET" request, just call the function
              (apply f (append (list response)
                               (map #'uri-decode (split parms "&")))))
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

(start-server "127.0.0.1" 1337
              #'handler)
