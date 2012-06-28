
(import * from simpledb)

(defvar fs (js-code "require('fs')"))

(let ((*no-transactions* true))
  (load (get-file "webdb.log")))

(display "Data loaded")

(setf *logwrite*
      (lambda (x)
        (fs.appendFile "webdb.log" (+ x "\n")
                       (lambda (err)
                         (when err
                           (display ~"ERROR: {err}"))))))

(defun start-server (address port handler)
  (let* ((http (js-code "require('http')"))
         (server (http.createServer handler)))
    (server.listen port address)))

(defun parse-float (x)
  (js-code "parseFloat(d$$x)"))

(defun web-eval (x)
  (toplevel-eval (parse-value x)))

(defun my-handler (request response)
  (let ((url (. request url))
        (parms null))
    (when (find "?" url)
      (let ((i (index "?" url)))
        (setf parms (slice url (1+ i)))
        (setf url (slice url 0 i))))
    (let ((content (try (get-file (+ "." url) null)
                        (try (str-value (apply (symbol-function #"web-{(slice url 1)}")
                                               (map #'uri-decode (split parms "&"))))
                             (str-value *exception*))))
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
      (funcall (. response writeHead)
               200 (js-object ("Content-Type" ctype)))
      (funcall (. response end) content))))

(start-server "127.0.0.1" 1337
              #'my-handler)
