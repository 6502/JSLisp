(import * from serialize)

(if node-js
    (progn
      ;;
      ;; Server side, an http-server able to serve static files
      ;; and to handle `/process` http requests.
      ;; Serving static files is useful because of Same Origin Policy
      ;;
      (defvar *typemap* #((".html" "text/html")
                          (".css"  "text/css")
                          (".js"   "text/javascript")
                          (".jpg"  "image/jpeg")
                          (".png"  "image/png")
                          (".pdf"  "application/pdf")
                          (".txt"  "text/plain")
                          (".gz"   "application/x-gzip")
                          (".zip"  "application/zip")))

      (defun process-request (req)
        (error "Unknown request {req.%class}"))

      (defun process (url parms data response)
        (display ~"Processing url={url}, parms={parms}, data={data}")
        (when (and (= parms "") (not (null? data)))
          (setf parms data))
        (let ((content (try (cond
                              ((= url "/process")
                               (to-buffer (process-request (from-buffer (uri-decode parms)))))
                              (((regexp "^/process-as\\.[a-z0-9]+$").exec url)
                               (process-request (from-buffer (uri-decode parms))))
                              (true (try (get-file (+ "." url) null)
                                         (progn
                                           (response.writeHead 404)
                                           (response.end "File not found")
                                           (return-from process)))))
                            (progn
                              (response.writeHead 500)
                              (response.end "Internal error")
                              (return-from process))))
              (ctype (or (aref *typemap* ((regexp "\\.[a-z0-9]*$").exec url))
                         "application/octect-stream")))
          (response.writeHead 200 #((Content-Type ctype)))
          (response.end content)))

      (defun rpc-handler (request response)
        (let ((url (. request url))
              (parms null))
          (when (find "?" url)
            (let ((i (index "?" url)))
              (setf parms (slice url (1+ i)))
              (setf url (slice url 0 i))))
          (if (= request.method "POST")
              (let ((data ""))
                (request.on "data"
                            (lambda (chunk)
                              (incf data chunk)))
                (request.on "end"
                            (lambda ()
                              (process url parms data response))))
              (process url parms null response))))

      (defun start-server (address port)
        (let* ((http (js-code "require('http')"))
               (server (http.createServer #'rpc-handler)))
          (server.listen port address)))
      (export start-server))

    (progn
      ;;
      ;; Client-side, routing remote function calls to blocking
      ;; http requests. Parameters and return values must be serializable.
      ;;
      (defun remote (x)
        (let ((request (uri-encode (serialize:to-buffer x))))
          (let ((reply (http "POST" "process?" request)))
            (let ((result (serialize:from-buffer (uri-decode reply))))
              result))))))

(defmacro defun-remote (name args &rest body)
  (let ((fields (map (lambda (f)
                       (if (list? f) (first f) f))
                     args)))
    (if node-js
        `(progn
           ;;
           ;; Server side; create and register specific handler
           ;;
           (defun ,name ,args ,@body)
           (defobject* ,#"{name}-req" ,fields)
           (defmethod rpc:process-request (req) (,#"{name}-req?" req)
                      (,name ,@(map (lambda (f)
                                      `(. req ,f))
                                    fields))))
        `(progn
           ;;
           ;; Client side; create the tunneling stub only
           ;;
           (defobject* ,#"{name}-req" ,fields)
           (defun ,name ,args
             (remote (,#"new-{name}-req" ,@fields)))))))

(export defun-remote)
