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
  (error ~"Unknown request {req.%class}"))

(defun process (url parms data response)
  (display ~"Processing url={url}, parms={parms}, data={data}")
  (when (and (= parms "") (not (null? data)))
    (setf parms data))
  (let ((content (try (cond
                        ((= url "/process")
                         (uri-encode
                          (json*
                           (process-request (json-parse* (uri-decode parms))))))
                        (((regexp "^/process-as\\.[a-z0-9]+$").exec url)
                         (process-request (json-parse* (uri-decode parms))))
                        (true (try (get-file (+ "." url) null)
                                   (progn
                                     (response.writeHead 404)
                                     (response.end "File not found")
                                     (return-from process)))))
                      (progn
                        (display ~"ERROR: {*exception*}")
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

(setf (symbol-macro 'rpc:defun)
      (lambda (name args &rest body)
        (setf name (module-symbol name))
        (let ((fields (filter (lambda (x) (/= x '&optional))
                              (map (lambda (f)
                                     (if (list? f) (first f) f))
                                   args))))
          `(progn
             (defun ,name ,args ,@body)
             (defobject ,#"{name}-req" ,fields)
             (defmethod process-request (req) (,#"{name}-req?" req)
                        (,name ,@(map (lambda (f)
                                        `(. req ,f))
                                      fields)))))))

(export start-server)
