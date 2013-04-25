;;
;; Server side, an http-server able to serve static files
;; and to handle `/process` http requests.
;; Serving static files is useful because of Same Origin Policy
;;

;; User: needed for operations requiring authorization and for session management
(defobject user
    (name               ;; Username
     secret             ;; Hashed secret
     permissions))      ;; List of permission tokens owned: a published function
                        ;; may enumerate a list of required tokens needed to allow
                        ;; the execution.

(defobject session
    (user               ;; A user object (not just the name).
     last-activity))    ;; (clock) at last successfull operation (used for timeouts)

;; Currently active sessions. Managed automatically
(defvar *open-sessions* #())

;; Map from user name to user object. This map must be filled and maintained by
;; the server application.
(defvar *users* #())

(import (hash) from crypto)

(defun session-cleanup ()
  "Closes existing sessions after 5 minutes of inactivity"
  (let ((limit (- (clock) (* 5 60 1000))))
    (dolist (session-id (keys *open-sessions*))
      (let ((session (aref *open-sessions* session-id)))
        (when (< session.last-activity limit)
          (remove-key *open-sessions* session-id))))))

(defun open-session (user-name)
  "Creates a new session for the specified [user-name]"
  (let ((user (aref *users* user-name))
        (session-id (join (repeat-collect 20
                                 (aref "abcdefghijklmnopqrstuvwxyz\
                                        ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                                        0123456789"
                                       (random-int (+ 26 26 10))))
                          "")))
    (when user
      (setf (aref *open-sessions* session-id) (new-session user (clock))))
    session-id))

(defun check-authorization (user-name session-id authcode request)
  "Returns the list of authorization tokens of the user \
   or null if the authorization failed."
  (let ((session (aref *open-sessions* session-id))
        (user (aref *users* user-name)))
    (if (and session user (= session.user user)
             (= authcode (hash (+ session-id user.secret request))))
        (progn
          (setf session.last-activity (clock))
          user.permissions)
        null)))

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

(defun process (url parsed-req response)
  (let ((content (try (cond
                        ((= url "/process")
                         (json* (process-request parsed-req)))
                        (((regexp "^/process-as\\.[a-z0-9]+$").exec url)
                         (process-request parsed-req))
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

(defobject uploaded-file (tmp-name uploaded-name))

(defun multipart-form-data (buf boundary)
  (let* ((bd (map #'char-code (+ "\r\n--" boundary)))
         (bd0 (map #'char-code (+ "--" boundary "\r\n")))
         (tlen (length buf))
         (cp0 null)
         (cp 0)
         (result #()))
    (labels ((match (b)
               (dotimes (i (length b))
                 (unless (= (aref buf (+ cp i))
                            (aref b i))
                   (return-from match false)))
               true))
      (when (match bd0)
        (incf cp (length bd0))
        (setf cp0 cp))
      (do ()
          ((> cp (- tlen (length bd))))
        (if (and (match bd)
                 (or (and (= (aref buf (+ cp (length bd))) 13)
                          (= (aref buf (+ cp (length bd) 1)) 10))
                     (and (= (aref buf (+ cp (length bd))) 45)
                          (= (aref buf (+ cp (length bd) 1)) 45))))
            (progn
              (unless (null? cp0)
                (let ((filename null)
                      (name null))
                  (do ()
                      ((or (= cp0 cp)
                           (and (= (aref buf cp0) 13)
                                (= (aref buf (1+ cp0)) 10)))
                         (incf cp0 2))
                    (do ((hs cp0))
                        ((or (= cp0 cp)
                             (= (aref buf cp0) 13))
                           (let* ((hline (+ "" (slice buf hs cp0)))
                                  (nm ((regexp " name=\"([^\"]+)\"").exec hline))
                                  (fm ((regexp " filename=\"([^\"]+)\"").exec hline)))
                             (when nm (setf name (second nm)))
                             (when fm (setf filename (second fm))))
                           (incf cp0 2))
                      (incf cp0)))
                  (if filename
                      (let ((dstfile (+ ((node:require "os").tmpDir)
                                        "/"
                                        (join (map (lambda (x)
                                                     (declare (ignorable x))
                                                     (char (+ 65 (random-int 26))))
                                                   (range 8))
                                              "")
                                        ".upload.tmp")))
                        ((node:require "fs").writeFileSync dstfile (slice buf cp0 cp))
                        (setf (aref result name) (new-uploaded-file dstfile filename)))
                      (setf (aref result name) (+ "" (slice buf cp0 cp))))))
              (if (= (aref buf (+ cp (length bd))) 45)
                  (setf cp tlen)
                  (incf cp (+ (length bd) 2)))
              (setf cp0 cp))
            (incf cp))))
    result))

(defobject http-request (parms))

(defun rpc-handler (request response)
  (let ((url (. request url))
        (parsed-req null))
    (when (find "?" url)
      (let ((i (index "?" url)))
        (setf parsed-req (new-http-request (uri-decode (slice url (1+ i)))))
        (setf url (slice url 0 i))))
    (if (= request.method "POST")
        (let ((data (list)))
          (unless parsed-req
            (setf parsed-req (new-http-request)))
          (request.on "data"
                      (lambda (chunk)
                        (push chunk data)))
          (request.on "end"
                      (lambda ()
                        (let ((bdcheck ((regexp "multipart.*boundary=(.*)").exec
                                        request.headers."content-type"))
                              (buf ((node:require "buffer").Buffer.concat data)))
                          (if bdcheck
                              (progn
                                (let ((extra (multipart-form-data buf (second bdcheck))))
                                  (dolist (k (keys extra))
                                    (setf (aref parsed-req k) (aref extra k)))))
                              (setf parsed-req (try
                                                 (json-parse* (+ "" buf))
                                                 null)))
                          (process url parsed-req response)))))
        (process url parsed-req response))))

(defun start-server (address port)
  (let* ((http (node:require "http"))
         (server (http.createServer #'rpc-handler)))
    (set-interval #'session-cleanup 1000)
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

(export start-server
        open-session
        check-authorization)