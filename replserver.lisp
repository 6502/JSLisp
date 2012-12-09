(import * from rpc-server)

(rpc:defun login (user-name)
  (open-session user-name))

(rpc:defun rget-file (user-name session-id filename authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* filename))))
    (when (and proplist
               (find "admin" proplist))
      (try (get-file filename) null))))

(rpc:defun rput-file (user-name session-id filename content authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* (list filename content)))))
    (when (and proplist
               (find "admin" proplist))
      (put-file filename content)
      true)))

(rpc:defun rlist-files (user-name session-id path authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* path))))
    (when (and proplist
               (find "admin" proplist))
      ((node:require "fs").readdirSync path))))

(rpc:defun rping (user-name session-id authcode)
  (check-authorization user-name session-id authcode "null"))

(defvar *processes* #())
(defvar *process-id* 0)
(defobject process (proc user-name output exit-code))

(rpc:defun rterminal (user-name session-id authcode)
  (let ((proplist (check-authorization user-name session-id authcode "null")))
    (when (and proplist
               (find "admin" proplist))
      (let* ((proc ((node:require "child_process").spawn
                    "/usr/bin/script"
                    (list "-q" "-f" "/dev/null")
                    #((stdio "pipe"))))
             (output (list))
             (p (new-process proc user-name output null))
             (id (incf *process-id*)))
        (proc.stdout.on "data" (lambda (data)
                                 (push (data.toString "utf-8") output)))
        (proc.stderr.on "data" (lambda (data)
                                 (push (data.toString "utf-8") output)))
        (proc.on "exit" (lambda (code)
                          (display ~"Terminal {id} ({user-name}) exit-code: {code}")
                          (setf p.exit-code code)))
        (setf (aref *processes* id) p)
        (display ~"Started terminal {id} ({user-name})")
        id))))

(rpc:defun rterminal-send (user-name session-id id x authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* (list id x)))))
    (when (and proplist
               (find "admin" proplist)
               (aref *processes* id)
               (= user-name (aref *processes* id).user-name))
      (let ((p (aref *processes* id)))
        (when (null? p.exit-code)
          (p.proc.stdin.write x))))))

(rpc:defun rterminal-receive (user-name session-id id authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* id))))
    (when (and proplist
               (find "admin" proplist)
               (aref *processes* id)
               (= user-name (aref *processes* id).user-name))
      (let ((p (aref *processes* id)))
        (list p.exit-code (splice p.output))))))

(rpc:defun rterminal-detach (user-name session-id id authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* id))))
    (when (and proplist
               (find "admin" proplist)
               (aref *processes* id)
               (= user-name (aref *processes* id).user-name))
      (display ~"Detaching terminal {id} ({user-name})")
      (remove-key *processes* id))))

(defun main ()
  (start-server "127.0.0.1" 1337))

(setf (aref rpc-server:*users* "agri")
      (rpc-server:new-user "agri" -1258049249 (list "admin")))

(main)
