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

(defun main ()
  (start-server "127.0.0.1" 1337))

(setf (aref rpc-server:*users* "agri")
      (rpc-server:new-user "agri" -1258049249 (list "admin")))

(main)
