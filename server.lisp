(import * from rpc-server)

(rpc:defun login (user-name)
  (open-session user-name))

(rpc:defun remote (user-name session-id x authcode)
  (let ((proplist (check-authorization user-name session-id authcode
                                       (json* x))))
    (when (and proplist
               (find "admin" proplist))
      (toplevel-eval x))))

(defun main ()
  (start-server "127.0.0.1" 1337))

(setf (aref rpc-server:*users* "agri")
      (rpc-server:new-user "agri" -1258049249 (list "admin")))

(main)
