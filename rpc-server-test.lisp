(import * from rpc-test)

(defun main ()
  (rpc:start-server "127.0.0.1" 1337))

(main)