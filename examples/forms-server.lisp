(import * from serialize)
(import * from gui)
(import * from rpc-server)
(import examples/forms)

(defun main ()
  (start-server "127.0.0.1" 1337))

(main)
