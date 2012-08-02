(import * from rpc-server)
(import * from examples/pdfgrid)

(defun main ()
  (start-server "127.0.0.1" 14730))

(main)