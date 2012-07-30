(import * from rpc)

(defun-remote square (x)
  (* x x))

(defun-remote cube (x)
  (* x (square x)))

(export square cube)
