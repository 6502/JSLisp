(import * from rpc)
(import * from rpc-test)

(dotimes (i 20)
  (display (square i))
  (display (cube i)))
