(import * from pdf)

(rpc:defun grid (text size
                 width height
                 rows cols
                 x0 y0
                 dx dy)
  (let ((pdf (pdf ((list width height)
                   (if (< width height) "portrait" "landscape"))
                  (font-size size)
                  (font "Helvetica")
                  (dotimes (r rows)
                    (dotimes (c cols)
                      (text text
                            (+ x0 (* c dx))
                            (+ y0 (* r dy))))))))
    (pdf.write "result.pdf")
    "result.pdf"))

(export grid)
