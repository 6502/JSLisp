(defun parse-date (x)
  (let ((m ((regexp "(\\d{4})-(\\d{2})-(\\d{2})").exec x)))
    (when m
      (date (atoi (aref m 1))
            (1- (atoi (aref m 2)))
            (atoi (aref m 3))))))

(defun str-date (d)
  (if d
      (+ (rpad (d.getFullYear) 4 "0")
         "-"
         (rpad (1+ (d.getMonth)) 2 "0")
         "-"
         (rpad (d.getDate) 2 "0"))
      ""))

(export parse-date str-date)
