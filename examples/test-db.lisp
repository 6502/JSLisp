(import * from gui)

(defun remote (x)
  (parse-value (http-get (+ "eval?" (uri-encode (str-value x false))))))

(defun records-window (table)
  (let* ((w (window 0 0 200 300 :title ~"Records {table}"))
         (cols (remote `(keys ,table)))
         (data (map (lambda (f)
                      (remote `(. ,table ,f)))
                    cols))
         (t (create-element "table"))
         (trh (create-element "tr")))
    (set-style t
               %/width 100
               %/height 100)
    (append-child t trh)
    (dolist (c cols)
      (let ((th (create-element "th")))
        (append-child trh th)
        (set-style th
                   backgroundColor "#EEEEEE"
                   border "solid 1px #000000"
                   px/margin 0
                   px/padding 4)
        (setf th.textContent c)))
    (dolist (r (apply #'zip data))
      (let ((tr (create-element "tr")))
        (append-child t tr)
        (dolist (field r)
          (let ((td (create-element "td")))
            (append-child tr td)
            (set-style td
                       px/padding 4
                       px/margin 0
                       border "solid 1px #000000")
            (setf td.textContent field)))))
    (append-child w.client t)
    (show-window w)))

(defun tables-window ()
  (let ((w (window 0 0 200 300 :title "Tables"))
        (tablelist (create-element "div")))
    (dolist (x (remote `*tables*))
      (let ((row (create-element "div")))
        (setf row.textContent x)
        (set-style row
                   cursor "default")
        (set-handler row onmouseover (set-style row backgroundColor "#FFFF00"))
        (set-handler row onmouseout (set-style row backgroundColor "#FFFFFF"))
        (set-handler row onclick (records-window x))
        (append-child tablelist row)))
    (append-child w.client tablelist)
    (show-window w)))

(tables-window)