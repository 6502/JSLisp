(let ((modules #()))
  (dolist (k (keys window))
    (when (and (find "$$" k)
               (find (first k) "mfd"))
      (let* ((object (aref window k))
             (ix (index "$$" k))
             (module-name (demangle (+ "$$" (slice k 1 ix))))
             (name (demangle (slice k ix)))
             (module (or (aref modules module-name)
                         (setf (aref modules module-name) #((functions #())
                                                            (macros #())
                                                            (variables #()))))))
        (case (first k)
          ("m" (setf (aref module.macros name) object))
          ("f" (setf (aref module.functions name) object))
          ("d" (setf (aref module.variables name) object))))))
  (display (keys modules)))

(labels ((find-all (prefix)
           (map (lambda (x)
                        (list (let* ((ix (index "$$" x))
                                     (m (+ (demangle (+ "$$" (slice x 1 ix))) ":"))
                                     (n (demangle (slice x ix))))
                                (when (= m ":") (setf m ""))
                                (+ m n))
                              (documentation (aref window x))))
                      (filter (lambda (x)
                                (and (aref window x)
                                     (= (aref x 0) prefix)
                                     (>= (index "$$" x) 0)
                                     (= (index "$$G$35_" x) -1)))
                              (keys window)))))
  (let ((functions (find-all "f"))
        (macros (find-all "m"))
        (result ""))
    (labels ((out (x)
               (incf result x))
             (htmfix (x)
               (setf x (htm x))
               (setf x (replace x "\\n" "<br/>"))
               (setf x (replace x "\\[\\[" "<pre>"))
               (setf x (replace x "\\]\\]" "</pre>"))
               (setf x (replace x "\\[" "<span class=\"code\">"))
               (setf x (replace x "\\]" "</span>"))))
      (out "
<!DOCTYPE html>
<html>
  <head>
    <title>JsLisp reference </title>
    <style>
      .doc {
          border: solid 1px #000000;
          padding: 8px;
          margin: 16px;
      }

      pre {
          font-family: Courier New;
          font-weight: bold;
          color: #000080;
          padding: 2px;
          background-color: #F0F0FF;
      }

      h1 {
          font-family: Arial;
          font-size: 40px;
          font-weight: bold;
          text-align:center;
          color: #000080;
      }

      span.code {
          font-family: Courier New;
          font-weight: bold;
          white-space: no-wrap;
          color: #000080;
      }
    </style>
  </head>
  <body>
    <h1>JsLisp reference</h1>
")
      (dolist (group (list (list "Functions" functions)
                           (list "Macros" macros)))
        (out ~"<h2>{(first group)}</h2>")
        (dolist (f (sort (second group) (lambda (x y) (< (uppercase (first x))
                                                         (uppercase (first y))))))
          (out ~"<a href=\"#{(slice (first group) 0 1)}_{(mangle (first f))}\">")
          (out ~"<span class=\"code\">{(first f)}</span>")
          (out ~"</a> ")))
      (dolist (group (list (list "Functions" functions)
                           (list "Macros" macros)))
        (dolist (f (sort (second group) (lambda (x y) (< (uppercase (first x))
                                                         (uppercase (first y))))))
          (out ~"<div class=\"doc\"><a name=\"{(slice (first group) 0 1)}_{(mangle (first f))}\">")
          (out ~"{(slice (first group) 0 (1- (length (first group))))}: ")
          (out ~"<span class=\"code\">{(htm (first f))}</span></a>")
          (let* ((doc (or (second f) "No documentation available")))
            (out ~"<br/>{(htmfix doc)}</div>"))))
      (out "</body></html>"))
    (set-timeout (lambda ()
                   (funcall (. document write) result))
                 0)))