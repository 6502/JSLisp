(let ((functions (map (lambda (x)
                        (list (demangle (slice x 1))
                              (documentation (aref window x))))
                      (filter (lambda (x)
                                (and (aref window x)
                                     (= (slice x 0 3) "f$$")))
                              (keys window))))
      (macros (map (lambda (x)
                     (list (demangle (slice x 1))
                           (documentation (aref window x))))
                   (filter (lambda (x)
                             (and (aref window x)
                                  (= (slice x 0 3) "m$$")))
                           (keys window))))
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
        (let* ((doc (or (second f) "No documentation available"))
               (pre ""))
          (out ~"<br/>{(htmfix doc)}</div>"))))
    (out "</body></html>"))
  (set-timeout (lambda (&rest args)
                 (funcall (. document write) result))
               0))