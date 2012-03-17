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
      (compspecs (map (lambda (x)
                        (list (demangle x)
                              (documentation (aref (js-code "jscompile") x))))
                      (keys (js-code "jscompile"))))
      (result ""))
  (labels ((out (x)
             (incf result x)))
    (out "
<!DOCTYPE html>
<html>
  <head>
    <title>JsLisp reference </title>
    <style>
      .doc { border: solid 1px #000000;
             padding: 8px;
             margin: 16px; }
      pre { font-family: Courier New;
            font-weight: bold;
            color: #000080;
            padding: 2px;
            background-color: #F0F0FF; }
      h1 { font-family: Arial;
           font-size: 40px;
           font-weight: bold;
           text-align:center;
           color: #000080; }
    </style>
  </head>
  <body>
    <h1>JsLisp reference</h1>
")
    (dolist (group (list (list "Functions" functions)
                         (list "Macros" macros)
                         (list "Compile specializations" compspecs)))
      (out ~"<h2>{(first group)}</h2>")
      (dolist (f (sort (second group) (lambda (x y) (< (first x) (first y)))))
        (out ~"<a href=\"#{(slice (first group) 0 1)}_{(mangle (first f))}\">")
        (out ~"<span style=\"white-space:nowrap; font-family:Courier new; font-weight:bold\">{(first f)}</span>")
        (out ~"</a> ")))
    (dolist (group (list (list "Functions" functions)
                         (list "Macros" macros)
                         (list "Compile specializations" compspecs)))
      (dolist (f (sort (second group) (lambda (x y) (< (first x) (first y)))))
        (out ~"<div class=\"doc\"><a name=\"{(slice (first group) 0 1)}_{(mangle (first f))}\">")
        (out ~"{(slice (first group) 0 (1- (length (first group))))}: ")
        (out ~"<span style=\"font-weight:bold; font-family:Courier New\">{(htm (first f))}</span></a>")
        (let* ((doc (or (second f) "No documentation available"))
               (i (index "\n" doc))
               (pre ""))
          (when (/= i -1)
            (setf pre (slice doc 0 i))
            (setf doc (slice doc (1+ i))))
          (out ~"<pre>{(htm pre)}</pre>{(htm doc)}</div>"))))
    (out "</body></html>"))
  (let ((w (funcall (. window open) "" "reference" "width=800, height=800, left=100, top=100")))
    (funcall (. w document write) result)))
