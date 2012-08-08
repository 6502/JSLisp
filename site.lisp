(defobject section
  (level title content))

(defobject paragraph
  (content))

(defobject heading
  (level content))

(defobject list-item
  (content))

(defobject code-section
  (title content))

(defobject sample-code
  (title link))

(defobject verbatim-html-section
  (content))

(defobject table
  (rows))

(defvar *sections* (list))

(defun parse-site (txt)
  (symbol-macrolet ((line (aref lines line-index))
                    (line+ (aref lines (1- (incf line-index))))
                    (next-line (incf line-index))
                    (eof (>= line-index (length lines)))
                    (first-char (aref line 0)))
    (do ((lines (split txt "\n"))
         (line-index 0)
         (current-section null))
        (eof)
      (case first-char
        (undefined
           ; Ignore empty lines
           next-line)
        ("@"
           ; A section
           (do ((i 1 (1+ i)))
               ((/= (aref line i) "@")
                (setf current-section (make-section :level i
                                                    :title (strip (slice line+ i))
                                                    :content (list)))
                (push current-section *sections*))))
        ("#"
           ; An heading
           (do ((i 1 (1+ i)))
               ((/= (aref line i) "#")
                  (push (make-heading :level (1+ i)
                                      :content (strip (slice line+ i)))
                        current-section.content))))
        ("-"
           ; A list item
           (do ((para (strip (slice line+ 1))))
               ((not (= first-char " "))
                  (push (make-list-item :content para)
                        current-section.content))
             (incf para line+)))
        ("+"
           ; A table
           (do ((rows (list)))
               ((and (/= first-char "+")
                     (/= first-char "|"))
                  (push (make-table :rows rows)
                        current-section.content))
             (when (= first-char "|")
               (push (map #'strip (split (slice line 1 (- (length line) 2)) "|"))
                     rows))
             next-line))
        ("<"
           ; A verbatim HTML section
           (unless (= line+ "<<")
             (error "A verbatim HTML section begins with \"<<\" on first columnn"))
           (do ((para ""))
               ((or eof (= line ">>"))
                  (push (make-verbatim-html-section :content para)
                        current-section.content))
             (incf para (+ line+ "\n")))
           (when (= line ">>")
             next-line))
        ("["
           ; A code section
           (do ((title (strip (slice line+ 1)))
                (para ""))
               ((or eof (= first-char "]"))
                  (push (make-code-section :title title
                                           :content para)
                        current-section.content))
             (incf para (+ line+ "\n")))
           (when (= first-char "]")
             next-line))
        ("!"
           ; An example program
           (let ((i (index ":" line)))
             (push (make-sample-code :title (slice line 1 i)
                                     :link (slice line (1+ i)))
                   current-section.content))
           next-line)
        (otherwise
           ; A regular paragraph, ends at first blank line
           (do ((para (strip line+)))
               ((undefined? first-char)
                  (push (make-paragraph :content para)
                        current-section.content))
             (incf para (+ " " line+))))))))

(defun htmfix (x)
  (replace (htm x)
           "\\[([^\\]]*?)\\]"
           "<span class=\"code\">$1</span>"))

(defconstant +PROLOGUE+ "<!DOCTYPE HTML>
<html>
  <head>
    <title>JsLisp</title>
    <style type=\"text/css\">
    body {
        font-family: Arial, sans-serif;
        padding: 8px;
        color: #000000;
        background-color: #FFFFFF;
    }

    pre {
        font-family: 'Courier New', courier, monospace;
        font-weight: bold;
        padding: 12px;
        color: #000080;
        background-color: #F8F8FF;
        border-radius: 8px;
        border: solid 2px #000080;
    }

    div.index {
        float: left;
        width: 200px;
        border: solid 2px #000080;
        border-radius: 8px;
        padding: 8px;
    }

    div.content {
        margin-left: 235px;
    }

    a {
        padding-left: 6px;
        padding-right: 6px;
        cursor: pointer;
        color: #0000FF;
        font-weight: bold;
        text-decoration: underline;
    }

    a:hover {
        background-color: #0000FF;
        color: #FFFFFF;
    }

    span.runcode {
        padding-left: 20px;
    }

    span.output {
        color: #800000;
    }

    span.code {
        font-family: 'Courier New', courier, monospace;
        font-weight: bold;
        color: #000080;
    }

    h1 {
        text-align: center;
    }

    table {
        border: solid 2px #000080;
        border-collapse: collapse;
        margin-left: auto;
        margin-right: auto;
    }

    td {
        border: solid 1px #000080;
        padding: 6px;
    }

    th {
        border: solid 1px #000080;
        padding: 6px;
        background-color: #E0E0FF;
        font-weight: bold;
        text-align: center;
    }

    </style>
  </head>
  <body>
    <div class=\"index\">
      <img src=\"jslisp.png\">
      <hr size=1 noshade/>
      @index@
    </div>
    <div class=\"content\">")

(defconstant +EPILOGUE+ "
    </div>
    <script>
var current_section = null;

function show(x) {
    if (current_section) current_section.style.display = 'none';
    current_section = document.getElementById(x);
    current_section.style.display = 'block';
}

function showoutput(x) {
    var nodes = document.getElementById(x).getElementsByClassName('output');
    for (var i=0; i<nodes.length; i++)
        nodes[i].style.display = (nodes[i].style.display == 'none' ? 'inline' : 'none');
}

function samplesrc(x) {
    var w = window.open('jslisp.html?src=examples/'+x, '_blank',
                        'width=800, height=800, left=100, top=100');
}

function samplerun(x) {
    var w = window.open('jslisp.html?load=examples/'+x, '_blank',
                        'width=800, height=800, left=100, top=100');
}

show('About');

    </script>
  </body>
</html>")

(defun id (x)
  (replace x "[^a-zA-Z0-9]" ""))

(defun htmcode (x)
  (replace
   (replace (htm x)
            "(^|\\n)((--&gt;|Ready\\.|WARNING:|\\*\\*ERROR\\*\\*:).*?)(?=\\n)"
            "<span class=\"output\">$1$2</span>")
   "(^|\\n)=(.*?)(?=\\n)"
   "<span class=\"output\">$1$2</span>"))

(defun generate-site ()
  (let ((result +PROLOGUE+)
        (inside-list false)
        (index ""))
    (dolist (section *sections*)
      (incf result ~"<div style=\"display:none;\" id=\"{(id section.title)}\">")
      (incf result ~"<h1>{section.title}</h1><hr/>")
      (dotimes (i (1- section.level))
        (incf index "<span style=\"display:inline-block; width:20px;\"></span>"))
      (incf index
            ~"<a onclick=\"show('{(id section.title)}')\">{(htmfix section.title)}</a><br/>")
      (dolist (x section.content)
        (cond
          ((verbatim-html-section? x)
           (incf result x.content))
          ((heading? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<h{x.level}>{(htmfix x.content)}</h{x.level}>"))
          ((sample-code? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result (htmfix x.title))
           (incf result ~"<a class=\"code\" onclick=\"samplesrc('{x.link}')\">show-source</a>")
           (incf result ~"<a class=\"code\" onclick=\"samplerun('{x.link}')\">run</a><br/>"))
          ((paragraph? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<p>{(htmfix x.content)}</p>"))
          ((list-item? x)
           (unless inside-list
             (setf inside-list true)
             (incf result "<ul>"))
           (incf result ~"<li>{(htmfix x.content)}</li>"))
          ((table? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<table>")
           (let ((first-row true))
             (dolist (row x.rows)
               (incf result "<tr>")
               (dolist (col row)
                 (incf result (cond
                                (first-row "<th>")
                                ((length (replace col "[-+0-9.]" "")) "<td>")
                                (true "<td align=right>")))
                 (incf result (htmfix col))
                 (incf result (if first-row "</th>" "</td>")))
               (incf result "</tr>")
               (setf first-row false)))
           (incf result "</table>"))
          ((code-section? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<pre><center>{(htm x.title)}")
           (when (length x.content)
             (when (funcall (. (regexp "(^|\\n)(=|-->|Ready\.)") exec) x.content)
               (incf result ~"<span class=\"runcode\"><a onclick=\"showoutput('{(id x.title)}')\">show/hide output</a></span>"))
             (incf result ~"<hr noshade size=1/></center>")
             (incf result ~"<div id=\"{(id x.title)}\">{(htmcode x.content)}</div>"))
           (incf result "</pre>"))
          (true (error "Unsupported element type"))))
      (incf result "</div>"))
    (when inside-list
      (setf inside-list false)
      (incf result "</ul>"))
    (incf result +EPILOGUE+)
    (setf result (replace result "@index@"
                          (replace index "\\$" "$$")))
    result))

(if node-js
    (eval '(progn
            (parse-site (get-file "site.txt"))
            (display (generate-site))))
    (eval '(progn
            (parse-site (http-get "site.txt"))
            (set-timeout (lambda (&rest args)
                           (funcall (. document write) (generate-site)))
             0))))
