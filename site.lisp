(defstruct section
  title content)

(defstruct paragraph
  content)

(defstruct heading
  level content)

(defstruct list-item
  content)

(defstruct code-section
  title content)

(defstruct verbatim-html-section
  content)

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
           (setf current-section (make-section :title (strip (slice line+ 1))
                                               :content (list)))
           (push current-section *sections*))
        ("#"
           ; An heading
           (do ((i 0 (1+ i)))
               ((/= (aref line i) "#")
                  (push (make-heading :level (1+ i)
                                      :content (strip (slice line+ i)))
                        (section-content current-section)))))
        ("-"
           ; A list item
           (do ((para (strip (slice line+ 1))))
               ((not (= first-char " "))
                  (push (make-list-item :content para)
                        (section-content current-section)))
             (incf para line+)))
        ("<"
           ; A verbatim HTML section
           (unless (= line+ "<<")
             (error "A verbatim HTML section begins with \"<<\" on first columnn"))
           (do ((para ""))
               ((or eof (= line ">>"))
                  (push (make-verbatim-html-section :content para)
                        (section-content current-section)))
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
                        (section-content current-section)))
             (incf para (+ line+ "\n")))
           (when (= first-char "]")
             next-line))
        (otherwise
           ; A regular paragraph, ends at first blank line
           (do ((para (strip line+)))
               ((undefinedp first-char)
                  (push (make-paragraph :content para)
                        (section-content current-section)))
             (incf para (+ " " line+))))))))

(defun htmfix (x)
  (replace (htm x)
           "\\[([^\\]]*?)\\]"
           "<span class=\"code\">$1</span>"))

(defconstant +PROLOGUE+ "<!DOCTYPE HTML>
<html>
  <head>
    <link href='http://fonts.googleapis.com/css?family=Droid+Serif:400,700' rel='stylesheet' type='text/css'>
    <title>JsLisp</title>
    <style type=\"text/css\">
    body {
        font-family: 'Droid Serif', serif;
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

    span.code {
        font-family: 'Courier New', courier, monospace;
        font-weight: bold;
        color: #000080;
    }

    h1 {
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

function run(x) {
    var content = document.getElementById(x).innerText;
    alert(content);
}

show('About');

    </script>
  </body>
</html>")

(defun id (x)
  (replace x "[^a-zA-Z0-9]" ""))

(defun generate-site ()
  (let ((result +PROLOGUE+)
        (inside-list false)
        (index ""))
    (dolist (section *sections*)
      (incf result ~"<div style=\"display:none;\" id=\"{(id (section-title section))}\">")
      (incf result ~"<h1>{(section-title section)}</h1><hr/>")
      (incf index
            ~"<a onclick=\"show('{(id (section-title section))}')\">{(htmfix (section-title section))}</a><br/>")
      (dolist (x (section-content section))
        (cond
          ((verbatim-html-section? x)
           (incf result (verbatim-html-section-content x)))
          ((heading? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<h{(heading-level x)}>{(htmfix (heading-content x))}</h{(heading-level x)}>"))
          ((paragraph? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<p>{(htmfix (paragraph-content x))}</p>"))
          ((list-item? x)
           (unless inside-list
             (setf inside-list true)
             (incf result "<ul>"))
           (incf result ~"<li>{(htmfix (list-item-content x))}</li>"))
          ((code-section? x)
           (when inside-list
             (setf inside-list false)
             (incf result "</ul>"))
           (incf result ~"<pre><center>{(htm (code-section-title x))}")
           ;(incf result ~"<span class=\"runcode\"><a onclick=\"run('{(id (code-section-title x))}')\">Run</a></span>")
           (incf result ~"<hr noshade size=1/></center>")
           (incf result ~"<div id=\"{(id (code-section-title x))}\">{(htm (code-section-content x))}</div></pre>"))
          (true (error "Unsupported element type"))))
      (incf result "</div>"))
    (when inside-list
      (setf inside-list false)
      (incf result "</ul>"))
    (incf result +EPILOGUE+)
    (setf result (replace result "@index@"
                          (replace index "\\$" "$$")))
    (set-timeout (lambda (&rest args)
                   (funcall (. document write) result))
                 0)))

(parse-site (http-get "site.txt"))
(generate-site)