(defun raw-replace (s x y)
  (let ((ix (index x s)))
    (when (>= ix 0)
      (setf s (+ (slice s 0 ix)
                 y
                 (slice s (+ ix (length x))))))
    s))

(let ((jslisp-html (get-file "jslisp.html"))
      (jslisp-js (get-file "jslisp.js"))
      (libs (map (lambda (fname)
                   (+ "<div style=\"display:hidden\" id=\"embedded-" fname "\">"
                      ((js-code "escape") (get-file fname))
                      "</div>"))
                 (list "boot.lisp"
                       "gui.lisp"
                       "layout.lisp"
                       "graphics.lisp"
                       "base64.lisp"
                       "locale.lisp"
                       "rpc-client.lisp"))))
  (setf jslisp-html
        (raw-replace jslisp-html
                     "<script src=\"jslisp.js\">"
                     (+ "<script>" jslisp-js)))
  (setf jslisp-html
        (raw-replace jslisp-html
                     "</script>"
                     (+ "</script>"
                        (join libs "")
                        "<script>
                         var old_http_get = f$$http_get;
                         f$$http_get=function(url, onSuccess, onFailure, binary) {
                           if (!onSuccess && !onFailure && !binary) {
                             var x = document.getElementById(\"embedded-\" + url);
                             if (x) return unescape(x.textContent);
                           }
                           return old_http_get(url, onSuccess, onFailure, binary);
                         }</script>")))
  (put-file "jslisp-standalone.html"
            jslisp-html)
  (length jslisp-html))