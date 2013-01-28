(import * from gui)
(import * from layout)

(defobject page ((title "<senza titolo>")
                 (date-from 0)
                 (date-to 9999999999)
                 (content "Titolo
                           ======

                           Contenuto della pagina

                           Parte in **grassetto**

                           Elenco puntato
                           - prima voce
                           - seconda voce
                             in cui sono andato anche a capo
                           - terza voce

                           ! *colonna 1 ! *colonna 2
                           ! dato 1     ! dato 2
                           ! =centrato  ! >destra

                           [children]")
                 (children (list))))

(defun tag (txt)
  (cond
    ((= (slice txt 0 2) "b ")
     ~"<b>{(slice txt 2)}</b>")
    ((= (slice txt 0 2) "h ")
     ~"<h2>{(slice txt 2)}</h2>")
    ((= (slice txt 0 2) "i ")
     ~"<img class=\"image\" src=\"{(slice txt 2)}\">")
    ((= (slice txt 0 4) "fen ")
     (let* ((sz 32)
            (res ~"<div style=\"margin-left:auto; margin-right:auto; \
                   width:{(* sz 8)}px; height:{(* sz 8)}px; \
                   box-shadow: 4px 4px 4px rgba(0,0,0,0.5)\">")
            (pos (make-array (list 8 8)))
            (a "#AABBCC")
            (b "#CCDDEE"))
       (do ((y 0)
            (x 0)
            (i 4 (1+ i)))
         ((>= i (length txt)))
         (cond
           ((<= "1" (aref txt i) "8")
            (incf x (atoi (aref txt i))))
           ((find (aref txt i) "prnbqkPRNBQKx")
            (setf (aref pos y x)
                  (let ((ix (index (aref txt i) "prnbqkPRNBQKx")))
                    (slice "bpbrbnbbbqbkwpwrwnwbwqwkdt" (* ix 2) (+ 2 (* ix 2)))))
            (incf x)))
         (when (= x 8)
           (setf x 0)
           (incf y)))
       (dotimes (y 8)
         (incf res ~"<div style=\"height:{sz}px\">")
         (dotimes (x 8)
           (incf res ~"<div style=\"width:{sz}px; height:{sz}px; display:inline-block; \
                       background-color:{(if (odd? (+ y x)) a b)}\">")
           (when (aref pos y x)
             (incf res ~"<img src=\"examples/img/{(aref pos y x)}.png\" width={sz} height={sz}>"))
           (incf res ~"</div>"))
         (incf res ~"</div>"))
       (incf res ~"</div>")))
    ((find "-&gt;" txt)
     (let ((p (index "-&gt;" txt)))
       ~"<a href=\"{(slice txt (+ 5 p))}\">\
         {(slice txt 0 p)}\
         </a>"))
    (true "<span style=\"color:#F00\">**INVALID TAG**</span>")))

(defun format (txt)
  (setf txt (htm txt))
  (setf txt (replace txt "^ *$" ""))
  (setf txt (replace txt "\\n([!-]) " "\n\n$1 "))
  (setf txt (replace txt
                     "^([^\\n]*)\\n==+\\n"
                     "[h $1]\n"))
  (setf txt (replace txt
                     "\\*\\*([^*]+)\\*\\*"
                     "[b $1]"))
  (setf txt (replace txt "\\[([^\\]]*)\\]"
                     (lambda (txt)
                       (tag (slice txt 1 -1)))))
  (let ((res "")
        (li false)
        (tab false))
    (dolist (p (split txt (regexp "\\n\\n+")))
      (setf p (strip p))
      (setf p (replace p "\\n" "<br/>"))
      (when (> (length p) 0)
        (cond
          ((= (slice p 0 2) "- ")
           (unless li
             (setf li true)
             (incf res "<ul>"))
           (incf res ~"<li>{(slice p 2)}</li>"))
          ((= (slice p 0 2) "! ")
           (unless tab
             (setf tab true)
             (incf res "<center><table>"))
           (let ((cells (+ "<tr><td>"
                           (replace (slice p 2) "!" "</td><td>")
                           "</td></tr>")))
             (setf cells (replace cells
                                  "<td> *\\*([^<]*)</td>"
                                  "<th>$1</th>"))
             (setf cells (replace cells
                                  "<td> *=([^<]*)</td>"
                                  "<td style=\"text-align: center\">$1</td>"))
             (setf cells (replace cells
                                  "<td> *&gt;([^<]*)</td>"
                                  "<td style=\"text-align: right\">$1</td>"))
             (incf res cells)))
          (true
            (when tab
              (setf tab false)
              (incf res "</table></center>"))
            (when li
              (setf li false)
              (incf res "</ul>"))
            (incf res ~"<p>{p}</p>")))))
    ~"<div>
      <style type=\"text/css\">
         img.image \\{ margin-left: auto;
                       margin-right: auto;
                       max-width: 400px;
                       display: block }
         body \\{ font-family: Arial;
                  font-size: 16px;
                  color: #000040 }
         table \\{ border-collapse: collapse; }
         li \\{ margin-bottom: 0.5em; }
         td \\{ border: solid 1px #000000;
                padding: 4px; }
         th \\{ border: solid 1px #000000;
                background-color: #FFFFFF;
                padding: 8px;
                text-align: center; }
      </style>
      {res}
      </div>"))

(defun edit-page (page &optional cback)
  (let** ((w (window 0 0 0.75 0.75 title: "Editing pagina"))
          (title (add-widget w (input "titolo")))
          (date (add-widget w (button "date" #'date)))
          (commenti (add-widget w (checkbox "Accetta commenti")))
          (text (add-widget w (text-area "Sorgente")))
          (html (add-widget w (set-style (create-element "div")
                                         overflow "auto"
                                         backgroundColor "#EEEEEE")))
          (ok (add-widget w (button "OK" #'ok)))
          (cancel (add-widget w (button "Annulla" #'cancel cancel: true)))
          (last-text null)
          (#'date ())
          (#'ok ()
            (setf page.title (text title))
            (setf page.content (text text))
            (hide-window w)
            (when cback (funcall cback true)))
          (#'cancel ()
            (hide-window w)
            (when cback (funcall cback false))))
    (set-layout w (H border: 8 spacing: 8
                     (V size: 40
                        (H (dom title)
                           size: 60
                           (V :filler: size: 28 (dom date)))
                        size: 20
                        (H (dom commenti))
                        size: undefined
                        (dom text)
                        size: 30
                        (H :filler:
                           size: 80
                           (dom ok)
                           (dom cancel)
                           :filler:))
                     (dom html)))
    (let ((t (set-interval (lambda ()
                             (when (/= last-text (text text))
                               (setf last-text (text text))
                               (setf html.innerHTML (format last-text))))
                           1000)))
      (setf w.close-cback (lambda () (clear-interval t))))
    (setf (text title) page.title)
    (setf (text text) page.content)
    (focus text)
    (show-window w center: true)))

(defun site-editor ()
  (let** ((w (window 0 0 0.75 0.75 title: "Site editor"))
          (treeview (add-widget w (set-style (create-element "div")
                                             overflow "auto"
                                             backgroundColor "#EEEEEE")))
          (root (make-page title: "pagina-iniziale"
                           content: "Pagina iniziale
                                     ===============

                                     Questa e' la prima pagina del sito"))
          (#'rebuild ()
            (do () ((not treeview.firstChild))
              (treeview.removeChild treeview.firstChild))
            (labels ((add (parent x xp)
                          (let* ((entry (append-child parent
                                                      (set-style (create-element "div")
                                                                 border "solid 1px #000000"
                                                                 position "relative"
                                                                 px/padding 8
                                                                 px/margin 8)))
                                 (title (append-child entry
                                                      (set-style (create-element "div")
                                                                 display "inline-block"
                                                                 cursor "pointer")))
                                 (add (append-child entry
                                                    (set-style (create-element "input")
                                                               position "absolute"
                                                               px/top 4
                                                               px/right 4
                                                               px/height 26
                                                               px/width 60)))
                                 (delete (if xp
                                             (append-child entry
                                                           (set-style (create-element "input")
                                                                      position "absolute"
                                                                      px/top 4
                                                                      px/right 68
                                                                      px/height 26
                                                                      px/width 60))
                                             null)))
                            (setf add.type "button")
                            (setf add.value "add")
                            (set-handler add onclick
                              (push (make-page) x.children)
                              (rebuild))
                            (when delete
                              (setf delete.type "button")
                              (setf delete.value "delete")
                              (set-handler delete onclick
                                (nremove x xp.children)
                                (rebuild)))
                            (setf title.textContent x.title)
                            (set-handler title onmousedown
                              (edit-page x (lambda (x)
                                             (when x (rebuild)))))
                            (dolist (y x.children)
                              (add entry y x)))))
              (add treeview root null))))
    (rebuild)
    (set-layout w (V border: 8 spacing: 8
                     (dom treeview)))
    (show-window w center: true)))

(defun main ()
  (site-editor))

(main)
