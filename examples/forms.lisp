(defobject document (name description pages))

(defobject page (width height entities))

(defobject rect (x0 y0 x1 y1 color))

(defobject image (x0 y0 x1 y1 url))

(defobject text (x0 y0 x1 y1
                 text color size
                 family bold italic))

(defvar *docs* null)

(when node-js
  (defun load-documents ()
    (unless *docs*
      (setf *docs* #())
      (dolist (L (split (get-file "forms.dat") "\n"))
        (unless (= 0 (length L))
          (let ((x (from-buffer (uri-decode L))))
            (cond
              ((= (first x) "+")
               (setf (aref *docs* (second x)) (third x)))
              ((= (first x) "-")
               (remove-key *docs* (second x)))
              (true (error "Unknown command {(str-value x)}")))))))))

(rpc:defun list-documents ()
  (load-documents)
  (map (lambda (name) (list name (aref *docs* name).description))
       (keys *docs*)))

(rpc:defun get-document (name)
  (load-documents)
  (aref *docs* name))

(rpc:defun save-document (doc)
  (load-documents)
  (setf (aref *docs* doc.name) doc)
  ((js-code "require('fs')").appendFile "forms.dat"
   (+ (uri-encode (to-buffer `("+" ,doc.name ,doc))) "\n")
   (lambda (err) (when err (display "ERROR: {err}")))))

(rpc:defun delete-document (name)
  (load-documents)
  (remove-key *docs* name)
  ((js-code "require('fs')").appendFile "forms.dat"
   (+ (uri-encode (to-buffer `("-" ,name))) "\n")
   (lambda (err) (when err (display "ERROR: {err}")))))

(export new-rect rect?
        new-image image?
        new-text text?
        new-document document?
        new-page page?
        list-documents
        get-document
        save-document
        delete-document)