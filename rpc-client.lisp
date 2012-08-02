(import * from serialize)

(defun remote (x)
  (let ((request (uri-encode (serialize:to-buffer x))))
    (let ((reply (http "POST" "process?" request)))
      (let ((result (serialize:from-buffer (uri-decode reply))))
        result))))

(setf (symbol-macro 'rpc:defun)
      (lambda (name args &rest body)
        (let ((fields (map (lambda (f)
                             (if (list? f) (first f) f))
                           args)))
          `(progn
             ;;
             ;; Client side; create the tunneling stub only
             ;;
             (defobject* ,#"{name}-req" ,fields)
             (defun ,name ,args
               (remote (,#"new-{name}-req" ,@fields)))))))
