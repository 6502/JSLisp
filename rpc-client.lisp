(defun remote (x)
  (let ((request (to-buffer x)))
    (let ((reply (http "POST" "process?" (uri-encode request))))
      (let ((result (from-buffer (uri-decode reply))))
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
             (defobject ,#"{name}-req" ,fields)
             (defun ,name ,args
               (remote (,#"new-{name}-req" ,@fields)))))))
