(defun remote (x)
  (let ((request (json* x)))
    (let ((reply (http "POST" "process?" request)))
      (let ((result (json-parse* reply)))
        result))))

(setf (symbol-macro 'rpc:defun)
      (lambda (name args &rest body)
        (declare (ignorable body))
        (let ((fields (filter (lambda (x) (/= x '&optional))
                              (map (lambda (f)
                                     (if (list? f) (first f) f))
                                   args))))
          `(progn
             ;;
             ;; Client side; create the tunneling stub only
             ;;
             (defobject ,#"{name}-req" ,fields)
             (defun ,name ,args
               (remote (,#"new-{name}-req" ,@fields)))))))
