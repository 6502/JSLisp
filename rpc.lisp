
(defmacro defun-remote (name args &rest body)
  (let ((fields (map (lambda (f)
                       (if (list? f) (first f) f))
                     args)))
    (if node-js
        `(progn
           (defun ,name ,args ,@body)
           (defobject ,#"{name}-req" ,fields)
           (defmethod rpc:process-request (req) (,#"{name}-req?" req)
                      (,name ,(map (lambda (f)
                                     `(. req ,f))
                                   fields))))
        `(progn
           (defobject ,#"{name}-req" ,fields)
           (defun ,name ,args
             (remote (,#"new-{name}-req" ,@fields)))))))

(export defun-remote)

(if node-js
    (defun process-request (req)
      (error "Unknown request {req.%class}"))
    (export process-request))
