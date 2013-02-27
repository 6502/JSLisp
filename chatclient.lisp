(defun send (server channel msg)
  "Sends [msg] (any object that can be parsed back from str-value) to the specified [channel]"
  (http "POST" ~"{server}/send?{channel}" (str-value msg)))

(defun receive (server channel handler)
  "Starts reading the specified [channel] executing function [handler] for each new message.
   Returns a function can be called to stop processing messages."
  (let ((last-received 0)
        (stopped false))
    (labels ((process (msg)
               (unless stopped
                 (unless (null? msg)
                   (dolist (x (split (uri-decode msg) "\n"))
                     (let ((ix (index ":" x)))
                       (setf last-received
                             (max last-received (1+ (read (slice x 0 ix)))))
                       (unless stopped
                         (try (funcall handler (read (slice x (1+ ix)))) null)))))
                 (http "POST" ~"{server}/receive?{channel}&{last-received}" ""
                       #'process
                       (lambda ()
                         ;; In case of failure retry after 100ms
                         (unless stopped
                           (set-timeout (lambda () (process null)) 100))))))
             (stop () (setf stopped true)))
      (process null)
      #'stop)))

(export send receive)