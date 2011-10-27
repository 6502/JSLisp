(defun find-path (start-states next goal)
  "Path-finding algorithm:
      start-states ... list of starting states
      next ........... function that given a state returns a list of neighbors
      goal ........... a function that returns true if the passed state is a goal state
   The function returns either a shortest path of states or null if no such a path exists"
  (do ((prev (let ((prev (js-object)))
               (dolist (x start-states)
                 (setf (aref prev x) null))
               prev))
       (active start-states)
       (result null))
      ((or result
           (zerop (length active)))
         result)
    (let ((new-active (list)))
      (dolist (x active)
        (dolist (n (funcall next x))
          (when (undefinedp (aref prev n))
            (setf (aref prev n) x)
            (push n new-active)
            (when (funcall goal n)
              (setf result (list))
              (do ()
                  ((not n)
                     (nreverse result))
                (push n result)
                (setf n (aref prev n)))))))
      (setf active new-active))))

(let* ((maze (list "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                   "x                               x"
                   "x  xx      xx    xxxxx  xxxxx   x"
                   "x  xx      xx  xxx      xx  xx  x"
                   "x  xx      xx   xxxxx   xxxxx   x"
                   "x  xx      xx      xxx  xx      x"
                   "x  xxxxxx  xx  xxxxx    xx       "
                   "x                               x"
                   "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
       (start (list 1 1))
       (next (lambda ((x y))
               (let ((moves (list)))
                 (dolist (xx (list (1- x) x (1+ x)))
                   (dolist (yy (list (1- y) y (1+ y)))
                     (when (and (or (/= xx x) (/= yy y))
                                (<= 0 yy (1- (length maze)))
                                (<= 0 xx (1- (length (aref maze yy))))
                              (= " " (aref (aref maze yy) xx)))
                       (push (list xx yy) moves))))
                 moves)))
       (goal (lambda ((x y))
               (or (= x 0)
                   (= y 0)
                   (= y (1- (length maze)))
                   (= x (1- (length (aref maze 0))))))))
  (let ((sol (find-path (list start)
                        next
                        goal))
        (elapsed (/ 100 (time (dotimes (i 100)
                                (find-path (list start)
                                           next
                                           goal))))))
    (when sol
      (dolist (p sol)
        (let ((x (first p))
              (y (second p)))
          (setf (aref maze y)
                (+ (subseq (aref maze y) 0 x)
                   "+"
                   (subseq (aref maze y) (1+ x))))))
      (dolist (L maze)
        (display L)))
    elapsed))
