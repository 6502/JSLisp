(import * from heap)

(defobject info
    (state
     cost
     prev
     index))

(defun find-path (start next goal)
  "Path-finding algorithm.
   [start] is a list of starting states, [next] is a function that given a state \
   returns a list of [(neighbor cost)] pairs and [goal] is a function that returns \
   [true] if the passed state is a goal state.
   The function returns eiter a minimal cost path of states to a goal state or \
   [null] if no such a path exists."
  (let ((seen #())
        (heap (make-heap (list)
                         (lambda (a b)
                           (<= a.cost b.cost))
                         (set index))))
    (dolist (x start)
      (let ((info (make-info state: x
                             cost: 0
                             prev: null
                             index: null)))
        (setf (aref seen x) info)
        (heap-push info heap)))
    (do ((result null))
        ((or result (zero? (length heap)))
           result)
      (let* ((info (heap-pop heap))
             (state info.state))
        (if (funcall goal state)
            (progn
              (setf result (list))
              (do ((x state (aref seen x).prev))
                  ((null? x) (nreverse result))
                (push x result)))
            (dolist ((nh cost) (funcall next state))
              (let ((nh-info (aref seen nh))
                    (tc (+ info.cost cost)))
                (if nh-info
                    (when (< tc nh-info.cost)
                      (setf nh-info.cost tc)
                      (setf nh-info.prev state)
                      (if (not (null? nh-info.index))
                          (heap-fix heap nh-info.index)
                          (heap-push nh-info heap)))
                    (heap-push (setf (aref seen nh)
                                     (make-info state: nh
                                                cost: tc
                                                prev: state
                                                index: null))
                               heap)))))))))

(defun module-test ()
  (let* ((maze (list "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"
                     "x  xx                x          x"
                     "x  xx      xx    xxxxx  xxxxx   x"
                     "x  xx      xx  xxx      xx  xx  x"
                     "x  xx      xx   xxxxx   xxxxx   x"
                     "x  xx      xx      xxx  xx      x"
                     "x  xxxxxx  xx  xxxxx    xx       "
                     "x          xx           xx      x"
                     "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"))
         (h (length maze))
         (w (length (first maze)))
         (start (list 1 1))
         (next (lambda ((x y))
                 (filter (lambda (move)
                           (let (((xx yy) (first move)))
                             (and (< -1 yy h) (= " " (aref maze yy xx)))))
                         `(((,(1- x) ,y) 100)
                           ((,(1+ x) ,y) 100)
                           ((,x ,(1- y)) 100)
                           ((,x ,(1+ y)) 100)
                           ((,(1- x) ,(1- y)) 141)
                           ((,(1+ x) ,(1- y)) 141)
                           ((,(1- x) ,(1+ y)) 141)
                           ((,(1+ x) ,(1+ y)) 141)))))
         (goal (lambda ((x y))
                 (or (= x 0) (= y 0) (= y (1- h)) (= x (1- w))))))
    (let ((sol (find-path (list start)
                          next
                          goal))
          (elapsed (/ 100 (time (repeat 100
                                        (find-path (list start)
                                                   next
                                                   goal))))))
      (when sol
        (dolist (p sol)
          (let ((x (first p))
                (y (second p)))
            (setf (aref maze y)
                  (+ (slice (aref maze y) 0 x)
                     "+"
                     (slice (aref maze y) (1+ x))))))
        (dolist (L maze)
          (display L)))
      (display elapsed))))

(export find-path)
