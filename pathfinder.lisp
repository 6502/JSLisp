(import * from heap)

(defstruct state-info
  state
  total-cost
  previous-state
  heap-index)

(defun find-path (start-states next goal)
  "Path-finding algorithm:
      start-states ... list of starting states
      next ........... function that given a state returns a list of (neighbor cost) pairs
      goal ........... a function that returns true if the passed state is a goal state
   The function returns either a minimal cost path of states or null if no such a path exists"
  (let ((seen #())
        (heap (heap (lambda (a b) (<= (state-info-total-cost a) (state-info-total-cost b)))
                    (lambda (x index) (setf (state-info-heap-index x) index)))))
    (dolist (x start-states)
      (let ((state-record (make-state-info state: x
                                           total-cost: 0
                                           previous-state: null
                                           heap-index: null)))
        (setf (aref seen x) state-record)
        (heap-push state-record heap)))
    (do ((result null))
        ((or result (zero? (heap-length heap)))
         result)
      (let* ((info (heap-pop heap))
             (state (state-info-state info)))
        (if (funcall goal state)
            (progn
              (setf result (list))
              (do ((x state (state-info-previous-state (aref seen x))))
                  ((null? x) (nreverse result))
                (push x result)))
            (dolist (nx (funcall next state))
              (let ((nh (first nx))
                    (cost (second nx)))
                (let ((nh-info (aref seen nh))
                      (tc (+ (state-info-total-cost info) cost)))
                  (if nh-info
                      (when (< tc (state-info-total-cost nh-info))
                        (setf (state-info-total-cost nh-info) tc)
                        (if (state-info-heap-index nh-info)
                            (heap-fix heap (state-info-heap-index nh-info))
                            (heap-push nh-info heap)))
                      (heap-push (setf (aref seen nh)
                                       (make-state-info state: nh
                                                        total-cost: tc
                                                        previous-state: state
                                                        heap-index: null))
                                 heap))))))))))

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
                       (push (list (list xx yy)
                                   (if (or (= xx x) (= yy y))
                                       100 141))
                             moves))))
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
                (+ (slice (aref maze y) 0 x)
                   "+"
                   (slice (aref maze y) (1+ x))))))
      (dolist (L maze)
        (display L)))
    elapsed))
