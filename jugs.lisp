(defun find-solution (start-state
                      next-state
                      goal)
  "Finds a solution of a generic game where start-state is an opaque object
containing the starting position, next-state is a function that given a
current state returns what are next possible states and goal is a function
that given a state returns true if the game goal is satisfied.
The function returns either null if there is no solution or a list of
steps starting from start-state and reaching first state that satisfies
game goals."

  (do ((seen #())
       (active-states (list (list start-state null)))
       (solution null))
      ((or solution (= 0 (length active-states)))
       solution)
    (let ((next-active-states (list)))
      (dolist (s active-states)
        (if (funcall goal (first s))
            (progn
              (setf solution (list))
              (do ((x s (second x)))
                  ((null? x))
                (push (first x) solution))
              (nreverse solution))
            (dolist (ns (funcall next-state (first s)))
              (let ((key (str-value ns)))
                (unless (aref seen key)
                  (setf (aref seen key) true)
                  (push (list ns s) next-active-states))))))
      (setf active-states next-active-states))))

(find-solution
 (list 0 0)
 (lambda ((a b))
   (list (list 3 b)
         (list a 5)
         (list 0 b)
         (list a 0)
         (let ((q (min (list a (- 5 b)))))
           (list (- a q) (+ b q)))
         (let ((q (min (list b (- 3 a)))))
           (list (+ a q) (- b q)))))
 (lambda ((a b))
   (= b 4)))
