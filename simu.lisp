(defstruct link
  from to resistance current new-current)

(defstruct node
  tension in out balancing-tension)

(defvar *nodes* (list))
(defvar *links* (list))

(defun node (tension)
  (let ((n (make-node :tension (or tension 0)
                      :out (list)
                      :in (list))))
    (push n *nodes*)
    n))

(defun link (from to resistance current)
  (let ((L (make-link :from from
                      :to to
                      :resistance resistance
                      :current (or current 0))))
    (push L *links*)
    (push L (node-in to))
    (push L (node-out from))
    L))

(defun update ()
  ;; Compute current from tensions
  (dolist (L *links*)
    (setf (link-current L)
          (/ (- (node-tension (link-from L))
                (node-tension (link-to L)))
             (link-resistance L))))

  ;; Zero current at nodes
  (dolist (n *nodes*)
    ;; Compute balancing tension
    ;; sum (x - vi) / ri = 0
    ;; x sum 1/ri - sum vi/ri = 0
    ;; x = sum vi/ri / sum 1/ri
    (setf (node-balancing-tension n)
          (/ (- (reduce #'+ (map (lambda (L)
                                   (/ (node-tension (link-from L))
                                      (link-resistance L)))
                                 (node-in n)))
                (reduce #'+ (map (lambda (L)
                                   (/ (node-tension (link-to L))
                                      (link-resistance L)))
                                 (node-out n))))
             (reduce #'+ (map (lambda (L)
                                (/ (link-resistance L)))
                              (append (node-in n)
                                      (node-out n)))))))
  ;; Stabilization
  (dolist (n *nodes*)
    (setf (node-tension n)
          (/ (+ (node-tension n)
                (node-balancing-tension n))
             2))))

(defmacro nodexy (x y)
  `(aref nodes (+ (* ,y N) ,x)))

(let* ((N 5)
       (nodes (make-array (* N N)))
       (m (ash N -1)))
  (dotimes (y N)
    (dotimes (x N)
      (setf (nodexy x y) (node 0))))
  (dotimes (y N)
    (dotimes (x N)
      (when (< x (1- N))
        (link (nodexy x y) (nodexy (1+ x) y) 1))
      (when (< y (1- N))
        (link (nodexy x y) (nodexy x (1+ y)) 1))))
  (let* ((a (node 0))
         (b (node 0))
         (La (link a (nodexy (1- m) m) 1))
         (Lb (link b (nodexy (1+ m) (1+ m)) 1)))
    (dotimes (i 10000)
      (update)
      (setf (node-tension a) 1)
      (setf (node-tension b) 1)
      (when (= i 499)
        (dotimes (y N)
          (display (str-value (map (lambda (x) (node-tension (nodexy x y)))
                                   (range N)))))))))
