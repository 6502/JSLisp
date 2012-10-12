(defun round-robin (n)
  (let ((L (range n))
        (T (list)))
    (dotimes (i (1- n))
      (let ((pairing (zip (slice L 0 (/ n 2))
                          (reverse (slice L (/ n 2))))))
        (dolist (i (range (if (odd? i) 0 2) (/ n 2) 2))
          (setf (aref pairing i)
                (reverse (aref pairing i))))
        (push pairing T)
        (push (first (splice L 1 1)) L)))
    T))

(defun counts (T)
  (let* ((n (1+ (length T)))
         (counts (make-array (list (/ n 2) n) 0)))
    (dolist (round T)
      (enumerate (board (a b) round)
                 (incf (aref counts board a))
                 (incf (aref counts board b))))
    counts))

(defun err (counts)
  (let ((err 0))
    (dolist (row counts)
      (dolist (x row)
        (incf err (* (- x 2) (- x 2)))))
    (- err (length (first counts)))))

(import * from gui)
(import * from layout)

(defun make-window ()
  (let** ((w (window 0 0 640 480 title: "Round robin board balancer"))
          (n (add-widget w (select "N" (range 4 20 2))))
          (wtournament (add-widget w (table (make-array (list 6 6) "x"))))
          (wcounts (add-widget w (table (make-array (list 6 6) "y"))))
          (start (add-widget w (button "Restart" #'start)))
          (status (add-widget w (set-style (create-element "div")
                                           position "absolute"
                                           backgroundColor "#444444"
                                           color "#00FF00"
                                           px/fontSize 20
                                           whiteSpace "pre"
                                           fontFamily "Arial"
                                           fontWeight "bold"
                                           textAlign "center")))
          (counter 0)
          (T null)
          (counts null)
          (err null)
          (#'calc ()
            (dotimes (k 1237)
              (incf counter)
              (let ((save-T (map #'copy T))
                    (save-counts (map #'copy counts)))
                (dotimes (i (1+ (random-int 4)))
                  (let* ((row (aref T (random-int (length T))))
                         (i (random-int (length row)))
                         (j (random-int (remove i (range (length row))))))
                    (let (((a1 b1) (aref row i))
                          ((a2 b2) (aref row j)))
                      (decf (aref counts i a1))
                      (decf (aref counts i b1))
                      (decf (aref counts j a2))
                      (decf (aref counts j b2))
                      (incf (aref counts j a1))
                      (incf (aref counts j b1))
                      (incf (aref counts i a2))
                      (incf (aref counts i b2))
                      (setf (aref row i) (list a2 b2))
                      (setf (aref row j) (list a1 b1)))))
                (let ((e (err counts)))
                  (cond
                    ((< e err)
                     (setf err e)
                     (show-solution))
                    ((> e err)
                     (setf T save-T)
                     (setf counts save-counts))))))
            (setf status.textContent
                  ~"Err:{err}, Experiments:{counter}"))
          (#'start ()
            (setf counter 0)
            (setf T (round-robin (atoi (text n))))
            (setf counts (counts T))
            (setf err (err counts))
            (show-solution))
          (#'show-solution ()
            (setf wtournament
                  (dom-replace
                   wtournament
                   (table (map (lambda (row)
                                 (map (lambda (pairing)
                                        ~"{(first pairing)}-{(second pairing)}")
                                      row))
                               T))))
            (setf wcounts
                  (dom-replace
                   wcounts
                   (table (map (lambda (row)
                                 (map (lambda (value)
                                        (let ((d (set-style (create-element "div")
                                                            position "absolute"
                                                            backgroundColor
                                                            (if (/= value 1 2)
                                                                "#FF8888"
                                                                "#EEEEEE")
                                                            textAlign "center"
                                                            px/fontSize 10)))
                                          (setf d.textContent value)
                                          d))
                                      row))
                               counts))))
            (show-window w)))
    (set-layout w (V border: 8 spacing: 2
                     (dom wtournament)
                     weight: 50
                     (dom wcounts)
                     size: 40
                     (H size: 50
                        (dom n)
                        size: 80
                        (dom start)
                        size: undefined
                        (dom status))))
    (start)
    (let ((id (set-interval #'calc 0)))
      (setf w.close-cback (lambda ()
                            (clear-interval id))))
    (show-window w center: true)))

(defun main ()
  (make-window))

(main)
