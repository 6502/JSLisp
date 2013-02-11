(defun longest-match (haystack needle)
  "Returns the size and position of the longest partial match of [needle] \
   in [haystack]. Returns [(0 0)] if the first character of [needle] is not \
   present in [haystack]"
  (do ((best 0)
       (best-index 0)
       (i 0))
    ((or (>= best (length needle))
         (>= i (- (length haystack) best)))
     (list best best-index))

    (if (= (slice haystack i (+ i best 1))
           (slice needle 0 (+ best 1)))
        (progn
          (setf best-index i)
          (incf best))
        (incf i))))

(defun delta-encode (current new)
  "Returns a compressed delta needed to compute [new] from [current]"
  (let ((res (list))
        (i0 0)
        (i 0))
    (labels ((flush ()
                    (when (> i i0)
                      (push (slice new i0 i) res)
                      (setf i0 i))))
      (do () ((>= i (length new)) (flush) res)
        (let (((sz pos) (longest-match (+ current (slice new 0 i)) (slice new i))))
          (if (<= sz (length ~"{pos} {sz}"))
              (incf i)
              (progn
                (flush)
                (push (list pos sz) res)
                (incf i sz)
                (setf i0 i))))))))

(defun delta-decode (current delta)
  "Returns a new version given [current] and a delta-encoded list of changes [delta]"
  (let ((res current))
    (dolist (x delta)
      (if (string? x)
          (incf res x)
          (incf res (slice res (first x) (+ (first x) (second x))))))
    (slice res (length current))))

(export delta-encode delta-decode)
