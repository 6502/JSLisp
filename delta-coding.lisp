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

(defconstant +ALPHABET+ "ABCDEFGHIJKLMNOPQRSTUVWXYZ\
                         abcdefghijklmnopqrstuvwxyz\
                         0123456789+/")

(defun encode-uint (x)
  (if (> x 31)
      (+ (aref +ALPHABET+ (+ 32 (logand x 31))) (encode-uint (ash x -5)))
      (aref +ALPHABET+ x)))

(defun encode-int (x)
  (if (< x 0)
      (encode-uint (1+ (* x -2)))
      (encode-uint (* x 2))))

(defmacro decode-uint (delta i)
  (let ((x '#.(gensym))
        (shift '#.(gensym))
        (c '#.(gensym)))
  `(do ((,x 0)
        (,shift 0)
        (,c (index (aref ,delta (1- (incf ,i))) +ALPHABET+)))
     ((< ,c 32) (+ ,x (ash ,c ,shift)))
     (incf ,x (ash (- ,c 32) ,shift))
     (incf ,shift 5)
     (setf ,c (index (aref ,delta (1- (incf ,i))) +ALPHABET+)))))

(defun decode-int (x)
  (if (logand x 1)
      (- (ash x -1))
      (ash x -1)))

(defun delta-encode (current new)
  "Returns a compressed delta needed to compute [new] from [current]"
  (let ((res "")
        (i0 0)
        (i 0))
    (labels ((flush ()
                    (when (> i i0)
                      (incf res (+ (encode-int (- i0 i)) (slice new i0 i)))
                      (setf i0 i))))
      (do () ((>= i (length new)) (flush) res)
        (let (((sz pos) (longest-match (+ current (slice new 0 i)) (slice new i))))
          (if (<= sz 4)
              (incf i)
              (progn
                (flush)
                (incf res (+ (encode-int sz) (encode-uint pos)))
                (incf i sz)
                (setf i0 i))))))))

(defun delta-decode (current delta)
  "Returns a new version given [current] and a delta-encoded list of changes [delta]"
  (do ((res current)
       (i 0))
    ((>= i (length delta)) (slice res (length current)))
    (let ((x1 (decode-int (decode-uint delta i))))
      (if (< x1 0)
          (progn
            (incf res (slice delta i (- i x1)))
            (decf i x1))
          (let ((x2 (decode-uint delta i)))
            (incf res (slice res x2 (+ x1 x2))))))))

(export delta-encode delta-decode)
