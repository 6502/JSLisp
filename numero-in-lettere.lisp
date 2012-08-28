(defun numero-in-lettere (x)
  (if (< x 0)
      (+ "meno " (numero-in-lettere (- x)))
      (labels ((numero (x)
                 (cond
                   ((>= x 2000000000)
                    (+ (numero (floor (/ x 1000000000)))
                       "miliardI"
                       (numero (% x 1000000000))))
                   ((>= x 1000000000)
                    (+ "unmiliardO" (numero (- x 1000000000))))
                   ((>= x 2000000)
                    (+ (numero (floor (/ x 1000000)))
                       "milionI"
                       (numero (% x 1000000))))
                   ((>= x 1000000)
                    (+ "unmilionE" (numero (- x 1000000))))
                   ((>= x 2000)
                    (+ (numero (floor (/ x 1000)))
                       "milA"
                       (numero (% x 1000))))
                   ((>= x 1000)
                    (+ "millE" (numero (- x 1000))))
                   ((>= x 200)
                    (+ (numero (floor (/ x 100)))
                       "centO"
                       (numero (% x 100))))
                   ((>= x 100)
                    (+ "centO" (numero (- x 100))))
                   ((>= x 20)
                    (+ (aref '("venti" "trenta" "quaranta" "cinquanta" "sessanta"
                               "settanta" "ottanta" "novanta")
                             (- (floor (/ x 10)) 2))
                       (numero (% x 10))))
                   (true
                    (aref '("" "uno" "due" "tre" "quattro" "cinque" "sei" "sette"
                            "otto" "nove" "dieci" "undici" "dodici" "tredici" "quattordici"
                            "quindici" "sedici" "diciassette" "diciotto" "diciannove")
                          x)))))
        (if (= x 0)
            "zero"
            (let ((s (numero x)))
              (setf s (replace s "[aeiou]uno" "uno"))
              (setf s (replace s "[aeiou]otto" "otto"))
              (lowercase s))))))

(export numero-in-lettere)