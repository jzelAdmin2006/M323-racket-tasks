;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Summe von Zahlen von 1 bis n
(: gaussian-sum (natural -> natural))

; Testfälle für Fakultät
(check-expect (gaussian-sum 0) 0)
(check-expect (gaussian-sum 1) 1)
(check-expect (gaussian-sum 2) 3)
(check-expect (gaussian-sum 3) 6)
(check-expect (gaussian-sum 4) 10)
(check-expect (gaussian-sum 100) 5050)

(define gaussian-sum
  (lambda (n)
    (if (= n 0)
     0
     (+ n (gaussian-sum (- n 1)))
     )))
