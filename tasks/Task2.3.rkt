;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Task2.3) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Zahl zu Text machen
;(: say-number (natural -> string))
(: say-number ((enum 0 1 2 3) -> string))


(check-expect (say-number 0) "zero")
(check-expect (say-number 1) "one")
(check-expect (say-number 2) "two")
(check-expect (say-number 3) "three")

(define say-number
  (lambda (n)
    (cond
      ((= n 0) "zero")
      ((= n 1) "one")
      ((= n 2) "two")
      ((= n 3) "three"))))
