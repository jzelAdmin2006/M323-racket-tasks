;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.12) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Kartentyp kann einer der folgenden sein:
; - Weiss
; - Gold
; - Platin
; - Schwarz
(define cardtype
  (signature (one-of "white" "gold" "platinum" "black")))

; Kartentyp gemäss Umsatz zurückgeben
(: card-type (number -> cardtype))

(check-expect (card-type 14999) "white")
(check-expect (card-type 15000) "gold")
(check-expect (card-type 49999) "gold")
(check-expect (card-type 50000) "platinum")
(check-expect (card-type 150000) "platinum")
(check-expect (card-type 150001) "black")


(define card-type
  (lambda (revenue)
    (cond
      ((< revenue 15000) "white")
      ((and (>= revenue 15000) (< revenue 50000)) "gold")
      ((and (>= revenue 50000) (<= revenue 150000)) "platinum")
      ((> revenue 150000) "black"))))
