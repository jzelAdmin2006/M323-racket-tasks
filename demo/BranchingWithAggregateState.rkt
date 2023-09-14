;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname RamificationsWithAggregateState) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Datendefinition
; Ein Aggregatzustand ist eins der folgenden:
; - fest
; - flüssig
; - gasförmig

; "eins der folgenden": Fallunterscheidung
; hier: Spezialfall "Aufzählung"

(define state
  (signature (one-of "solid" "liquid" "gas")))

; Aggregatzustand von Wasser berechnen
(: water-state (number -> state))

(check-expect (water-state 20) "liquid")
(check-expect (water-state -20) "solid")
(check-expect (water-state 200) "gas")
(check-expect (water-state 0) "solid")
(check-expect (water-state 100) "gas")

(define water-state
  (lambda (temp)
    (cond
      ((<= temp 0) "solid")
      ((and (> temp 0) (< temp 100)) "liquid")
      ((>= temp 100) "gas"))))
  