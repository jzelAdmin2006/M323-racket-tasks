;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.4) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Wassertemperatur nach Erhitzen berechnen, Sieden berücksichtigen
(: heat-water-1 (real real -> real))

(check-expect (heat-water-1 -10 20) 10)
(check-expect (heat-water-1 10 20) 30)
(check-expect (heat-water-1 90 20) 100)
(check-expect (heat-water-1 99 1) 100)
(check-expect (heat-water-1 99 2) 100)

(define heat-water-1
  (lambda (temp heat)
    (cond
      ((< (+ temp heat) 100) (+ temp heat))
      ((>= (+ temp heat) 100) 100))))

#|Aufgabe 2.4
Müssen es bei den beiden Bedingungen unbedingt <= und > sein? Was passiert, wenn Du <= durch
< ersetzt und das Programm dann laufen lässt? Was passiert, wenn Du dann auch das > ersetzt –
durch >=? Warum funktioniert das Programm dann immer noch?

Antwort: Es kommt nicht darauf an, weil der einzige Unterschied ist, dass wenn (+ temp heat)
= 100 ist, dass dann stattdessen die zweite Bedingung genommen wird. Da aber (+ temp heat
sowieso 100 ist, und auch bei der zweiten Bedingung 100 zurückgegeben wird, ist das auch
komplett richtig.
|#