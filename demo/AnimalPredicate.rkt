;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname AnimalPredicate) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Ein Gürteltier hat folgende Eigenschaften:
; - lebendig oder tot?
; - Gewicht in g (Gramm)
; "Zustand des Gürteltiers zu einem bestimmten Zeitpunkt"
(define-record dillo
  make-dillo ; Konstruktor
  dillo? ; Prädikat
  (dillo-alive? boolean)
  (dillo-g natural))

(: make-dillo (boolean natural -> dillo))
(: dillo-alive? (dillo -> boolean))
(: dillo-g (dillo -> natural))

(: dillo? (any -> boolean))

; Beispiele:
(define dillo1 (make-dillo #t 20000)) ; Gürteltier, lebendig, 20kg
(define dillo2 (make-dillo #f 15000)) ; Gürteltier, tot, 15kg

; Gürteltier überfahren
(: run-over-dillo (dillo -> dillo))

; objektorientiert: class Dillo   def runOver(self): self.alive = False
; Das Gürteltier wird genommen und ein Teil wird verändert,
; so dass das Gürteltier nicht mehr das Gleiche ist wie vorher.

; Testfälle:
(check-expect (run-over-dillo dillo1) (make-dillo #f 20000))
(check-expect (run-over-dillo dillo2) dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-g dillo))))

; Ein Tier ist eins der folgenden:
; - ein Gürteltier
; - ein Papagei
; handelt sich NICHT um eine Aufzählung
; gemischte Daten
(define animal
  (signature (mixed dillo parrot)))

; Ein Papagei hat folgende Eigenschaften:
; - ein Satz
; - Gewicht in g (Gramm)
(define-record parrot
  make-parrot
  parrot?
  (parrot-sentence string)
  (parrot-g natural))

; Beispiele
(define parrot1 (make-parrot "Der Schatz ist auf den Osterinseln!" 5000)) ; Papagei vom Pirat, 5kg
(define parrot2 (make-parrot "Guten Tag!" 3000)) ; netter Hauspapagei, 3kg

; Papagei überfahren
(: run-over-parrot (parrot -> parrot))

; Testfälle
(check-expect (run-over-parrot parrot1) (make-parrot "" 5000))
(check-expect (run-over-parrot parrot2) (make-parrot "" 3000))

(define run-over-parrot
  (lambda (parrot)
    (make-parrot "" (parrot-g parrot))))

; Tier überfahren
(: run-over-animal (animal -> animal))

(check-expect (run-over-animal dillo1)
              (run-over-dillo dillo1))
(check-expect (run-over-animal parrot1)
              (run-over-parrot parrot1))
(check-expect (run-over-animal dillo2)
              (run-over-dillo dillo2))
(check-expect (run-over-animal parrot2)
              (run-over-parrot parrot2))

(define run-over-animal
  (lambda (animal)
    (cond
      ((dillo? animal) (run-over-dillo animal))
      ((parrot? animal) (run-over-parrot animal)))))

(: animal? (any -> boolean))

(check-expect (animal? dillo1) #t)
(check-expect (animal? parrot1) #t)
(check-expect (animal? 5) #f)

(define animal? (lambda (a)
                  (or (dillo? a) (parrot? a))))
