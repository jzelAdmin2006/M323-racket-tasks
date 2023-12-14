;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task6.12) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; ----- Definitionen der Tiere -----
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

; ----- Listendefinition -----
; Eine Liste aus Zahlen ist eins der folgenden:
; - die leere Liste
; - ein Cons (Paare) besteht aus einer Zahl und einer Liste (Selbstbezug) der restlichen Elemente
#;(define list-of-number
    (signature (mixed empty-list cons-of-number)))

; Eine Liste von a's (Tieren und Zahlen) ist eins der folgenden:
; - die leere Liste
; - ein Cons besteht aus einem a und einer Liste der restlichen Elemente
(define list-of ; list-of ist eine Funktion für eine Signatur. Es kommt eine Signatur der Listenelemente herein und es kommt die Listen, die auf diesen Elementen bestehen, heraus.
  (lambda (a) ; Wir müssen das a binden -> lambda Ausdruck
    (signature (mixed empty-list (cons-of a))))) ; (cons-of a) ist eine Abstraktion für beliebige Signaturen, bei welchem "a" gebunden werden muss.

; Ein Cons besteht aus:
; - einem a (erstes Element)
; - einer Liste der restlichen Elemente
(define-record (cons-of a) ; Klammer setzen, für (cons-of a) können wir eine beliebige Signatur einsetzen. (Generischer Typ in Python oder Java) -> Ursprung dieses features ist die funktionale Programmierung
  cons
  cons?
  (first a) ; An dieser zentralen Stelle muss nun a stehen und nicht mehr number
  (rest (list-of a))) ; Der Selbstbezug muss auf die oben geschriebene list-of Definition hinweisen.

(define list-of-number (list-of number))

; Eine leere Liste besteht aus ... nichts.
(define-record empty-list
  make-empty 
  empty?)

; Beispiel
(define empty (make-empty))

; Beispiele
(define list0 empty) ; leere Liste
(define list1 (cons 17 empty)) ; Eine einelementige Liste: 17 (+ empty)
(define list2 (cons 5 (cons 17 empty))) ; Eine 2elementige Liste: 5 17
(define list3 (cons 3 list2)) ; Eine 3elementige Liste: 3 5 17
(define list4 (cons 2 (cons 3 (cons 5 (cons 10 empty)))))
(define list5 (cons 12 (cons -15 (cons -5 (cons 4 empty)))))

(: concatenate (list-of-number list-of-number -> list-of-number))

(check-expect (concatenate list0 list0) empty)
(check-expect (concatenate list0 list1) list1)
(check-expect (concatenate list1 list0) list1)
(check-expect (concatenate list1 list1) (cons 17 (cons 17 empty)))
(check-expect (concatenate list1 list2) (cons 17 (cons 5 (cons 17 empty))))
(check-expect (concatenate list2 list3) (cons 5 (cons 17 (cons 3 (cons 5 (cons 17 empty))))))
(check-expect (concatenate list3 list4) (cons 3 (cons 5 (cons 17 (cons 2 (cons 3 (cons 5 (cons 10 empty))))))))
(check-expect (concatenate list4 list5) (cons 2 (cons 3 (cons 5 (cons 10 (cons 12 (cons -15 (cons -5 (cons 4 empty)))))))))

(define concatenate
  (lambda (a b)
    (cond
      ((empty? a) b)
      (else
       (define r (rest a))
       (cons (first a) (concatenate r b))))))

; Kein Resultat
(define-record no-result
  make-no-result
  no-result?)

; Die kleinste Zahl aus einer Liste von Zahlen bekommen
(: list-min (list-of-number -> (mixed number no-result)))

; Testfälle
(check-expect (list-min list0) (make-no-result))
(check-expect (list-min list2) 5)
(check-expect (list-min list3) 3)

(define list-min
  (lambda (list)
    (cond
      ((empty? list) (make-no-result))
      (else ; (cons? list)
       (define rest-min (list-min (rest list)))
       (if (no-result? rest-min)
           (first list)
           (min (first list) rest-min))))))

; Das erste Vorkommen einer gegebenen Zahl aus einer Liste entfernen
(: delete-once (number list-of-number -> list-of-number))

; Testfälle
(check-expect (delete-once 0 list0) list0)
(check-expect (delete-once 17 list1) empty)
(check-expect (delete-once 5 list2) (cons 17 empty))
(check-expect (delete-once 5 list3) (cons 3 (cons 17 empty)))
(check-expect (delete-once 10 list4) (cons 2 (cons 3 (cons 5 empty))))
(check-expect (delete-once 5 (concatenate list3 list3)) (cons 3 (cons 17 (cons 3 (cons 5 (cons 17 empty))))))

(define delete-once
  (lambda (n list)
    (cond
      ((empty? list) empty)
      (else
       (define f (first list))
       (define r (rest list))
       (if (= f n) r (cons f (delete-once n r)))))))

; Liste von Zahlen in aufsteigender Reihenfolge sortieren
(: sort (list-of-number -> list-of-number))

; Testfälle
(check-expect (sort list0) list0)
(check-expect (sort list1) list1)
(check-expect (sort list2) list2)
(check-expect (sort (cons 4 (cons 3 (cons 6 (cons 1 empty))))) (cons 1 (cons 3 (cons 4 (cons 6 empty)))))
(check-expect (sort (cons 9 (cons -4 (cons 0 empty)))) (cons -4 (cons 0 (cons 9 empty))))
(check-expect (sort (cons 2 (cons 3 (cons 2 (cons 7 empty))))) (cons 2 (cons 2 (cons 3 (cons 7 empty)))))

(define sort
  (lambda (list)
    (cond
      ((empty? list) empty)
      (else
       (define min (list-min list))
       (cons min (sort (delete-once min list)))))))
