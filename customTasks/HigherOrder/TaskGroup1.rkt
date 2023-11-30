;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname TaskGroup1) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
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

; Testfälle:
(check-expect (run-over-dillo dillo1) (make-dillo #f 20000))
(check-expect (run-over-dillo dillo2) dillo2)

(define run-over-dillo
  (lambda (dillo)
    (make-dillo #f (dillo-g dillo))))

; Ein Tier ist eins der folgenden:
; - ein Gürteltier
; - ein Papagei
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

; ----- Funktionen auf Listen -----

(define list-of-number (signature (list-of number)))

; Beispiele
(define list0 empty) ; leere Liste
(define list1 (cons 17 empty)) ; Eine einelementige Liste: 17 (+ empty)
(define list2 (cons 5 (cons 17 empty))) ; Eine 2elementige Liste: 5 17
(define list3 (cons 3 list2)) ; Eine 3elementige Liste: 3 5 17

; ---- Funktionen, welche von den Listen konsumieren ----

; Summe der Elemente einer Liste berechnen
(: list-sum (list-of-number -> number))

; Testfälle
(check-expect (list-sum list0) 0)
(check-expect (list-sum list1) 17)
(check-expect (list-sum list2) 22)
(check-expect (list-sum list3) 25)

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (list-sum (rest list)))
       ))))

; Produkt der Elemente einer Liste
(: list-product (list-of-number -> number))

; Testfälle
(check-expect (list-product list0) 1)
(check-expect (list-product list1) 17)
(check-expect (list-product list2) 85)
(check-expect (list-product list3) 255)

(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1)
      ((cons? list)
       (* (first list)
          (list-product (rest list)))))))

; Hinweis: list-sum und list-product sind seht ähnlich. Wir werden unten abstahieren.




; ---- Funktionen, die etwas produzieren ----

; Gerade Elemente einer Liste herausfiltern
(: evens (list-of-number -> list-of-number))

; Beispiel
(define list4 (cons 2 (cons 3 (cons 5 (cons 10 empty)))))

; Testfall
(check-expect (evens list4) (cons 2 (cons 10 empty)))

(define evens
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (evens (rest list)))
       (if (even? f)
           (cons f r)
           r)))))

; Eine zweite Funktion, die ähnlich ist wie evens --> Ziel: Danach die Abstaktion durchführen können.

; Positive Elemente einer Liste herausfiltern
(: positives (list-of-number -> list-of-number))

; Beispiel
(define list5 (cons 12 (cons -15 (cons -5 (cons 4 empty)))))

; Testfall
(check-expect (positives list5) (cons 12 (cons 4 empty)))

(define positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (positives (rest list)))
       (if (positive? f)
           (cons f r)
           r)))))

; ----- Liste von Tieren -----

; Signatur
(: animal-list1 (list-of animal))

; Beispiel
(define animal-list1
  (cons dillo1 (cons parrot1 (cons dillo2 (cons parrot2 empty)))))

; Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal)))

(check-expect (run-over-animals animal-list1)
              (cons (run-over-animal dillo1)
                    (cons (run-over-animal parrot1)
                          (cons (run-over-animal dillo2)
                                (cons (run-over-animal parrot2)
                                      empty)))))

(define run-over-animals
  (lambda (alist)
    (cond
      ((empty? alist) empty)
      ((cons? alist)
       (cons
        (run-over-animal (first alist))
        (run-over-animals (rest alist)))))))

; ----- Abstraktion für die Funktionen positives und evens -----
; Neue Schreibweise für die Liste:
;(list 1 2 3 4) ; Mit der Bibliothek "Schreibe Dein Programm!" wird die Struktur linear angezeigt.
;(cons 1 (cons 2 (cons 3 (cons 4 empty)))) ; Im Vergleich dazu die verschachtelte von "Schreibe Dein Programm! - Anfänger".

; Kurzbeschreib: Listenelemente herausfiltern, die ein Kriterium erfüllen
(: list-filter ((%a -> boolean) (list-of %a) -> (list-of %a)))

; Frage: wie wird die Signatur aufgebaut?
; Signatur Gerüst und Ausführungen für "number":
;(: list-filter (... ... -> ...)) Mithilfe dieses Gerüstes können wir entwickeln, welche Elemente in diese Signatur kommen.

; Schritt 1: Es geht eine Zahlenliste hinein und es kommt eine Zahlenliste heraus.
;(: list-filter (... (list-of number) -> (list-of number)))

; Schritt 2: Die erste Elipse (...) bezieht sich auf "p?", eine Funktion: Es geht etwas in diese Funktion hinein (...) und es kommt etwas aus dieser Funktion heraus (...), deshalb ergänzen wir:
;(: list-filter ((... -> ...) (list-of number) -> (list-of number)))

; Schritt 3:
; - Unten sehen wir, first "f" geht in die Funktion hinein "if (p? f)" --> f entspricht (first list), also eine "number".
; - Aus "if (p? f)" sehen wir, dass f mit "if" verarbeitet wird. Somit kommt ein "boolean" heraus. Das führt zu folgender Ergänzung der Signatur:
; (: list-filter ((number -> boolean) (list-of number) -> (list-of number)))

; Wir versuchen das nun zu verstehen:
; Was steht in dieser Signatur: "list-filter" nimmt Zahlen und sagt ja, kommt in die neue Liste oder nicht. Diese Zahlen nimmt es aus der Eingangsliste und gibt es in eine neue Liste.
; Also "(number -> boolean)" ist die Funktion, welche dem Kriterium (siehe Kurzbeschreib) entspricht, welche sagt, ob das jeweilige Element in die neue Ausgabeliste kommt.
; Das ist unsere erste Higher-Order-Funktion.
; Was ist eine Higher-Order-Funktion? Eine Higher-Order-Funktion ist eine Funktion, die selbst eine Funktion als Argument bekommt (Beispiel oben) oder eine Funktion als Ergebnis produziert.
; Der Ursprung der Higher-Order-Funktionen ist die funktionale Programmierung. Heute ist diese aber auch in den univeralen Sprachen wie Python und Java zu finden.

; Schritt 4:
; Signatur Abstahieren über "number":
; Wir führen dazu eine Signaturvariable ein "%a". In Racket wird für Signaturvariablen ein % vor den Variablennamen gemacht.
; (: list-filter ((%a -> boolean) (list-of %a) -> (list-of %a)))

; Wir versuchen zu verstehen:
; Wir haben eine Filter-Funktion, die für ein "a" sagt: Kommt in die neue Liste oder nicht. Im weiteren haben wir eine Eingangsliste mit "a"s und eine Ausgangsliste mit "a"s.
; Wir haben also eine Liste von "a"s und eine Funktion, die "a"s akzeptiert. Logischerweise wird dann eine neue Liste mit ausgewählten "a"s erstellt.


; Testfälle
; Zahlenbeispiel
(check-expect (positives (list 1 -15 -3 5 7)) ; alte Funktion
              (list-filter positive? (list 1 -15 -3 5 7))) ; Abstraktion, für "p?" muss "positives?" übergeben werden.

; Filter auf Tiere anwenden: Ist das Tier ein Papagei:
(check-expect (list-filter parrot? animal-list1)
              (list parrot1 parrot2))

; Anonyme Funktionen: Kleine Funktionen (Wegwerffunktionen), die es nicht Wert sind zu definieren und einen Namen dafür zu verschwenden. Hier: Alle Tiere ausser Papageien.
; Man beachte: Variablen stehen in der funktionalen Programmierung für Werte. Mit dem lambda-Ausdruck können direkt Werte erstellt werden und in das Programm geschrieben werden.
(check-expect (list-filter (lambda (animal) (not (parrot? animal))) animal-list1)
              (list dillo1 dillo2))

(define list-filter ; Sowohl evens? als auch positive? etwas filtern heraus. --> list-filter
  (lambda (p? list) ; p? ist noch nicht definiert, deshalb wird p? über lambda gebindet.
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (list-filter p? (rest list))) ; Der rekursive Aufruf braucht auch den Parameter p?
       (if (p? f) ; Das konkrete Prädikat wegmachen und p? als Ersatz-Prädikat hinschreiben.
           (cons f r)
           r)))))

;----- Aufgaben ab hier -----

; Schreiben Sie eine Funktion "number-of-five?", die für alle 5er Zahlen True zurückgibt. Machen Sie Testfälle der "list-filter" Funktion mit der übergebenen "number-of-five?" Funktion.
; Gehen Sie nach Konstruktionsanleitung vor.
(: number-of-five? (natural -> boolean))

(check-expect (number-of-five? 5) #t)
(check-expect (number-of-five? 0) #t)
(check-expect (number-of-five? 10) #t)
(check-expect (number-of-five? 35) #t)
(check-expect (number-of-five? 12) #f)
(check-expect (number-of-five? 1) #f)
(check-expect (number-of-five? 49) #f)
(check-expect (list-filter number-of-five? (list 1 2 5 0 10 6 45)) (list 5 0 10 45))

(define number-of-five?
  (lambda (n)
    (= (modulo n 5) 0)))


; Eigene Funktion

; Ist eine Zahl eine Primzahl?
(: prime? (natural -> boolean))

(check-expect (prime? 19) #t)
(check-expect (prime? 5) #t)
(check-expect (prime? 2) #t)
(check-expect (prime? 1) #f)
(check-expect (prime? 0) #f)
(check-expect (prime? 10) #f)
(check-expect (list-filter prime? (list 1 2 5 0 10 6 45 3 37)) (list 2 5 3 37))

(define prime?
  (lambda (n)
    (define prime?-helper
      (lambda (n divisor)
        (cond
          [(> (* divisor divisor) n) #t]
          [(zero? (modulo n divisor)) #f]
          [else (prime?-helper n (+ divisor 1))])))
    (cond
      [(<= n 1) #f]
      [else (prime?-helper n 2)])))
