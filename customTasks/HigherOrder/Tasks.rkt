;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Tasks) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
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

; Beispiel, das ähnlich ist wie das Tiere überfahren:

; Neue Schreibweise für die Liste:
;(list 1 2 3 4) ; Mit der Bibliothek "Schreibe Dein Programm!" wird die Struktur linear angezeigt.
;(cons 1 (cons 2 (cons 3 (cons 4 empty)))) ; Im Vergleich dazu die verschachtelte von "Schreibe Dein Programm! - Anfänger".

; In einer Liste von Zahlen alle inkrementieren
(: inc-list ((list-of number) -> (list-of number)))

(check-expect (inc-list (list 1 2 3 4 5)) (list 2 3 4 5 6))
(check-expect (inc-list (list 5 6 8 9 10)) (list 6 7 9 10 11))

#;(define inc-list
    (lambda (list)
      (cond
        ((empty? list) empty)
        ((cons? list)
         (cons
          (+ 1 (first list))
          (inc-list (rest list)))))))

; Diese Funktion sieht ähnlich aus wie run-over-animals mit Ausname der Zeile "(run-over-animal (first alist))" vs. "(+ 1 (first list))"
; Wir können somit noch nicht gut abstahieren, weil der Start der Zeile: "run-over-animal" einstelling ist, der Start "+ 1" aber zweistellig ist.
; Wir benutzen das Prinzip: Was nicht passt kann man passend machen.
; Wir schreiben eine Funktion "inc", welche zu n 1 addiert.
(define inc
  (lambda (n)
    (+ 1 n)))

; Somit ergibt sich die neue Funktion inc-list wie folgt:

(define inc-list
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (inc (first list))
        (inc-list (rest list)))))))

; ----- Abstraktion für die Funktioneninc-list und run-over-animal -----

; Wir betrachten für die Abstraktion zuerst die Funktion "inc-list":

; Kurzbeschreib: Funktion auf alle Elemente einer Liste anwenden.
(: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))

; Frage: wie wird die Signatur aufgebaut?
; Signatur Gerüst und Ausführungen für "number":
; (: list-map (... ... -> ...)) Mithilfe dieses Gerüstes können wir entwickeln, welche Elemente in diese Signatur kommen.

; Schritt 1: Es geht eine Zahlenliste hinein und es kommt eine Zahlenliste heraus.
;(: list-map (... (list-of number) -> (list-of number)))

; Schritt 2: Die erste Elipse (...) bezieht sich auf "f", eine Funktion: Es geht etwas in diese Funktion hinein (...) und es kommt etwas aus dieser Funktion heraus (...), deshalb ergänzen wir:
;(: list-map ((... -> ...) (list-of number) -> (list-of number)))

; Schritt 3:
; Unten sehen wir, "first" geht in diese Funktion "f" hinein -> "first" ist in unserem Beispiel eine "number" (z.B. 1)
; Aus der Funktion "f" kommt wieder eine "number" (z.B. 2)
;(: list-map ((number -> number) (list-of number) -> (list-of number)))

; Wir versuchen das nun zu verstehen:
; Was steht in dieser Signatur: "list-map" nimmt Zahlen und vergrössert diese um 1. Diese Zahlen nimmt es aus der Eingangsliste und gibt es in eine neue Liste.
; Also "(number -> number)" ist die Funktion, welche gemäss Vorschrift (hier + 1) etwas mit dem Element macht und dieses dann in die Ausgabeliste setzt.
; Das ist unsere erste Higher-Order-Funktion.
; Was ist eine Higher-Order-Funktion? Eine Higher-Order-Funktion ist eine Funktion,die selbst eine Funktion als Argument bekommt (Beispiel oben) oder eine Funktion als Ergebnis produziert.
; Der Ursprung der Higher-Order-Funktionen ist die funktionale Programmierung. Heute ist diese aber auch in den univeralen Sprachen wie Python und Java zu finden.

; Schritt 4:
; Signatur Abstahieren über "number":
; Wir führen dazu eine Signaturvariable ein "%a". In Racket wird für Signaturvariablen ein % vor den Variablennamen gemacht.
; (: list-map ((%a -> %a) (list-of %a) -> (list-of %a)))

; Wir versuchen zu verstehen:
; Wir haben eine map-Funktion, die für ein "a" gemäss Vorschrift etwas berechnet und danach dieses "a" in die Ausgabeliste setzt. Im weiteren haben wir eine Eingangsliste mit "a"s und eine Ausgangsliste mit "a"s.
; Wir haben also eine Liste von "a"s und eine Funktion, die "a"s akzeptiert. Logischerweise wird dann eine neue Liste mit ausgewählten "a"s erstellt.

; Schritt 5:
; Bei unserer list-map Funktion gehen die gleichen Sachen herein und heraus.
; Schauen wir aber den Kurzbeschreib an, so sehen wir, dass diese allgemeiner beschrieben ist.(Funktion auf alle Elemente einer Liste anwenden.)
; Sollte die Ergebnisliste z.B. nicht "a"s sondern "b"s sein, so sehen wir, dass die übergebene Funktion nicht "a"s sondern "b"s liefern.
; (: list-map ((%a -> %b) (list-of %a) -> (list-of %b)))
; Natürlich funktioniert die angepasste Signatur auch für gleiche Eingabe und Ausgabelisten.

; Testfälle
; Zahlenbeispiel
(check-expect (list-map inc (list 1 2 3 4 5 6)) (list 2 3 4 5 6 7))

; list-map auf die Tiere mithilfe der Funktion "run-over-animal" anwenden:
(check-expect (list-map run-over-animal animal-list1) (list (run-over-animal dillo1) (run-over-animal parrot1) (run-over-animal dillo2) (run-over-animal parrot2)))

(define list-map
  (lambda (f list) ; "f" muss über lambda gebunden werden.
    (cond
      ((empty? list) empty)
      ((cons? list)
       (cons
        (f (first list)) ; "inc" und "run-over-animal sind nicht sehr ähnlich, daher nennen wir den Ersatz nun "f" für Funktion.
        (list-map f (rest list))))))) ; "f" muss auch über den rekursiven Aufruf wieder als Parameter gesetzt werden.

;----- Aufgaben ab hier -----

; Schreiben Sie eine Funktion "multiply-by-5", welche die Eingabezahl mit 5 multipliziert. Machen Sie Testfälle der "list-map" Funktion mit der übergebenen "multiply-by-5" Funktion.
; Gehen Sie nach Konstruktionsanleitung vor.

(: list-filter ((%a -> boolean) (list-of %a) -> (list-of %a)))

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


(check-expect (list-map odd? (list 1 5 7 43 2 6 2)) (list #t #t #t #t #f #f #f))

(: between_20_and_50? (natural -> boolean))

(check-expect (between_20_and_50? 19) #f)
(check-expect (between_20_and_50? 20) #t)
(check-expect (between_20_and_50? 35) #t)
(check-expect (between_20_and_50? 50) #t)
(check-expect (between_20_and_50? 51) #f)
(check-expect (list-filter between_20_and_50? (list 19 20 35 50 51)) (list 20 35 50))

(define between_20_and_50?
  (lambda (n)
    (and (>= n 20) (<= n 50))))

(: list-map-filtered ((%a -> boolean) (%a -> %b) (list-of %a) -> (list-of %b)))

(check-expect (list-map-filtered even? inc (list 1 2 5 17 24 13)) (list 3 25))

(define list-map-filtered
  (lambda (filter map l)
    (list-map map (list-filter filter l))))

(: not-x? ((%a -> boolean) %a -> boolean))

(check-expect (not-x? positive? 5) #f)
(check-expect (not-x? parrot? dillo) #t)

(define not-x?
  (lambda (x a)
    (not (x a))))
