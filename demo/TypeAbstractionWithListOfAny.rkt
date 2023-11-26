;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname TypeAbstractionWithListOfAny) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
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

; Eine leere Liste besteht aus ... nichts.
(define-record empty-list
  make-empty 
  empty?)

; Beispiel
(define empty (make-empty))

; Ein Cons besteht aus:
; - einer Zahl (erstes Element)
; - einer Liste der restlichen Elemente
#;(define-record cons-of-number
    cons
    cons?
    (first number) ; Das ist die Stelle, in der festgehalten wird, dass die Listenelemente nur Zahlen sind.
    (rest list-of-number)) 

; Ein Cons besteht aus:
; - einem a (erstes Element)
; - einer Liste der restlichen Elemente
(define-record (cons-of a) ; Klammer setzen, für (cons-of a) können wir eine beliebige Signatur einsetzen. (Generischer Typ in Python oder Java) -> Ursprung dieses features ist die funktionale Programmierung
  cons
  cons?
  (first a) ; An dieser zentralen Stelle muss nun a stehen und nicht mehr number
  (rest (list-of a))) ; Der Selbstbezug muss auf die oben geschriebene list-of Definition hinweisen.

; Beispiele
(define list0 empty) ; leere Liste
(define list1 (cons 17 empty)) ; Eine einelementige Liste: 17 (+ empty)
(define list2 (cons 5 (cons 17 empty))) ; Eine 2elementige Liste: 5 17
(define list3 (cons 3 list2)) ; Eine 3elementige Liste: 3 5 17

; Die list-of-number können wir mit einer Definition anbinden. List-of (Funktion-Signatur): Hier number
(define list-of-number (list-of number))

; ----- Funktionen auf Listen -----

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

; Hinweis: list-sum und list-product sind seht ähnlich. Wir werden das später abstahieren.




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
(: animal-list1 (list-of animal)) ; Hier wird wieder der generische Ansatz verwendet, welche die anderen Sprachen wie Java von der funktionalen Programmierung übernommen haben!

; Hier sehen wir, dass wir nicht nur auf Zahlen beschränkt sind!
(define animal-list1
  (cons dillo1 (cons parrot1 (cons dillo2 (cons parrot2 empty)))))

; Tiere überfahren
(: run-over-animals ((list-of animal) -> (list-of animal))) ; Für die Listenkonstruktoren brauchen wir Klammern, somit gibt es doppelte Klammern

(check-expect (run-over-animals animal-list1)
              (cons (run-over-animal dillo1) ; Ergebnisliste der überfahrenen Tiere
                    (cons (run-over-animal parrot1)
                          (cons (run-over-animal dillo2)
                                (cons (run-over-animal parrot2)
                                      empty)))))

; Schablobe
#;(define run-over-animals
    (lambda (alist)
      (cond
        ((empty? alist) ...)
        ((cons? alist)
         ...
         (first alist)
         (run-over-animals (rest alist)) ; Hier steht wieder der rekursive Aufruf!
         ...))))

(define run-over-animals
  (lambda (alist)
    (cond
      ((empty? alist) empty) ; Wenn keine Tiere mehr da sind kommt eine leere Liste heraus!
      ((cons? alist)
       (cons ; Wir müssen eine neue Liste produzieren. -> cons
        (run-over-animal (first alist)) ; first wird überfahren
        (run-over-animals (rest alist))))))) ; Selbstbezug

;(run-over-animals animal-list1) ; Darstellung der Liste der überfahrenen Tiere in der REPL
;dillo1 ; Die Erinnerung an das lebende Gürteltier ist immer noch vorhanden (dillo1 existiert noch). Wir verändern somit keine Datenstruktur.

;----- Hier ist Platz für die Aufgaben 1 - 3 -----










; ----- Nun können Sie den Sprach-Level wechseln! -----
; Jetzt können wir auf den Level "Schreibe Dein Programm" wechseln. Dieser besitzt bereits Listen.
; Kommentieren Sie folgende Funktionen aus:
; - list-of
; - empty-list
; - (cons-of a)
; - (define list-of-number (list-of number)) Hier muss nun (define list-of-number (signature (list-of number))) stehen
; Grund für signature: list-of ist jetzt kein Eigenbau mehr, deshalb der Verweis mit signature.
