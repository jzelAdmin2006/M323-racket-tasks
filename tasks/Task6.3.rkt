;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task6.3) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; ----- Listendefinition -----
; Eine Liste aus Zahlen ist eins der folgenden:
; - die leere Liste
; - ein Cons (Paare) besteht aus einer Zahl und einer Liste (Selbstbezug) der restlichen Elemente
(define list-of-number
  (signature (mixed empty-list cons-of-number)))

; Eine leere Liste besteht aus ... nichts.
(define-record empty-list
  make-empty 
  empty?) ; Grundsätzlich müsste es hier "empty-list?" heissen. Da aber dieses Prädikat öfters gebraucht wird kürzen wir es zu "empty?"

; Beispiel
(define empty (make-empty))

; Ein Cons besteht aus:
; - einer Zahl (erstes Element)
; - einer Liste der restlichen Elemente
(define-record cons-of-number
  cons ; Die Namen folgen in aller Regel einer bestimmten Konvention. Hier weichen wir davon ab, historische Begründung
  cons?
  (first number)
  (rest list-of-number)) ; Selbstbezug! -> "list-of-number"

; Beispiele
(define list0 empty) ; leere Liste
(define list1 (cons 17 empty)) ; Eine einelementige Liste: 17
(define list2 (cons 5 (cons 17 empty))) ; Eine 2elementige Liste: 5 17
(define list3 (cons 3 list2)) ; Eine 3elementige Liste: 3 5 17

; Geben Sie in der REPL list3 ein:
; #<record:cons-of-number 3 #<record:cons-of-number 5 #<record:cons-of-number 17 #<record:empty-list>>>>
; Die Ausgabe sieht linear aus, ist aber tatsächlich geschachtelt. Siehe Seite 168
; Wichtig: Asymetrische Struktur: Die Liste zerfällt in das erste Element und die Restlichen!!!
; In anderen Programmiersprachen wie Java gibt es z.B. ein Interface List<E>. Dieses ist auch zuständig
; für Folgen, schreibt aber keine spezielle Datenstruktur oder Repräsentation vor. In den funktionalen
; Sprachen sind Listen eine Datenstruktur wie oben dargestellt.




; ----- Funktionen auf Listen -----

; ---- Funktionen, welche von den Listen konsumieren ----
; Summe der Elemente einer Liste berechnen
(: list-sum (list-of-number -> number))

; Testfälle
(check-expect (list-sum list0) 0)
(check-expect (list-sum list1) 17)
(check-expect (list-sum list2) 22)
(check-expect (list-sum list3) 25)

; Gerüstbau: siehe Seite 169. Beachten Sie: (list-sum (rest list)) ... Der rest-Selektor hat einen Selbstbezug
; --> Sie können deshalb beim Gerüstbau bereits die Funktion "list-sum" angeben.

(define list-sum
  (lambda (list)
    (cond
      ((empty? list) 0); Die Summe einer leeren Liste ergibt 0 (neutrales Element bezüglich der Addition); Aufgabe 6.3: Testfälle failen alle, da es sich um eine rekursive Funktion handelt und es schlussendlich immer eine leere Liste gibt.
      ((cons? list)
       (+ (first list)
          (list-sum (rest list))) ; Summe der restlichen Elemente
       ))))

; Wie wird aufgrund der Funktionsanleitung addiert?
; list3: (+ 3 (+ 5 (+ 17 0))) "Mit dem 0 passiert nach 17 nichts schlimmes" --> neutrales Element

; Produkt der Elemente einer Liste
(: list-product (list-of-number -> number))

; Testfälle
(check-expect (list-product list0) 1)
(check-expect (list-product list1) 17)
(check-expect (list-product list2) 85)
(check-expect (list-product list3) 255)

; Gerüstbau siehe Seite 170
(define list-product
  (lambda (list)
    (cond
      ((empty? list) 1) ; neutrales Element für die Multiplikation
      ((cons? list)
       (* (first list)
          (list-product (rest list))))))) ; Produkt der restlichen Elemente

; Hinweis: list-sum und list-product sind seht ähnlich. Wir werden das später abstahieren.




; ---- Funktionen, die etwas produzieren ----

; Gerade Elemente einer Liste herausfiltern
(: evens (list-of-number -> list-of-number))

; Beispiel
(define list4 (cons 2 (cons 3 (cons 5 (cons 10 empty))))) ; In dieser Liste befinden sich gerade und ungerade Zahlen

; Testfall
(check-expect (evens list4) (cons 2 (cons 10 empty)))

; Gerüstbau: Beachten Sie: (evens (rest list)) ... Der rest-Selektor hat einen Selbstbezug
; --> Sie können deshalb beim Gerüstbau bereits die Funktion "evens" angeben.
#;(define evens
  (lambda (list)
    (cond
      ((empty? list) ...)
      ((cons? list)
       ...
       (first list)
       (evens (rest list))))))

; Wir benutzen die eingebaute Funktion even?
; Wichtig: Hier wird nur das Element first geprüft. Denken Sie an die asymmetrische Struktur.
; Durch den Selbstbezug wird jedes der restlichen Elemente einmal zum first!
#;(define evens
  (lambda (list)
    (cond
      ((empty? list) empty) ; leere Liste: wenn keine Zahlen hereingehen, können auch keine Zahlen herausgehen! --> empty
      ((cons? list)
       (if (even? (first list)) ; Falls dieses Element gerade ist, landet es in der Ausgabeliste. Test mit even?
           (cons (first list) (evens (rest list))) ; Konsequente: Das gerade Element aus first und die restlichen geraden Elemente
           (evens (rest list))))))) ; Alternative: gerade Zahlen unter den restlichen Zahlen

; Jeder Programmierer wird sich im Code oben über die Verdoppelung stören.
; In dieser Variante werden die doppelt vorkommenden Ausdrücke in eine Variable gelegt und mithilfe der Variable verwendet.
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

; Eine zweite Funktion, die ähnlich ist wie evens --> Ziel: Danach (Higer-Order-Programmierung) die Abstaktion durchführen können.

; Positive Elemente einer Liste herausfiltern
(: positives (list-of-number -> list-of-number))

; Beispiel
(define list5 (cons 12 (cons -15 (cons -5 (cons 4 empty))))) ; In dieser Liste befinden sich positive und negative Zahlen

; Testfall
(check-expect (positives list5) (cons 12 (cons 4 empty)))

; Wir benutzen die eingebaute Funktion positive?
#;(define positives
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (if (positive? (first list))
          (cons (first list) (positives (rest list)))
          (positives (rest list)))))))

; Refaktoring positives
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


; Ungerade Elemente einer Liste herausfiltern
(: odds (list-of-number -> list-of-number))

; Testfall
(check-expect (odds list4) (cons 3 (cons 5 empty)))

(define odds
  (lambda (list)
    (cond
      ((empty? list) empty)
      ((cons? list)
       (define f (first list))
       (define r (odds (rest list)))
       (if (odd? f)
           (cons f r)
           r)))))
