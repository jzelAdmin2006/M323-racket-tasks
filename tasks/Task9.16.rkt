;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Task9.16) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
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

; ----- Fold -----

; Liste einfalten
(: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))

; Grundsätzlicher Aufbau der Signatur (Eine Funktion mit 3 Eingaben):
; (: list-fold (... ... ... -> ...))

; Die dritte Eingabe ist eine Liste:
; (: list-fold (... ... (list-of ...) -> ...))

; for-cons ist eine zweistellige Funktion:
; (: list-fold (... (... ... -> ...) (list-of ...) -> ...))

; Die Listenelemente können beliebig sein:
; (: list-fold (... (... ... -> ...) (list-of %a) -> ...))

; for-cons nimmt Elemente der Liste entgegen:
; (: list-fold (... (%a ... -> ...) (list-of %a) -> ...))

; Der Gesamtergebnistyp ist noch offen:
; (: list-fold (... (%a ... -> ...) (list-of %a) -> %b))

; Beim rekursiven Aufruf sehen wir, dass das Gesamtergebnis %b in die zweite Stelle der Funktion for-cons hineingeht:
; (: list-fold (... (%a %b -> ...) (list-of %a) -> %b))

; Das Ergebnis von for-cons muss das gleiche sein wie das Gesamtergebnis:
; (: list-fold (... (%a %b -> %b) (list-of %a) -> %b))

; Die leere Liste muss der gleiche Datentyp sein, wie das Gesamtergebnis:
; (: list-fold (%b (%a %b -> %b) (list-of %a) -> %b))
; In der als Parameter übergebenen Funktion wird ein Listenelement mit dem Gesamtergebnis verrechnet und damit zum neuen Gesamtergebnis.

(check-expect (list-fold 0 + (list 1 2 3 4 5)) 15)
(check-expect (list-fold 1 * (list 1 2 3 4 5)) 120)

(define list-fold
  (lambda (for-empty for-cons list) ; Die neuen Variablen müssen den Parameter hinzugefügt werden!
    (cond
      ((empty? list) for-empty) ; Die 0 oder 1 sind im empty-Zweig, daher for-empty!
      ((cons? list)
       (for-cons (first list) ; Die * oder + sind im cons-Zweig, daher for-cons!
                 (list-fold for-empty for-cons (rest list))))))) ; Ebenso gehören Sie in den rekursiven Aufruf.


; list-fold entspricht der Schablone:
; Für eine Liste braucht es ein "cond", "(empty? list)", "(cons? list)", "first list)" und der rekursive Aufruf.
; Was passiert mit der leeren Liste? Das ist von Funktion zu Funktion unterschiedlich. --> for-empty
; Die zweite Sache, die unterschiedlich ist was mit dem ersten Element gemacht wird, sowie wie mithilfe des rekursiven Aufrufs der restlichen Elemente das Gesamterggebnis gebildet wird. --> for-cons
; list-fold ist die Manifestation des Patterns für Listenfunktionen.
; Probieren Sie folgendes aus: (list-fold empty cons (list 1 2 3 4 5 6)) --> Identität (Also list.fold entspricht den Rückrad der Liste)

(: list-map2 ((%a -> %b) (list-of %a) -> (list-of %b)))

(check-expect (list-map2 even? (list 1 2 3 4)) (list #f #t #f #t))

(define list-map2
  (lambda (f list)
    (list-fold empty
               (lambda (first rest)
                 (cons
                  (f first)
                  rest))
               list)))

(: list-filter2 ((%a -> boolean) (list-of %a) -> (list-of %a)))

(check-expect (list-filter2 even? (list 1 2 3 4)) (list 2 4))

(define list-filter2
  (lambda (f list)
    (list-fold empty
               (lambda (first rest)
                 (if (f first)
                     (cons first rest)
                     rest))
               list)))

(: list-length2 ((list-of any) -> natural))

(check-expect (list-length2 (list 1 2 3 4)) 4)

(define list-length2
  (lambda (list)
    (list-fold 0
               (lambda (first rest)
                 (+ 1 rest))
               list)))

(: list-or ((%a -> boolean) (list-of %a) -> boolean))

(check-expect (list-or even? (list 2 3 1)) #t)
(check-expect (list-or even? (list 1 3)) #f)

(define list-or
  (lambda (f list)
    (list-fold #f
               (lambda (first rest)
                 (or (f first) rest))
               list)))

(: count-trues ((%a -> boolean) (list-of %a) -> natural))

(check-expect (count-trues even? (list 1 2 3 4)) 2)

(define count-trues
  (lambda (f list)
    (list-fold 0
               (lambda (first rest)
                 (if (f first)
                     (+ 1 rest)
                     rest))
               list)))

(: contains? (%a (list-of %a) -> boolean))

(check-expect (contains? 3 (list 1 2 3 4)) #t)
(check-expect (contains? 5 (list 1 2 3 4)) #f)
(check-expect (contains? "a" (list "b" "a")) #t)
(check-expect (contains? "a" (list "b" "c")) #f)

(define contains?
  (lambda (a list)
    (list-fold #f
               (lambda (first rest)
                 (or (equal? a first) rest))
               list)))

(: remove-duplicates ((list-of %a) -> (list-of %a)))

(check-expect (remove-duplicates (list 1 2 3 2 4 5 2 5)) (list 1 3 4 2 5))

(define remove-duplicates
  (lambda (list)
    (list-fold empty
               (lambda (first rest)
                 (if (contains? first rest)
                     rest
                     (cons first rest)))
               list)))


(: any? ((%a -> boolean) (list-of %a) -> boolean))

(check-expect (any? even? (list 1 2 3)) #t)
(check-expect (any? even? (list 1 5 3)) #f)

(define any?
  (lambda (f list)
    (list-fold #f
               (lambda (first rest)
                 (or (f first) rest))
               list)))
