;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname FunctionFactories) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Funktionsfabriken

; 1. Komposition (f°g)(x) = f(g(x))

; zwei Prozeduren komponieren
(: compose ((%b -> %c) (%a -> %b) -> (%a -> %c)))

(define compose
  (lambda (f g )
    (lambda (x)
      (f (g x)))))

; Beispiele
(define add-5
  (lambda (x)
    (+ x 5)))

(define add-23
  (lambda (x)
    (+ x 23)))

; Testfälle
(check-expect ((compose add-5 add-23) 3) (add-5 (add-23 3)))
(check-expect ((compose (lambda (x) (* x 2)) (lambda (x) (+ x 3))) 5) (* (+ 5 3) 2))

; 2. Repeat
(: repeat ( number (%a -> %a) -> (%a -> %a)))

(define repeat
  (lambda (n f) ; n = Anzahl, f = Funktion
    (if (= n 0)
        (lambda (x) x)
        (compose f (repeat (- n 1) f)))))

; Testfälle
(check-expect ((repeat 5 (lambda (x) (* x 2))) 1) (* 2 2 2 2 2))
(check-expect ((repeat 0 (lambda (x) (* x 2))) 1) 1)
(check-expect ((repeat 5 (lambda (x) (* x x))) 2) 4294967296)
(check-expect ((repeat 5 (lambda (x) (string-append "Test" x))) "Test") "TestTestTestTestTestTest")

; 3. Transformationen Zwischenschritt
; Jetzt schauen wir Transformationen von einer Funktion mit 2 Parameter in eine Funktion mit nur einem Parameter,
; die eine Funktion mit einem weiteren Parameter zurückgibt, die dann schliesslich den Wert liefert lässt, an.

; Funktion erzeugen, die eine Konstante addiert
(: make-add0 (number -> (number -> number)))

(define make-add0
  (lambda (a)
    (lambda (b)
      (+ a b))))

; Beispiele:
(define add-1 (make-add0 1))
(define add-7 (make-add0 7))

; Testfälle
(check-expect (add-1 15) 16)
(check-expect (add-1 13) 14)
(check-expect (add-7 15) 22)
(check-expect (add-7 13) 20)
; make-add ist eine Funktion, die zwei Argumente nicht auf einmal akzeptiert, sondern nacheinander
(check-expect ((make-add0 3) 10) 13)

; Funktion erzeugen, die mit einer Konstante multipliziert
(: make-mult0 (number -> (number -> number)))

(define make-mult0
  (lambda (a)
    (lambda (b)
      (* a b))))

; Testfälle
(check-expect ((make-mult0 3) 5) 15)
(check-expect ((make-mult0 7) 6) 42)

; Funktion erzeugen, die an eine Liste ein Element vorn anhängt
(: make-prepend0 (%a -> ((list-of %a) -> (list-of %a))))

(define make-prepend0
  (lambda (a)
    (lambda (b)
      (cons a b))))

; Testfälle
(check-expect ((make-prepend0 5) (list 2 3 1)) (list 5 2 3 1))
(check-expect ((make-prepend0 "Hallo") (list "BBZW" "Sursee")) (list "Hallo" "BBZW" "Sursee"))

; 4. curryfizieren (currify) (Transformationen)
; Diese Funktion wurde nach Haskell Curry benannt.

; Funktion mit zwei Parameter staffeln
(: curry ((%a %b -> %c) -> (%a -> (%b -> %c))))

(define curry
  (lambda (f)
    (lambda (a)
      (lambda (b)
        (f a b)))))

; Beispiele von oben
(define make-add (curry +))
(define make-mult (curry *))
(define make-prepend (curry cons))

;Testfälle
(check-expect ((make-add 3) 10) 13)
(check-expect ((make-add 4) 11) 15)
(check-expect ((make-mult 3) 5) 15)
(check-expect ((make-mult 7) 6) 42)
(check-expect ((make-prepend 5) (list 2 3 1)) (list 5 2 3 1))
(check-expect ((make-prepend "Hallo") (list "BBZW" "Sursee")) (list "Hallo" "BBZW" "Sursee"))


; 5. uncurryfizieren (uncurrify) (Transformationen)

; Funktion, zu einer Funktion mit zwei Parametern entstaffeln
(: uncurry ((%a -> (%b -> %c)) -> (%a %b -> %c)))

(define uncurry
  (lambda (f)
    (lambda (a b)
      ((f a) b))))

; Beispiele
(define make-add2 (uncurry make-add))
(define make-mult2 (uncurry make-mult))
(define make-prepend2 (uncurry make-prepend))

; Testfälle
(check-expect (make-add2 3 10) 13)
(check-expect (make-add2 4 11) 15)
(check-expect (make-mult2 3 5) 15)
(check-expect (make-mult2 7 6) 42)
(check-expect (make-prepend2 5 (list 2 3 1)) (list 5 2 3 1))
(check-expect (make-prepend2 "Hallo" (list "BBZW" "Sursee")) (list "Hallo" "BBZW" "Sursee"))

; Somit ist die Transformation ein Isomorphismus:

(define add
  (lambda (a b)
    (+ a b)))

(check-expect (add 2 3) 5)

(define add-c (curry add))

(check-expect ((add-c 2) 3) 5)

(define add-uc (uncurry add-c))

(check-expect (add-uc 2 3) 5)