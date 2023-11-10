;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task4) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Fakultät (Anzahl Möglichkeiten, n Objekte anzuordnen bzw. grundsätzlich Produkt von allen Zahlen von 1 bis n)
(: factorial (natural -> natural))

; Testfälle für Fakultät
(check-expect (factorial 0) 1)
(check-expect (factorial 1) 1)
(check-expect (factorial 2) 2)
(check-expect (factorial 3) 6)
(check-expect (factorial 4) 24)
(check-expect (factorial 100) 93326215443944152681699238856266700490715968264381621468592963895217599993229915608941463976156518286253697920827223758251185210916864000000000000000000000000)

(define factorial
  (lambda (n)
    (if (= n 0)
     1
     (* n (factorial (- n 1)))
     )))

; Kombination (z.B. beim Lotto die k richtigen Zahlen von gesamthaften n)
(: combination (natural natural -> natural))

; Testfälle für Kombination
(check-expect (combination 42 6) 5245786)
(check-expect (combination 10 3) 120)
(check-expect (combination 2 0) 1)
(check-expect (combination 2 2) 1)
(check-expect (combination 5 3) 10)

(define combination
  (lambda (n k)
    (/ (factorial n) (* (factorial (- n k)) (factorial k)))))

; Rekursive Implementierung der Kombination (sehr ineffizient, ist aber eine gute Übung)
(: combination-recursive (natural natural -> natural))

; Testfälle für Kombination
(check-expect (combination-recursive 42 6) 5245786)
(check-expect (combination-recursive 10 3) 120)
(check-expect (combination-recursive 2 0) 1)
(check-expect (combination-recursive 2 2) 1)
(check-expect (combination-recursive 5 3) 10)

(define combination-recursive
  (lambda (n k)
    (if (or (= k 0) (= n k))
        1
        (+ (combination-recursive (- n 1) (- k 1)) (combination-recursive (- n 1) k )))))
