;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
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

; Variation (z.B. beim Pferderennen die k schnellsten Pferde von gesamthaften n)
(: variation (natural natural -> natural))

; Testfälle für Variation
(check-expect (variation 10 3) 720)
(check-expect (variation 2 0) 1)
(check-expect (variation 2 2) 2)
(check-expect (variation 5 3) 60)

(define variation
  (lambda (n k)
    (/ (factorial n) (factorial (- n k)))))
