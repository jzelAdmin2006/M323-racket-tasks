;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.14) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Kleinere Zahl zurückgeben
(: min-2 (number number -> number))

(check-expect (min-2 4 8) 4)
(check-expect (min-2 2 0) 0)

(define min-2
  (lambda (a b)
    (if
     (< a b)
     a
     b
     )))

; Grössere Zahl zurückgeben
(: max-2 (number number -> number))

(check-expect (max-2 4 8) 8)
(check-expect (max-2 2 0) 2)

(define max-2
  (lambda (a b)
    (if
     (> a b)
     a
     b
     )))

; Ist die erste von drei gegebenen Zahlen zwischen den beiden anderen (oder gleich eine der anderen beiden)?
(: between? (number number number -> boolean))

(check-expect (between? 234 6247 16) #t)
(check-expect (between? 1234 13713 1346) #f)
(check-expect (between? 2315 6 16) #f)
(check-expect (between? 2315 2315 16) #t)
(check-expect (between? 1234 13713 1234) #t)

(define between?
  (lambda (a b c)
     (and (>= a (min-2 b c)) (<= a (max-2 b c)))))


; Eine Position kann einer der folgenden sein:
; - Torwärtin
; - Abwehrspielerin
; - Mittelfeldspielerin
; - Stürmerin
; - Ersatzspielerin
; - ungültig
(define position
  (signature (one-of "goalkeeper" "defender" "midfielder" "striker" "substitute" "invalid")))


; Position gemäss Rückennummer zurückgeben
(: nummer->position (natural -> position))


(check-expect (nummer->position 1) "goalkeeper")

(check-expect (nummer->position 2) "defender")
(check-expect (nummer->position 4) "defender")
(check-expect (nummer->position 5) "defender")

(check-expect (nummer->position 6) "midfielder")
(check-expect (nummer->position 8) "midfielder")
(check-expect (nummer->position 10) "midfielder")

(check-expect (nummer->position 9) "striker")
(check-expect (nummer->position 11) "striker")

(check-expect (nummer->position 12) "substitute")
(check-expect (nummer->position 77) "substitute")
(check-expect (nummer->position 99) "substitute")

(check-expect (nummer->position 0) "invalid")
(check-expect (nummer->position 100) "invalid")


(define nummer->position
  (lambda (n)
    (cond
      ((= n 1) "goalkeeper")
      ((between? n 2 5) "defender")
      ((or (between? n 6 8) (= n 10)) "midfielder")
      ((or (= n 9) (= n 11)) "striker")
      ((between? n 12 99) "substitute")
      (else "invalid"))))
