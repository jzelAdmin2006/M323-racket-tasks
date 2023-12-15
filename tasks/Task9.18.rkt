;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Task9.18) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Gerade Elemente einer Liste herausfiltern
(: evens ((list-of number) ->( list-of number)))

; Testfall
(check-expect (evens (list 2 3 5 10)) (list 2 10))

(define evens
  (lambda (list)
    (filter even? list)))


; Nullen in einer Liste von Zahlen zählen
(: count-zeroes ((list-of number) -> natural))

; Testfälle
(check-expect (count-zeroes (list 1 2 3 4)) 0)
(check-expect (count-zeroes (list 0 1 2 3 4)) 1)
(check-expect (count-zeroes (list 1 0 2 3 0 4)) 2)
(check-expect (count-zeroes (list 0 0 0 1 2 0 3 4 0)) 5)

(define count-zeroes
  (lambda (list)
    (length (filter zero? list))))


; Nach Vielfachen einer Zahl n filtern
(: multiples (number (list-of number) -> (list-of number)))

; Testfälle
(check-expect (multiples 2 (list 1 2 3 4 5 6 7 8 9 10)) (list 2 4 6 8 10))
(check-expect (multiples 3 (list 1 2 3 4 5 6 7 8 9 10)) (list 3 6 9))

(define multiples
  (lambda (n list)
    (filter (lambda (x) (zero? (modulo x n))) list)))
