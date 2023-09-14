;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.6) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Get absolute amount of number (turn positive if number is negative)
(: absolute-amount (number -> number))

(check-expect (absolute-amount -1) 1)
(check-expect (absolute-amount 1) 1)

(define absolute-amount (lambda (n)
                         (if (>= n 0) n (- 0 n))))
