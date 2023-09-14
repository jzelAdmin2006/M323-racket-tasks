;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Images) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
;(define x(+ 12
;   (* 2 3)
;   ))
;(+ x 2)

(define c1 (circle 50 "solid" "red"))
(define c2 (circle 50 "solid" "blue"))

(beside c1 c2)
(overlay c1 c2)
(above c1 c2)