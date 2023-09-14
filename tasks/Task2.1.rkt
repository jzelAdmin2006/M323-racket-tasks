;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.1) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(: xor (boolean boolean -> boolean))

(define xor (lambda (a b)
 (and (or a b) (not (and a b)))))

(check-expect (xor #t #t) #f)
(check-expect (xor #t #f) #t)
(check-expect (xor #f #t) #t)
(check-expect (xor #f #f) #f)
