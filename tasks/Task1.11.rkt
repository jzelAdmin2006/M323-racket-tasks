;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Aufgabe1.11) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(: celsius->fahrenheit (number -> number))

(define celsius->fahrenheit (lambda (c) (+ 32 (* c (/ 9 5)))))

(check-expect (celsius->fahrenheit 0) 32)
(check-expect (celsius->fahrenheit 50) 122)


(: fahrenheit->celsius (number -> number))

(define fahrenheit->celsius (lambda (f) (/ (- f 32) (/ 9 5))))

(check-expect (fahrenheit->celsius 32) 0)
(check-expect (fahrenheit->celsius 122) 50)
