;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task1.13) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(: time->seconds (number number number number -> number))

(define time->seconds (lambda (h min s ds) (+ (* 3600 h) (+ (* 60 min) (+ s (/ ds 10))))))

(check-expect (time->seconds 14 54 38 3) 53678.3)
(check-expect (time->seconds 10 45 25 7) 38725.7)


(: seconds->beats (number -> number))

(define seconds->beats (lambda (s) (/ (round (* (/ s 86.4) 100)) 100)))

(check-expect (seconds->beats 10929.6) 126.5)


(: time->beats (number number number number -> number))

(define time->beats (lambda (h min s ds) (seconds->beats (time->seconds h min s ds))))

(check-expect (time->beats 6 4 19 2) 253)
