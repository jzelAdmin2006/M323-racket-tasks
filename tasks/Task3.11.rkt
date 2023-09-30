;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3.11) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Eine Wochentagangabe ist eine der folgenden:
; - Montag
; - Dienstag
; - Mittwoch
; - Donnerstag
; - Freitag
; - Samstag
; - Sonntag
(define weekday-designation
  (signature (one-of "monday" "tuesday" "wednesday" "thursday" "friday" "saturday" "sunday")))

(define day-hour
  (signature (integer-from-to 0 23)))

; Eine Reservation besteht aus:
; - Name des Raums
; - Wochentag
; - Uhrzeit (Stunde)
; - Name des Dozenten
(define-record reservation
  make-reservation
  (room-name string)
  (day-of-week weekday-designation)
  (time-hour day-hour)
  (lecturer-name string))

(define empty-reservation (make-reservation "Raum 1.1" "thursday" 15 ""))
(define example-reservation (make-reservation "Raum 2.3" "tuesday" 10 "Bucher"))

; reservieren, falls noch nicht reserviert wurde
(: reserve (reservation string -> reservation))

(check-expect (reserve empty-reservation "Lagger") (make-reservation "Raum 1.1" "thursday" 15 "Lagger"))
(check-expect (reserve example-reservation "Lagger") example-reservation)

(define reserve
  (lambda (r l)
    (if (string=? (lecturer-name r) "")
        (make-reservation (room-name r) (day-of-week r) (time-hour r) l)
        r)))
