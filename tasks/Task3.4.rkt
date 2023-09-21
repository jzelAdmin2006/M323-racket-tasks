;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3.4) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Uhrzeit aus einer Zeitangabe im 12-Stunden-Format konstruieren

; Eine Angabe, ob eine Uhrzeit vor oder nach dem Mittag ist, ist eine der folgenden:
; - morgens
; - nachmittags
(define ampm
  (signature (one-of "AM" "PM")))

; Eine Uhrzeit besteht aus:
; - Stunde
; - Minute
(define-record wallclock-time
  make-wallclock-time
  (time-hour natural)
  (time-minute natural))

(: make-wallclock-time-12h (natural natural ampm -> wallclock-time))

(define t1 (make-wallclock-time 12 24))
(define t2 (make-wallclock-time 7 0))
(define t3 (make-wallclock-time 23 55))

(check-expect (make-wallclock-time-12h 0 24 "PM") t1)
(check-expect (make-wallclock-time-12h 7 0 "AM") t2)
(check-expect (make-wallclock-time-12h 11 55 "PM") t3)

(define make-wallclock-time-12h (lambda (h m amorpm)
                                  (if (string=? amorpm "AM")
                                      (make-wallclock-time h m)
                                      (make-wallclock-time (+ h 12) m))))
