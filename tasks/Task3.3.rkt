;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3.3) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Eine Uhrzeit besteht aus:
; - Stunde
; - Minute
(define-record time
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))

; Bestimmen, ob es sich bei einer Uhrzeit um eine vor oder nach dem Mittag handelt

(: time->ampm (time -> string))

(define t1 (make-time 12 24))
(define t2 (make-time 7 0))
(define t3 (make-time 23 55))

(check-expect (time->ampm t1) "pm")
(check-expect (time->ampm t2) "am")
(check-expect (time->ampm t3) "pm")

(define time->ampm (
                    lambda (t) (if (< (time-hour t) 12)
                                   "am"
                                   "pm")))
