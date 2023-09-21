;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname RecordsWithTime) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; zusammengesetzte Daten:
; besteht aus / hat folgende Eigenschaften

; Eine Uhrzeit besteht aus:
; - Stunde
; - Minute
(define-record time
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))

(define t1 (make-time 12 24))
(define t2 (make-time 7 0))
(define t3 (make-time 23 55))

(time-hour t1)
(time-minute t3)

; Signaturen, die automatisch erstellt werden (müsste man normalerweise nicht machen)
(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))


; Minuten seit Mitternacht berechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight t1) (+ (* 12 60) 24))
(check-expect (minutes-since-midnight t2) (* 7 60))
(check-expect (minutes-since-midnight t3) (+ (* 23 60) 55))

(define minutes-since-midnight
  (lambda (t)
    (+ (* (time-hour t) 60) (time-minute t))
    ))

; aus den Minuten seit Mitternacht die Uhrzeit berechnen
(: msm->time (natural -> time))

(check-expect (msm->time (+ (* 12 60) 24)) t1)
(check-expect (msm->time (* 7 60)) t2)
(check-expect (msm->time (+ (* 23 60) 55)) t3)

(define msm->time
  (lambda (msm)
    (make-time (quotient msm 60) (remainder msm 60))
    ))
