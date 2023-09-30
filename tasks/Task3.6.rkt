;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3.6) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; zusammengesetzte Daten:
; besteht aus / hat folgende Eigenschaften

; Eine Uhrzeit besteht aus:
; - Stunde
; - Minute
(define-record time
  make-time ; Konstruktor
  (time-hour natural)
  (time-minute natural))

(define t1 (make-time 23 59))
(define t2 (make-time 0 0))
(define t3 (make-time 23 55))
(define t4 (make-time 22 55))

(time-hour t1)
(time-minute t3)

; Signaturen, die automatisch erstellt werden (müsste man normalerweise nicht machen)
(: make-time (natural natural -> time))
(: time-hour (time -> natural))
(: time-minute (time -> natural))


; Minuten seit Mitternacht berechnen
(: minutes-since-midnight (time -> natural))

(check-expect (minutes-since-midnight t1) (+ (* 23 60) 59))
(check-expect (minutes-since-midnight t2) 0)
(check-expect (minutes-since-midnight t3) (+ (* 23 60) 55))

(define minutes-since-midnight
  (lambda (t)
    (+ (* (time-hour t) 60) (time-minute t))
    ))

; Es gibt höchstens 1439 Minuten seit Mitternacht
(define msm (signature (integer-from-to 0 1439)))

; aus den Minuten seit Mitternacht die Uhrzeit berechnen
(: msm->time (msm -> time))

(check-expect (msm->time (+ (* 23 60) 59)) t1)
(check-expect (msm->time 0) t2)
(check-expect (msm->time (+ (* 23 60) 55)) t3)

(define msm->time
  (lambda (m)
    (make-time (quotient m 60) (remainder m 60))
    ))

; Uhrzeit eine gegebene Anzahl Minuten addieren
(: time-add-minutes (time natural -> time))

(check-expect (time-add-minutes t3 3) (make-time 23 58))
(check-expect (time-add-minutes t2 1237) (make-time 20 37))
(check-expect (time-add-minutes t4 10) (make-time 23 05))

(define time-add-minutes
  (lambda (t m)
    (msm->time (+ (minutes-since-midnight t) m))))
