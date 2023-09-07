;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task1.15) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(: diff-in-hours (number number number number number number -> number))

(define diff-in-hours (lambda (h1 m1 s1 h2 m2 s2)
  (+ (- h2 h1)
     (/ (- m2 m1) 60.0) 
     (/ (- s2 s1) 3600.0))))

(check-within (diff-in-hours 9 15 30 10 16 31) 0.9 1.1)


(: money-earned (number number number number number number -> number))

(define money-earned 
  (lambda (hb mb sb he me se) 
    (* 7200 (diff-in-hours hb mb sb he me se))))

(check-expect (money-earned 9 15 30 17 15 30) 57600)
