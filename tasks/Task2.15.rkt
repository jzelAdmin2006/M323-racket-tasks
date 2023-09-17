;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.15) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Bussgeld für zu langes Parken
(: zu-langes-parken (number -> number))

(check-expect (zu-langes-parken 20) 5)
(check-expect (zu-langes-parken 40) 10)
(check-expect (zu-langes-parken 70) 15)
(check-expect (zu-langes-parken 150) 20)
(check-expect (zu-langes-parken 190) 25)

(define zu-langes-parken
  (lambda (min)
    (cond
      ((<= min 30) 5)
      ((<= min 60) 10)
      ((<= min 120) 15)
      ((<= min 180) 20)
      (else 25))))

; Bussgeld für das Überfahren einer roten Ampel
(: rote-ampel-bussgeld (number boolean -> number))

(check-expect (rote-ampel-bussgeld 0.5 #f) 50)
(check-expect (rote-ampel-bussgeld 0.5 #t) 125)
(check-expect (rote-ampel-bussgeld 1.5 #f) 125)
(check-expect (rote-ampel-bussgeld 1.5 #t) 200)

(define rote-ampel-bussgeld
  (lambda (sek gefahr?)
    (cond
      ((and (<= sek 1) (not gefahr?)) 50)
      ((and (> sek 1) gefahr?) 200)
      (else 125))))

; Punkte für das Überfahren einer roten Ampel
(: rote-ampel-punkte (number boolean -> number))

(check-expect (rote-ampel-punkte 0.5 #f) 3)
(check-expect (rote-ampel-punkte 0.5 #t) 4)
(check-expect (rote-ampel-punkte 1.5 #f) 4)
(check-expect (rote-ampel-punkte 1.5 #t) 4)

(define rote-ampel-punkte
  (lambda (sek gefahr?)
    (if (<= sek 1)
        (if gefahr? 4 3)
        4)))

; Fahrverbot für das Überfahren einer roten Ampel
(: rote-ampel-fahrverbot (number boolean -> boolean))

(check-expect (rote-ampel-fahrverbot 0.5 #f) #f)
(check-expect (rote-ampel-fahrverbot 0.5 #t) #t)
(check-expect (rote-ampel-fahrverbot 1.5 #f) #t)
(check-expect (rote-ampel-fahrverbot 1.5 #t) #t)

(define rote-ampel-fahrverbot
  (lambda (sek gefahr?)
    (or gefahr? (> sek 1))))
