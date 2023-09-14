;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task1.14) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
(: liters-per-hundred-kilometers (number number -> number))

(define liters-per-hundred-kilometers (lambda (l r) (* (/ l r) 100)))

(check-expect (liters-per-hundred-kilometers 1 100) 1)


(: miles-per-gallon (number number -> number))

(define miles-per-gallon (lambda (m g) (/ m g)))

(check-expect (miles-per-gallon 100 5) 20)


(: kilometers-per-mile number)
(define kilometers-per-mile 1.61)

(: kilometers->miles (number -> number))
(define kilometers->miles (lambda (km) (/ km kilometers-per-mile)))
(check-expect (kilometers->miles 1.61) 1)

(: miles->kilometers (number -> number))
(define miles->kilometers (lambda (m) (* m kilometers-per-mile)))
(check-expect (miles->kilometers 1) 1.61)


(: liters-per-gallon number)
(define liters-per-gallon 3.79)

(:  liters->gallons (number -> number))
(define  liters->gallons (lambda (l) (/ l liters-per-gallon)))
(check-expect ( liters->gallons 3.79) 1)

(: gallons->liters (number -> number))
(define gallons->liters (lambda (g) (* g liters-per-gallon)))
(check-expect (gallons->liters 1) 3.79)


(: reciprocal (number -> number))
(define reciprocal (lambda (n) (/ 1 n)))
(check-expect (reciprocal 2) 0.5)

(: l/100km->mi/gal (number -> number))
(define l/100km->mi/gal (lambda (ckm) (kilometers->miles (* (reciprocal (liters->gallons ckm)) 100))))
(check-within (l/100km->mi/gal 5) 46.9 47.1)
(check-within (l/100km->mi/gal 8) 29.3 29.5)
(check-within (l/100km->mi/gal 12) 19.5 19.7)


(: mi/gal->l/100km (number -> number))
(define mi/gal->l/100km (lambda (migal) (* (gallons->liters (reciprocal (miles->kilometers migal))) 100)))
(check-within (mi/gal->l/100km 20) 11.7 11.9)
(check-within (mi/gal->l/100km 30) 7.7 7.9)
(check-within (mi/gal->l/100km 50) 4.6 4.8)
