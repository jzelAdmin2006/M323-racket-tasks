;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task3.9) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Ein Bruch besteht aus:
; - Zähler
; - Nenner
(define-record fraction
  make-fraction
  (fraction-numerator integer)
  (fraction-denominator integer))

(define f1 (make-fraction 3 6))
(define f2 (make-fraction 14 18))
(define f3 (make-fraction 3 4))
(define f4 (make-fraction 4 8))


; GGT von Zähler und Nenner eines Bruches bekommen
(: gcd-fraction (fraction -> integer))

(check-expect (gcd-fraction f1) 3)
(check-expect (gcd-fraction f2) 2)
(check-expect (gcd-fraction f3) 1)

(define gcd-fraction
  (lambda (f)
    (gcd (fraction-numerator f) (fraction-denominator f))
    ))


; Vorzeichen wechseln
(: switch-presign (integer -> integer))

(check-expect (switch-presign -1) 1)
(check-expect (switch-presign 2) -2)
(check-expect (switch-presign 0) 0)

(define switch-presign
  (lambda (n)
    (- 0 n)
    ))


; Unnötige negative Vorzeichen bei Bruch entfernen
(: clean-fraction-presign (fraction -> fraction))

(check-expect (clean-fraction-presign f1) f1)
(check-expect (clean-fraction-presign (make-fraction -1 -2)) (make-fraction 1 2))

(define clean-fraction-presign
  (lambda (f)
    (if (< (fraction-denominator f) 0) (make-fraction (switch-presign (fraction-numerator f)) (switch-presign (fraction-denominator f))) f)
    ))


; Bruch kürzen
(: truncate (fraction -> fraction))

(check-expect (truncate (make-fraction -1 -1)) (make-fraction 1 1))
(check-expect (truncate f1) (make-fraction 1 2))
(check-expect (truncate f2) (make-fraction 7 9))
(check-expect (truncate f3) f3)

(define truncate
  (lambda (f)
    (clean-fraction-presign (make-fraction (/ (fraction-numerator f) (gcd-fraction f)) (/ (fraction-denominator f) (gcd-fraction f))))
    ))


; Durch Bruch repräsentierte rationale Zahl bekommen
(: fraction->rational (fraction -> rational))

(check-expect (fraction->rational f1) 0.5)
(check-within (fraction->rational f2) 0.77 0.78)
(check-expect (fraction->rational f3) 0.75)
(check-expect (fraction->rational f4) 0.5)

(define fraction->rational
  (lambda (f)
    (/ (fraction-numerator f) (fraction-denominator f))
    ))


; Test auf Gleichheit der durch zwei Brüche repräsentierten rationalen Zahlen
(: fraction=? (fraction fraction -> boolean))

(check-expect (fraction=? f1 f1) #t)
(check-expect (fraction=? f1 f4) #t)
(check-expect (fraction=? f1 f2) #f)

(define fraction=?
  (lambda (f1 f2)
    (= (fraction->rational f1) (fraction->rational f2))
    ))


; Multiplikation von Brüchen
(: fraction* (fraction fraction -> fraction))

(check-expect (fraction* f1 f1) (make-fraction 1 4))
(check-expect (fraction* f2 f3) (make-fraction 7 12))

(define fraction*
  (lambda (f1 f2)
    (truncate (make-fraction (* (fraction-numerator f1) (fraction-numerator f2)) (* (fraction-denominator f1) (fraction-denominator f2))))
    ))


; Division von Brüchen
(: fraction/ (fraction fraction -> fraction))

(check-expect (fraction/ f1 f1) (make-fraction 1 1))
(check-expect (fraction/ f2 f3) (make-fraction 28 27))

(define fraction/
  (lambda (f1 f2)
    (fraction* f1 (make-fraction (fraction-denominator f2) (fraction-numerator f2)))
    ))


; Produkt von Zähler des ersten Bruchs und Nenner des zweiten Bruchs
(: numerator1-denominator2* (fraction fraction -> integer))

(check-expect (numerator1-denominator2* f1 f1) 18)
(check-expect (numerator1-denominator2* f2 f3) 56)
(check-expect (numerator1-denominator2* f3 f2) 54)

(define numerator1-denominator2*
  (lambda (f1 f2)
    (* (fraction-numerator f1) (fraction-denominator f2))
    ))


; Addition von Brüchen
(: fraction+ (fraction fraction -> fraction))

(check-expect (fraction+ f1 f1) (make-fraction 1 1))
(check-expect (fraction+ f2 f3) (make-fraction 55 36))

(define fraction+
  (lambda (f1 f2)
    (truncate (make-fraction (+ (numerator1-denominator2* f1 f2) (numerator1-denominator2* f2 f1)) (* (fraction-denominator f1) (fraction-denominator f2))))
    ))


; Subtraktion von Brüchen
(: fraction- (fraction fraction -> fraction))

(check-expect (fraction-numerator (fraction- f1 f4)) 0)
(check-expect (= (fraction-denominator (fraction- f1 f4)) 0) #f)
(check-expect (fraction- f2 f3) (make-fraction 1 36))

(define fraction-
  (lambda (f1 f2)
    (fraction+ f1 (make-fraction (switch-presign (fraction-numerator f2)) (fraction-denominator f2)))
    ))
