;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.13) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Kleinere Zahl zurückgeben
(: min-2 (number number -> number))

(check-expect (min-2 4 8) 4)
(check-expect (min-2 2 0) 0)

(define min-2
  (lambda (a b)
    (if
     (< a b)
     a
     b
     )))

; Kleinste Zahl von 3 zurückgeben
(: min-3 (number number number -> number))

(check-expect (min-3 234 6247 16) 16)
(check-expect (min-3 1234 13713 1346) 1234)
(check-expect (min-3 2315 6 16) 6)

(define min-3
  (lambda (a b c)
    (min-2 (min-2 a b) c)))


; Grössere Zahl zurückgeben
(: max-2 (number number -> number))

(check-expect (max-2 4 8) 8)
(check-expect (max-2 2 0) 2)

(define max-2
  (lambda (a b)
    (if
     (> a b)
     a
     b
     )))

; Grösste Zahl von 3 zurückgeben
(: max-3 (number number number -> number))

(check-expect (max-3 234 6247 16) 6247)
(check-expect (max-3 1234 13713 1346) 13713)
(check-expect (max-3 2315 6 16) 2315)

(define max-3
  (lambda (a b c)
    (max-2 (max-2 a b) c)))


; Ist die erste von drei gegebenen Zahlen das Minimum?
(: min-3? (number number number -> boolean))

(check-expect (min-3? 234 6247 16) #f)
(check-expect (min-3? 1234 13713 1346) #t)
(check-expect (min-3? 2315 6 16) #f)

(define min-3?
  (lambda (a b c)
    (if
     (= a (min-3 a b c))
     #t
     #f
     )))

; Ist die erste von drei gegebenen Zahlen zwischen den beiden anderen liegt (oder gleich eine der anderen beiden ist)?
(: valid-value? (number number number -> boolean))

(check-expect (valid-value? 234 6247 16) #t)
(check-expect (valid-value? 1234 13713 1346) #f)
(check-expect (valid-value? 2315 6 16) #f)
(check-expect (valid-value? 2315 2315 16) #t)
(check-expect (valid-value? 1234 13713 1234) #t)

(define valid-value?
  (lambda (a b c)
    (if
     (and (>= a (min-2 b c)) (<= a (max-2 b c)))
     #t
     #f
     )))

; Die clamp Funktion begrenzt x auf den Bereich [min, max], indem sie entweder x, min oder max zurückgibt, je nachdem wo x liegt.
(: clamp (number number number -> number))

(check-expect (clamp 5 3 7) 5) 
(check-expect (clamp 1 3 7) 3) 
(check-expect (clamp 9 3 7) 7) 
(check-expect (clamp 3 3 7) 3) 
(check-expect (clamp 7 3 7) 7) 
(check-expect (clamp 7 7 7) 7) 
(check-expect (clamp 4 5 4) 5) 

(define clamp
  (lambda (x min max)
    (max-2 min (min-2 x max))))
