;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task4.10_boolean) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; Steigung einer Gerade berechnen
(: slope (real real real real -> (mixed real boolean)))

; Hat eine Gerade keine Steigung?
(: no-slope? ((mixed real boolean) -> boolean))

(check-expect (slope 0 0 1 1) 1)
(check-expect (slope 0 0 2 1) 1/2)
(check-expect (slope 1 2 3 5) 3/2)
(check-expect (no-slope? (slope 1 2 3 5)) #f)
(check-expect (no-slope? (slope 0 0 0 1)) #t)

#;(define slope
    (lambda (x1 y1 x2 y2)
      (/ (- y2 y1)
         (- x2 x1))))

(define slope
  (lambda (x1 y1 x2 y2)
    (if (= (- x2 x1) 0)
        #f
        (/ (- y2 y1)
           (- x2 x1))))) 

(define no-slope? boolean?)
