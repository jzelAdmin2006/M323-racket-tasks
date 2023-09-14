;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname BranchingWithCutePets) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Ein Haustier ist eins der folgenden:
; - Hund
; - Katze
; - Schlange
(define pet
  (signature (one-of "dog" "cat" "snake")))

; Ist ein Tier niedlich?
(: cute? (pet -> boolean))

(check-expect (cute? "dog") #t)
(check-expect (cute? "cat") #t)
(check-expect (cute? "snake") #f)

(define cute?
  (lambda (p)
    (cond
      ((string=? p "cat") #t)
      ((string=? p "dog") #t)
      ((string=? p "snake") #f))))


; Ein Tier gegen ein niedliches umtauschen
(: trade-for-cute (pet -> pet))

(check-expect (trade-for-cute "dog") "dog")
(check-expect (trade-for-cute "cat") "cat")
(check-expect (trade-for-cute "snake") "cat")

(define trade-for-cute
  (lambda (p)
    (cond
      ((cute? p) p)
      (else "cat"))))
