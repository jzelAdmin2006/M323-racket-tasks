;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task2.11) (read-case-sensitive #f) (teachpacks ()) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ())))
; 1 bis 8 «Flensburg»-Punkte
(define flensburg (signature (integer-from-to 0 8)))

; Eine Saktion kann eine der folgenden sein:
; - Keine Sanktionen
; - Vormerkung
; - Ermahnung
; - Verwarnung
; - Entzug
(define sanction
  (signature (one-of "no sanctions" "caution" "admonition" "warning" "withdrawal")))

; Saktionen gemäss Punkten herausgeben
(: flensburg->sanction (flensburg -> sanction))

(check-expect (flensburg->sanction 0) "no sanctions")
(check-expect (flensburg->sanction 1) "caution")
(check-expect (flensburg->sanction 3) "caution")
(check-expect (flensburg->sanction 4) "admonition")
(check-expect (flensburg->sanction 5) "admonition")
(check-expect (flensburg->sanction 6) "warning")
(check-expect (flensburg->sanction 7) "warning")
(check-expect (flensburg->sanction 8) "withdrawal")


(define flensburg->sanction
  (lambda (f)
    (cond
      ((= f 0) "no sanctions")
      ((and (>= f 1) (<= f 3)) "caution")
      ((or (= f 4) (= f 5)) "admonition")
      ((or (= f 6) (= f 7)) "warning")
      (else "withdrawal"))))
