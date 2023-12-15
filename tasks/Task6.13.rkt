;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Aufgabe_6.13) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Signatur id (identifikationsnummer)
(define id
  (signature natural))

; Eine Stempelkarte besteht aus
; - Mitarbeiternummer
; - Stundenliste
(define-record timecard
  make-timecard
  timecard?
  (timecard-id id)
  (timecard-times (list-of rational)))

; Eine Personalakte besteht aus
; - Name
; - Nummer
; - Stundenlohn
(define-record file
  make-file
  file?
  (file-name string)
  (file-id id)
  (file-rate rational))

; Eine Überweisung besteht aus
; - Mitarbeiternummer
; - Betrag in Euro
(define-record transfer
  make-transfer
  transfer?
  (transfer-id id)
  (transfer-amount rational))

; Mitarbeiter für Tests:
(define hans (make-file "Hans" 0 10))
(define otto (make-file "Otto" 1 7.5))
(define gertrud (make-file "Gertrud" 2 15))
(define karl (make-file "Karl" 3 20))
(define workers (list hans otto gertrud karl))
        
                
; Karten:
(define hans-card (make-timecard 0 (list 6.5 7 4.5)))
(define otto-card  (make-timecard 1 (list 7 4 5.25)))
(define gertrud-card (make-timecard 2 (list 2 2)))
(define karl-card (make-timecard 3 (list 8 7.75 4 10)))
(define cards (list hans-card otto-card gertrud-card karl-card))
              

; Personalakte eines Mitarbeiter holen
(: lookup-file ((list file) id -> file))

; Testfälle
(check-expect (lookup-file workers 3) karl)
(check-expect (lookup-file workers 2) gertrud)
  
(define lookup-file
  (lambda (files id)
    (cond
      ((empty? files) (violation "Personalakte nicht vorhanden!"))
      ((cons? files)
       (if (= id (file-id (first files)))
           (first files)
           (lookup-file (rest files) id))))))

; summiere eine Liste
(: sum-list ((list rational) -> rational))

(define sum-list
  (lambda (list)
    (cond
      ((empty? list) 0)
      ((cons? list)
       (+ (first list)
          (sum-list (rest list)))))))

; Berechne die Stundensumme für eine Stempelkarte
(: compute-hours (timecard -> rational))

; Testfälle
(check-within (compute-hours hans-card) 18 0.001)
(check-within (compute-hours otto-card) 16.25 0.001)
(check-expect (compute-hours gertrud-card) 4)
(check-within (compute-hours karl-card) 29.75 0.001)

(define compute-hours
  (lambda (t)
    (sum-list (timecard-times t))))

; Berechne die Lohnsumme für einen einzelnen Mitarbeiter und seine Stempelkarte
(: compute-pay (file timecard -> rational))

; Testfälle
(check-within (compute-pay hans hans-card) 180 0.001)
(check-within (compute-pay otto otto-card) 121.875 0.001)
(check-expect (compute-pay gertrud gertrud-card) 60)
(check-within (compute-pay karl karl-card) 595  0.001)

(define compute-pay
  (lambda (f t)
    (* (file-rate f)
       (compute-hours t))))

; Berechnet fällige Überweisungen
(: compute-transfers ((list-of timecard) (list-of file) -> (list-of transfer)))

; Testfall
(check-within (compute-transfers cards workers) 
              (list (make-transfer 0 180)
                    (make-transfer 1 121.875)
                    (make-transfer 2 60)
                    (make-transfer 3 595.0))
              0.001)

(define compute-transfers
  (lambda (cards files)
    (cond
      ((empty? cards) empty)
      ((cons? cards)
       (cons
        (make-transfer
         (timecard-id (first cards))
         (compute-pay (lookup-file files (timecard-id (first cards))) (first cards)))
        (compute-transfers (rest cards) files))))))
