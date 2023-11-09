;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname SelfReferenceWithRivers) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
; Ein Fluss kommt entweder aus:
; - einer Quelle
; - einem Hauptfluss und einem Nebenfluss

; Ein Fluss ist eins der Folgenden:
; - ein Bach aus einer Quelle
; - ein Zusammentreffen aus einem Hauptfluss und einem Nebenfluss
(define river
  (signature (mixed creek confluence)))

; Ein Bach hat folgende Eigenschaften:
; - Ursprungsort
(define-record creek
  make-creek
  creek?
  (creek-origin string))

; Ein Zusammentreffen hat folgende Eigenschaften:
; - Ort
; - Hauptfluss
; - Nebenfluss
(define-record confluence
  make-confluence
  confluence?
  (confluence-origin string)
  (confluence-main-stem river)
  (confluence-tributary river))

; Beispiele
(define eschach (make-creek "Heimliswald")) ; Neckarquelle
(define prim (make-creek "Dreifaltigkeitsberg")) ; Quelle
(define neckar-1 (make-confluence "Rottweil" eschach prim))
(define schlichem (make-creek "Tieringen"))
(define neckar-2 (make-confluence "Epfendorf" neckar-1 schlichem))

; Fliesst der Fluss durch einen bestimmten Ort?
(: flows-through? (river string -> boolean))

; Testfälle
(check-expect (flows-through? eschach "Heimliswald") #t)
(check-expect (flows-through? eschach "Zürich") #f)
(check-expect (flows-through? neckar-2 "Heimliswald") #t)
(check-expect (flows-through? neckar-2 "Rottweil") #t)
(check-expect (flows-through? neckar-2 "Luzern") #f)

(define flows-through?
  (lambda (river town)
    (cond
      ((creek? river)
       (string=? (creek-origin river) town))
       
       ((confluence? river)
        (or
         (string=? (confluence-origin river) town) ; Ist der Ort des Zusammenflusses identisch mit dem Ort den wir suchen?
         (flows-through? (confluence-main-stem river) town) ; Fliesst der Hauptfluss durch den Ort?
         (flows-through? (confluence-tributary river) town) ; Fliesst der Nebenfluss durch den ort?
         )
        ))))