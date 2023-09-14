;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "beginner-reader.rkt" "deinprogramm" "sdp")((modname Task1.12) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(: double-string (string -> string))
(: quadruple-string (string -> string))
(: octuple-string (string -> string))
(: sixteentuple-string (string -> string))

(define double-string
  (lambda (s)
   (string-append s s)))

(define quadruple-string
  (lambda (s)
   (string-append (double-string s) (double-string s))))

(define octuple-string
  (lambda (s)
   (string-append (quadruple-string s) (quadruple-string s))))

(define sixteentuple-string
  (lambda (s)
   (string-append (octuple-string s) (octuple-string s))))

(check-expect (double-string "Sperber") "SperberSperber")
(check-expect (quadruple-string "Sperber") "SperberSperberSperberSperber")
(check-expect (octuple-string "Sperber") "SperberSperberSperberSperberSperberSperberSperberSperber")
(check-expect (sixteentuple-string "Sperber") "SperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperberSperber")
