;; Die ersten drei Zeilen dieser Datei wurden von DrRacket eingefügt. Sie enthalten Metadaten
;; über die Sprachebene dieser Datei in einer Form, die DrRacket verarbeiten kann.
#reader(lib "vanilla-reader.rkt" "deinprogramm" "sdp")((modname Task9.25) (read-case-sensitive #f) (teachpacks ((lib "image.rkt" "teachpack" "deinprogramm" "sdp"))) (deinprogramm-settings #(#f write repeating-decimal #f #t none explicit #f ((lib "image.rkt" "teachpack" "deinprogramm" "sdp")))))
(define-record no-result
  make-no-result
  no-result?)

(define not-found (make-no-result))
(define empty-phonebook (λ (s) not-found))

(define phonebook-result
  (signature (mixed no-result string)))
(define phonebook
  (signature (string -> phonebook-result)))

(: add-to-phonebook (phonebook string string -> phonebook))
(: lookup-in-phonebook (phonebook string -> phonebook-result))

(check-expect (lookup-in-phonebook empty-phonebook "Hans") not-found)
(check-expect (lookup-in-phonebook (add-to-phonebook empty-phonebook "Hans" "754829") "Hans") "754829")
(check-expect (lookup-in-phonebook (add-to-phonebook empty-phonebook "Hans" "754829") "Lea") not-found)

(define add-to-phonebook (λ (pb name number)
                           (λ (search-name)
                             (if (string=? search-name name)
                                 number
                                 (pb search-name)))))

(define lookup-in-phonebook (λ (pb name)
                              (pb name)))
