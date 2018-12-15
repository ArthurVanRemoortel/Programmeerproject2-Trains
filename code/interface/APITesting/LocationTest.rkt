#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt" "./MessageHandler.rkt" "../FullAPI/Z21MessageLocation.rkt")
;setup
(define z (setup))

(listen z handle-msg)

;Messages

(define get-location-message (make-rmbus-get-data-msg "00"))


;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

(send z get-location-message) ;Expect Z21 to answer with location data for group index 0 of the modules (location data depends on physical setup and current state)