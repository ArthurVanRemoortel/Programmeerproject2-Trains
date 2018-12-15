#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt" "../FullAPI/Z21MessageSysStat.rkt" "../FullAPI/Z21MessageSettings.rkt" "./MessageHandler.rkt")

;Setup
(define z (setup))

(listen z handle-msg)

;Messages

(define get-loco-mode-message (make-get-loco-mode-msg "03" "00")) ;Tests assumes that there is a loc on the track with address 3

;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

(send z get-loco-mode-message) ;Expect Z21 to answer with loc mode + loc address (3 default)

(sleep 1)