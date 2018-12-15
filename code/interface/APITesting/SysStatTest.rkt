#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt" "../FullAPI/Z21MessageSysStat.rkt" "./MessageHandler.rkt")


;Setup
(define z (setup))

(listen z handle-msg)

;Messages
(define serial-message (make-serial-msg))

(define x-bus-message (make-x-bus-msg))

(define get-status-message (make-get-status-msg))

(define set-track-power-off-message (make-set-track-power-off-msg))

(define set-track-power-on-message (make-set-track-power-on-msg))

(define wrong-message (bytes 4 0 4 0))

(define stop-message (make-stop-msg))

(define firmware-message (make-firmware-msg))

(define broadcast-message (make-broadcast-msg (list general-broadcast-flag))) ;Position broadcast flag will be tested in the positions test

(define get-broadcast-flags-message (make-get-broadcast-msg))

(define logoff-message (make-logoff-msg))

(define get-system-state-message (make-get-system-state-msg))

;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

(send z serial-message) ;Expect Z21 answering with serial number

(sleep 1)

(send z x-bus-message) ;Expect Z21 answering with x-bus answer (x-bus version + x-bus central)

(sleep 1)

(send z get-status-message) ;Expect Z21 answering with status changed message

(sleep 1)

(send z set-track-power-off-message) ;Expect Z21 answering with track power off message + physically turning track power off (blinking leds on Z21)

(sleep 1)

(send z set-track-power-on-message) ;Expect Z21 answering with track power on message + physically turning track power on (end of blinking leds on Z21)

(sleep 1)

(send z wrong-message) ;Expect Z21 answering with unknown command message

(sleep 1)

(send z stop-message) ;Expect Z21 answering with stopped message + physically stopped all trains (blinking leds on Z21) (power is still on )

(sleep 1)

(send z firmware-message) ;Expect Z21 answering with firmware number

(sleep 1)

(send z broadcast-message) ;Expect Z21 sending messages as soon as changes are made to the system (todo)

(sleep 1)

(send z get-broadcast-flags-message)

(sleep 1)

(send z get-system-state-message) ;Expect Z21 answering with a full report on system status

(sleep 1)

(send z logoff-message) ;No answer from z21 or physical result, allows to use repl with clean slate



