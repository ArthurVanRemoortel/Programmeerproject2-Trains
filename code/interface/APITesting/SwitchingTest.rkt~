#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt" "./MessageHandler.rkt" "../FullAPI/Z21MessageSwitches.rkt" "../FullAPI/racket-bits-utils-master/bits.rkt")

;Setup
(define z (setup))

(listen z handle-msg)

(define (loop i)
  (when (not(= i 255))
    (begin
      (display i) (newline)
      (send z (make-set-switch-msg (byte->hex-string i) "01" true 2))
      (sleep 1)
      (send z (make-set-switch-msg (byte->hex-string i) "01" true 1))
      (sleep 1)
      (loop (+ i 1)))))

;Messages

(define get-switch-info     (make-get-switch-info-msg "00" "00")) ;We assume that a switch with address 0 is connected

(define set-switch-message2 (make-set-switch-msg "00" "00" true 2))

(define set-switch-message1 (make-set-switch-msg "00" "00" true 1))

;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

;(send z get-switch-info) ;Expect the Z21 to answer with the switch info message

;(sleep 1)

;(send z set-switch-message2) ;Expect switch 0 to physically change position (if nothing happens the switch was already in position 2)

;(sleep 1)

;(send z set-switch-message1) ;No matter what the switch's original position was, it should now physically have changed positions

(define (set-switch lsb pos)
  (send z (make-set-switch-msg (byte->hex-string lsb) "00" true pos)))