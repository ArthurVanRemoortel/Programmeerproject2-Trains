#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt"  "../FullAPI/Z21MessageDecoderCV.rkt" "./MessageHandler.rkt")

;Setup
(define z (setup))

(listen z handle-msg)

;Messages

(define cv-read-message (make-cv-read-msg "00" "00")) ;Read out cv 0 of the loc residing on prog track cv0 should be loc address

(define cv-write-message (make-cv-write-msg "3f" "00" "50")) ;On tested loc cv 3f (i.e. 63b) is sound volume, adapt for other locs if needed

(define cv-pom-write-message (make-cv-pom-write-byte-msg "03" "00" "3f" "00" "c0")) ;We assume there's a loc with address 3 on track + previous assumption concerning sound CV

(define cv-pom-write-bit-message (make-cv-pom-write-bit-msg "03" "00" "3f" "00" 0 7)) ;Previous assumptions still hold

(define cv-pom-read-message (make-cv-pom-read-msg  "03" "00" "3f" "00"))
 

;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

;(send z cv-read-message) ;Expect Z21 answering with cv result message (or cv-nack and cv-nack-sc if something went wrong), answer depends on the loc's address currently on the prog track

;(sleep 3)

;(send z cv-write-message) ;Expect Z21 answering with cv result message (or cv-nack and cv-nack-sc if something went wrong), answer should be written value = 63

;(sleep 3)

(send z cv-pom-write-message) ;Expect Z21 answering with cv result message (or cv-nack and cv-nack-sc if something went wrong), answer should be written value = 192

(sleep 3)

(send z cv-pom-write-bit-message) ;Expect Z21 answering with cv result message (or cv-nack and cv-nack-sc if something went wrong), answer should be written value = 64

(sleep 3)

(send z cv-pom-read-message) ;Expect Z21 answering with cv result message (or cv-nack and cv-nack-sc if something went wrong), answer should be written value = 64

