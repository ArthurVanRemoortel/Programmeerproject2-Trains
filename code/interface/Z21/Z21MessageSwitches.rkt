#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide make-get-switch-info-msg make-set-switch-msg is-switch-info-msg? get-switch-info-address switch-position1 switch-position2  switch-no-position  switch-illegal-position get-switch-info-position)

;Implementation of the switch part of the protocol (based on document version 1.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get switch info MSG (5.1) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Request information for a certain switch (indentified by the supplied switch address) to z21
;z21 answers with switch info message (see 5.3)

(define (make-get-switch-info-msg add-lsb add-msb)
  (let
      ([len-lsb  "08"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "43"]
       [db0      add-msb]
       [db1      add-lsb]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set switch info MSG (5.2) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Request to change a switch's position
;z21 does not answer synchronously. The change will be notified through the switch info message if the client is subscribed to those messages.
;Positions depend on a switch layout, therefore positions cannot be specified absolutely (e.g. set switch right)
;The following relative positions are provided: out1, out2. Which can be activated or deactivated.

;Create a position (for the z21), active is a boolean indicating whether the position should be activated, position is either 1 or 2
(define (create-position active position)
  (let
      ([active-bit    (if active 1 0)])
    (cond
      ((= position 1) (quasiquote (1 0 0 0 (unquote active-bit) 0 0 0))) ;last-bit indicates position (position 1 = 0)
      ((= position 2) (quasiquote (1 0 0 0 (unquote active-bit) 0 0 1))) ;last-bit indicates position (position 2 = 1)
      (else (raise (string-append "Unknown switch position: " (number->string position)))))))

;active is a boolean indicating whether the position should be activated, position is either 1 or 2
(define (make-set-switch-msg add-lsb add-msb active position)
  (let
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "53"]
       [db0      add-msb]
       [db1      add-lsb]
       [db2      (byte->hex-string (bit-list->byte (create-position active position)))]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;
;Switch info MSG (5.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;Message sent by the z21 to the client containing switch information.
;This message is sent to the client if the client sends the get-switch-info message (see 5.1) or if
;the state of the switch has been changed by another client, manually, or if the client enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-switch-info-msg? msg)
  (let*
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "43"]
       [db0      template-wildcard]
       [db1      template-wildcard]
       [db2      template-wildcard]
       [xor      template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)])
    (msg-eq? msg template)))

(define (get-switch-info-address msg)
  (let
      ([add-msb (get-msg-data-byte msg 1)]
       [add-lsb (get-msg-data-byte msg 2)])
    (read-word add-lsb add-msb)))

(define switch-position1         1)
(define switch-position2         2)
(define switch-no-position       0)
(define switch-illegal-position -1)


;Last two bits of the position byte indicate the switch position. 00 = not switched, 01 = position 1, 10 = position 2, 11 = illegal position
;This function retrieves the position from a switch info message and returns one of the above defined positions
(define (get-switch-info-position msg)
  (let*
      ([position-bits (byte->bit-list (get-msg-data-byte msg 3))]
       [data-bits     (list-tail position-bits 6)])
    (cond
      ((equal? data-bits '(0 0)) switch-no-position)
      ((equal? data-bits '(0 1)) switch-position1)
      ((equal? data-bits '(1 0)) switch-position2)
      ((equal? data-bits '(1 1)) switch-illegal-position)
      (else (raise "Error occured during position decoding")))))

