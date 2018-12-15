#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide make-get-loco-mode-msg is-get-loco-mode-answer? get-loco-mode-address is-dcc-loco-mode? is-mm-loco-mode? make-set-loc-msg dcc-mode mm-mode make-get-turnout-mode-msg is-get-turnout-mode-answer? get-turnout-mode-address is-dcc-turnout-mode? is-mm-turnout-mode? make-set-turnout-msg)

;Implementation of the settings part of the protocol (based on document version 1.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get loco mode MSG (3.1) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the output format (i.e. DCC or MM) for a given loc-address (lsb and msb seperate) which will be transmitted in big-endian format 

(define (make-get-loco-mode-msg add-lsb add-msb)
  (let
      ([len-lsb  "06"]
       [len-msb  "00"]
       [head-lsb "60"]
       [head-msb "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb add-msb add-lsb)))

;z21 return message containing the 8-bit modus code 0x0 = DCC, 0x1 = MM

;Checks whether a message is a loco mode answer. The loc for which it answers the get loco mode needs to be extracted with function bellow
(define (is-get-loco-mode-answer? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "60"]
       [head-msb "00"]
       [data0    template-wildcard]
       [data1    template-wildcard]
       [modus    template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb data0 data1 modus)])
    (msg-eq? msg template)))

(define (get-loco-mode-address msg)
  (let
      ([add-msb (get-msg-data-byte msg 0)]
       [add-lsb (get-msg-data-byte msg 1)])
    (read-word add-lsb add-msb)))

(define (is-dcc-loco-mode? msg)
  (let
      ([mode (get-msg-data-byte msg 2)])
    (= mode dcc-mode)))

(define (is-mm-loco-mode? msg)
  (let
      ([mode (get-msg-data-byte msg 2)])
    (= mode mm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set loco mode MSG (3.2) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Set the loco mode for the given loc-address. All loc addresses >= 256 are automatically converted to the dcc format. Mode 0 = DCC, 1 = MM

(define dcc-mode 0)
(define mm-mode  1)

(define (make-set-loc-msg add-lsb add-msb mode)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "61"]
       [head-msb "00"]
       [data0    add-msb]
       [data1    add-lsb]
       [data2    mode])
    (make-hex-msg len-lsb len-msb head-lsb head-msb data0 data1 data2)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get turnout mode MSG (3.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Returns the output format (i.e. DCC or MM) for a given turnout-address (lsb and msb seperate) which will be transmitted in big-endian format 

(define (make-get-turnout-mode-msg add-lsb add-msb)
  (let
      ([len-lsb  "06"]
       [len-msb  "00"]
       [head-lsb "60"]
       [head-msb "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb add-msb add-lsb)))

;z21 return message containing the 8-bit modus code 0x0 = DCC, 0x1 = MM

;Check if a return message is the answer for the specified turnout-address
(define (is-get-turnout-mode-answer? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "60"]
       [head-msb "00"]
       [data0    template-wildcard]
       [data1    template-wildcard]
       [modus    template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb data0 data1 modus)])
    (msg-eq? msg template)))

(define (get-turnout-mode-address msg)
  (let
      ([add-msb (get-msg-data-byte msg 0)]
       [add-lsb (get-msg-data-byte msg 1)])
    (read-word add-lsb add-msb)))

(define (is-dcc-turnout-mode? msg)
  (let
      ([mode (get-msg-data-byte msg 2)])
    (= mode dcc-mode)))

(define (is-mm-turnout-mode? msg)
  (let
      ([mode (get-msg-data-byte msg 2)])
    (= mode mm-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set turnout mode MSG (3.4) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Set the turnout mode for the given turnout-address. All turnout addresses >= 256 are automatically converted to the dcc format. Mode 0 = DCC, 1 = MM

(define (make-set-turnout-msg add-lsb add-msb mode)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "61"]
       [head-msb "00"]
       [data0    add-msb]
       [data1    add-lsb]
       [data2    mode])
    (make-hex-msg len-lsb len-msb head-lsb head-msb data0 data1 data2)))
