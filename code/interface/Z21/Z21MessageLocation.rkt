#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide is-rmbus-datachanged-msg? get-rmbus-data make-rmbus-get-data-msg make-rmbus-programmodule-msg get-group-index get-occupancies get-module get-ports get-rmbus-data-group-index)

;Implementation of the r-bus (train position) part of the protocol (based on document version 1.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV rmbus datachanged MSG (7.1) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Message sent by the Z21 to the client containing track position information
;The message is sent if:
; - the client subscribed for the message using the appropriate broadcast flag (00000002) (see 2.16)
; - the client has explicitly asked for the information with the rmbus get-data message (see 7.2)
;A datachanged message bundles a group index which indicates the range of position-modules for which it contains information
; - group index = 0 (position-modules with addresses 1 to 10)
; - group index = 1 (position-modules with addresses 11 to 20)
;The datachanged message also contains position-module states (10 bytes) which allocates a byte per position-module. Each bit of one such byte indicates occupancy for a given port
;e.g. group index 1 with position-module states: 0x01 0x00 0xC5 0x00 0x00 0x00 0x00 0x00 0x00 0x00 means that position-module 11 has an occupancy on port 1 and position-module 13 has occupancy
;on ports 8, 7, 3 and 1

(define (is-rmbus-datachanged-msg? msg)
  (let*
      ([len-lsb  "0f"]
       [len-msb  "00"]
       [head-lsb "80"]
       [head-msb "00"]
       [g-index  template-wildcard]
       [stat1    template-wildcard]
       [stat2    template-wildcard]
       [stat3    template-wildcard]
       [stat4    template-wildcard]
       [stat5    template-wildcard]
       [stat6    template-wildcard]
       [stat7    template-wildcard]
       [stat8    template-wildcard]
       [stat9    template-wildcard]
       [stat10   template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb g-index stat1 stat2 stat3 stat4 stat5 stat6 stat7 stat8 stat9 stat10)])
    (msg-eq? msg template)))

(define get-group-index car)
(define get-occupancies cdr)
(define get-module      car)
(define get-ports       cdr)

;create a list for which the car() is the position-module identifier and the cdr() is a list of indices which represents the occupied ports
(define (create-poslist index occ-byte)
  (let
      ([occ-bits    (byte->bit-list occ-byte)]
       [occupancies '()])
    (define (loop rest index)
      (when (not (empty? rest))
        (begin
          (when (= (car rest) 1)
              (set! occupancies (cons index occupancies)))
          (loop (cdr rest) (- index 1)))))
    
    (loop occ-bits 8)
    (cons index occupancies)))

;return a list of lists for which the car is the position-module identifier and the cdr the occupied ports (use group-index and occupancies functions to access data)
(define (get-rmbus-data msg)
  (let*
      ([states      (get-msg-data msg 11 0)]
       [group-index (bytes-ref states 0)])
    (define (loop i res)
      (if (= i 11)
          res
          (loop (+ i 1) (cons (create-poslist i (bytes-ref states i)) res))))
    (loop 1 '())))

(define (get-rmbus-data-group-index msg)
  (let
      ([index-byte (get-msg-data-byte msg 0)])
    (read-byte index-byte)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV rmbus get data MSG (7.2) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;request the rmbus datachanged message for the given group-index (see explanation above) to the z21
(define (make-rmbus-get-data-msg group-index)
  (let
      ([len-lsb  "05"]
       [len-msb  "00"]
       [head-lsb "81"]
       [head-msb "00"]
       [data     group-index])
    (make-hex-msg len-lsb len-msb head-lsb head-msb data)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV rmbus program module MSG (7.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is used to set the address of a module. When programming a position module no other module should be connected to the r-bus!
;In order to set the address of a position module, one must send the intended address using this message and finalise the message by sending the "set address to zero message"
;Valid addresses are 1 to 20 (0 is reserved for the "end program module" message)
;Before sending a (make-rmbus-programmodule-msg 0) message, one must wait for the module to stop blinking (see led)

(define (make-rmbus-programmodule-msg new-address)
  (let
      ([len-lsb  "05"]
       [len-msb  "00"]
       [head-lsb "82"]
       [head-msb "00"]
       [data     new-address])
    (make-hex-msg len-lsb len-msb head-lsb head-msb data)))
