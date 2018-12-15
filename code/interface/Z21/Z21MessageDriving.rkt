#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide make-get-loco-info-msg make-set-loco-drive-msg low-speed-range med-speed-range high-speed-range make-set-loco-function-msg is-loco-info-msg? get-loco-info-address loco-info-busy? get-loco-info-forward? get-loco-info-speed get-loco-info-speed-range loco-info-doppeltraction? loco-info-smartsearch? loco-info-func-on? func-off func-once func-switch 
 func-nap)

;Implementation of the driving part of the protocol (based on document version 1.02)

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get loco info MSG (4.1) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Requests information concerning a certain loc (address given as least and most significant bytes), automatically subscribes the client to information concerning the loc
;z21 answers with the loco-info msg (see 4.4)

(define (make-get-loco-info-msg add-lsb add-msb)
  (let
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e3"]
       [db0      "f0"]
       [db1      add-msb]
       [db2      add-lsb]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set loco drive MSG (4.2) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Specifies the driving speed for a certain loc (address given as least and most significant bytes)
;Extra arguments include the speed ranges (see generate speed-range function), the direction (true = forwards, false = backwards) and the speed (based on the generate-speed function)
;z21 answers to client subscribed to loco-info messages (see 4.4)

;Speed ranges, as dictated by the z21 protocol can be either 14, 28 or 128

(define low-speed-range  14)
(define med-speed-range  28)
(define high-speed-range  128)

(define (generate-speed-range range)
  (cond
    ((= range low-speed-range)  "11") ;Speed range 14 is send as 0x11
    ((= range med-speed-range)  "12") ;Speed range 28 is send as 0x12
    ((= range high-speed-range)  "13") ;Speed range 128 is send as 0x13
    (else (raise (string-append "Unknown speed range: " (number->string range))))))

;Forward should be a boolean value indicating the trains direction (i.e. true = forward, false = backwards), speed should be [0,127[
;The speed of a train is an 8-bit value, for which first bit indicates direction (1 being forward) and the remaining 7 bits indicate speed
(define (generate-speed forward speed)
  (let
      ([dir (if forward 1 0)]
       [speed-bits (integer->bit-list speed)])
    (cond
      ((> speed 127) (raise (string-append "Unable to generate speed for speed value: " (number->string speed))))
      ((< (length speed-bits) 7)  (bit-list->byte (cons dir (pad speed-bits 7))))
      (else (bit-list->byte (cons dir speed-bits))))))

;range should be either 14,28 or 128. Forward is a boolean (true = forwards, false = backwards). Speed should be [0,127]
(define (make-set-loco-drive-msg add-lsb add-msb range forward speed)
  (let
      ([len-lsb  "0a"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e4"]
       [db0      (generate-speed-range range)]
       [db1      add-msb]
       [db2      add-lsb]
       [db3      (byte->hex-string (generate-speed forward speed))]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set loco function MSG (4.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Activates a given loc-function (e.g. lights) given the loc-address (given as least and most significant bytes), a function index (e.g. 0 = lights) and a function type (see generate-function).
;z21 answers if the client is subscribed to the loco-info messages (see 4.4)

(define func-off    '(0 0)) ;Turn the loc function off
(define func-once   '(0 1)) ;The loc function acts once and stops
(define func-switch '(1 0)) ;The loc function needs to be switched on and off
(define func-nap    '(1 1)) ;The above actions are not applicable

;Given a type (see above) and a function index (integer indicating which function should be created), this function generates an 8-bit value,
;for which the first two bits indicate the type and 6 remaining indicate which function should be called on the loc
(define (generate-function type func-index)
  (let
      ([types      (list func-off func-once func-switch func-nap)]
       [index-bits (integer->bit-list func-index)])
    (cond
      ((not (member type types)) (raise (string-append "Unknown function type: " (list->string type))))
      ((< (length index-bits) 7) (bit-list->byte (cons (car type) (cons (cadr type ) (pad index-bits 6)))))
      (else (bit-list->byte (cons (car type) (cons (cadr type) (bit-list->byte index-bits))))))))

(define (make-set-loco-function-msg add-lsb add-msb type func-num)
  (let
      ([len-lsb  "0a"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e4"]
       [db0      "f8"]
       [db1      add-msb]
       [db2      add-lsb]
       [db3      (byte->hex-string (generate-function type func-num))]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 xor)))

;;;;;;;;;;;;;;;;;;;;;;;
;Loco info MSG (4.4) ;;
;;;;;;;;;;;;;;;;;;;;;;;

;Message sent by the z21 to a client containing information concerning a specific loc. This message is send to the client after a get-loco-info message (see 4.1) or when:
;The loc status has been changed by another client or the client has activated the appropriate flag 0x00000001 (see 2.16)
;and was subscribed to this specific loc with get-loco-info (see 4.1)

;The message varies in length depending on the amount of loc functionality (In our setup of locs and the z21 this not the case yet so one can assume length of 7 bytes for the moment)
;The layout of the message goes as follows:
;+---------+-----------------+-----------------------------------------------------------------------------------------+--+
;| Postion |      Data       |                                         Comment                                         |  |
;+---------+-----------------+-----------------------------------------------------------------------------------------+--+
;| db0     | Loc address msb |                                                                                         |  |
;| db1     | Loc address lsb |                                                                                         |  |
;| db2     | 0000BKKK        | B=1: Loc controlled by other device ("Busy"). KKK = speed range info: 0=14, 2=28, 4=128 |  |
;| db3     | RVVVVVVV        | R=direction: 1=forward,0=backward. VVVVVVV = speed (depending on speed range)           |  |
;| db4     | 0DSLFGHJ        | D=Doppeltraction (on/off), S=SmartSearch, L=F0 (light), F=F4, g=F3, H=F2, J=F1          |  |
;| db5     | F5-F12          | F5= bit  0 (lsb)                                                                        |  |
;| db6     | F13-F20         | F13= bit 0 (lsb)                                                                        |  |
;| db7     | F21-F28         | F21= bit 0 (lsb)                                                                        |  |
;| dbn     |                 | (optional) For future extensions                                                        |  |
;+---------+-----------------+-----------------------------------------------------------------------------------------+--+


;Currently the standard version of loco info message is supported, additional functions will be needed for future extensions
(define (is-loco-info-msg? msg)
  (let*
      ([len-lsb  template-wildcard]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "ef"]
       [db0      template-wildcard]
       [db1      template-wildcard]
       [db2      template-wildcard]
       [db3      template-wildcard]
       [db4      template-wildcard]
       [db5      template-wildcard]
       [db6      template-wildcard]
       [db7      template-wildcard]
       [xor      template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 db6 db7 xor)])
    (msg-eq? msg template)))

(define (get-loco-info-address msg)
  (let
      ([add-msb (get-msg-data-byte msg 1)] ;First data byte is the x-head byte
       [add-lsb (get-msg-data-byte msg 2)])
    (read-word add-lsb add-msb)))

(define (loco-info-busy? msg)
  (let
      ([busy-bit (list-ref (byte->bit-list (get-msg-data-byte msg 3)) 4)])
    (= busy-bit 1)))

(define (get-loco-info-speed-range msg)
  (let*
      ([range     (list-tail (byte->bit-list (get-msg-data-byte msg 3)) 5)])
    (bit-list->integer range)))

(define (get-loco-info-forward? msg)
  (let*
      ([speed-byte (get-msg-data-byte msg 4)]
       [direction  (list-ref (byte->bit-list speed-byte) 0)]) ;Direction is first bit (msb) of the speed-byte 1 = forwards
    (= direction 1)))

(define (get-loco-info-speed msg)
  (let
      ([speed-byte (get-msg-data-byte msg 4)])
    (bit-list->integer (cdr (byte->bit-list speed-byte))))) ;Speed is given by the seven remaining bits of the speed byte

(define (loco-info-doppeltraction? msg)
  (let
      ([doppel-bit (list-ref (byte->bit-list (get-msg-data-byte msg 5)) 1)])
    (= doppel-bit 1)))

(define (loco-info-smartsearch? msg)
  (let
      ([smart-bit (list-ref (byte->bit-list (get-msg-data-byte msg 5)) 2)])
    (= smart-bit 1)))

(define (in-range? start end num)
  (and (>= num start) (<= num end)))

;Returns the function state as a boolean, given the byte (starting from the data area of an msg) holding the func's state and the offset within the byte
(define (get-func msg byte-index offset)
  (let*
      ([byte      (get-msg-data-byte msg byte-index)]
       [bits-big  (byte->bit-list  byte)])
    (= (list-ref bits-big offset) 1)))

;The first four functions are interleaved with other data (e.g. smartsearch) and need to be handled differently (could use clean-up) !!
(define (get-base-func msg func-index)
  (let
      ([bits (byte->bit-list (get-msg-data-byte msg 5))])
    (cond
      ((= func-index 0) (= (list-ref bits 3) 1))
      ((= func-index 1) (= (list-ref bits 7) 1))
      ((= func-index 2) (= (list-ref bits 6) 1))
      ((= func-index 3) (= (list-ref bits 5) 1))
      (else (= (list-ref bits 4) 1)))))
   
   
;The byte is transformed (by loco-info-func-on) to big endian, highest-index function has offset 0
(define (get-offset highest-index func-index)
  (- highest-index func-index))

(define (loco-info-func-on? msg func-index)
  (cond
    ((in-range? 0 4   func-index)   (get-base-func msg func-index)) ;First five functions are special (due to interleaved data)
    ((in-range? 5 12  func-index)   (get-func msg 6 (get-offset 12 func-index)))
    ((in-range? 13 20 func-index)   (get-func msg 7 (get-offset 20 func-index)))
    ((in-range? 21 28 func-index)   (get-func msg 8 (get-offset 28 func-index)))
    (else (raise (string-append "Function index not supported yet: " (number->string func-index))))))
    
