#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide make-cv-read-msg make-cv-write-msg is-cv-nack-sc-msg? is-cv-nack-msg? is-cv-result-msg?  get-cv-result-address get-cv-result-result generate-cv-msb make-cv-pom-write-byte-msg make-cv-pom-write-bit-msg make-cv-pom-read-msg make-cv-pom-acc-write-byte-msg make-cv-pom-acc-write-bit-msg make-cv-pom-acc-read-byte-msg)

;Implementation of the decoder cv part of the protocol (based on document version 1.02)

;;;;;;;;;;;;;;;;;;;;;
;CV read MSG (6.1) ;;
;;;;;;;;;;;;;;;;;;;;;

;Request a particular cv value to z21 (given its address)
;z21 responds with bc-programming-mode (for subsribed clients) and
;cv-nack-sc (see 6.3), cv-nack (see 6.4) or cv-result (see 6.5)

(define (make-cv-read-msg add-lsb add-msb)
  (let
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "23"]
       [db0      "11"]
       [db1      add-msb]
       [db2      add-lsb]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)))

;;;;;;;;;;;;;;;;;;;;;;
;CV write MSG (6.2) ;;
;;;;;;;;;;;;;;;;;;;;;;

;Request to write the value of a particular cv (given its address) to z21
;z21 responds with bc-programming-mode (for subsribed clients) and
;cv-nack-sc (see 6.3), cv-nack (see 6.4) or cv-result (see 6.5)

(define (make-cv-write-msg add-lsb add-msb value)
  (let
      ([len-lsb  "0a"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "24"]
       [db0      "12"]
       [db1      add-msb]
       [db2      add-lsb]
       [db3      value]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;
;CV nack sc MSG (6.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;

;Message sent by the z21 to the client if something went wrong while trying to read or write a cv (e.g. a short circuit)

(define (is-cv-nack-sc-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "12"]
       [xor      "73"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;
;CV nack MSG (6.4) ;;
;;;;;;;;;;;;;;;;;;;;;

;Message send by the z21 to the client if it was unable to receive an acknowledgment after a cv-read or cv-write 

(define (is-cv-nack-msg? msg)
  (let*
      ([len-lsb "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "13"]
       [xor      "73"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))


;;;;;;;;;;;;;;;;;;;;;;;
;CV result MSG (6.5) ;;
;;;;;;;;;;;;;;;;;;;;;;;

;Contains the result of either a cv-read, or the value written after a cv-write

(define (is-cv-result-msg? msg)
  (let*
      ([len-lsb  "0a"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "64"]
       [db0      "14"]
       [db1      template-wildcard]
       [db2      template-wildcard]
       [db3      template-wildcard]
       [xor      template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 xor)])
    (msg-eq? msg template)))

(define (get-cv-result-address msg)
  (let
      ([add-msb (get-msg-data-byte msg 2)]
       [add-lsb (get-msg-data-byte msg 3)])
    (read-word add-lsb add-msb)))

;Returns the value either requested (with read) or written (with write)
(define (get-cv-result-result msg)
  (get-msg-data-byte msg 4))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom write byte MSG (6.6) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;similar to the cv write message (see 6.2) except that programming mode is not enabled and track voltage must be on
;pom = programming on main track. Feedback will not be provided by the z21

;Only two bits of the msb are used, the rest is entertwined with other bits in a single byte
;Since cv addresses are limited (currenlty 256) this is not a problem
(define (generate-cv-msb msb static-bits)
  (let*
      ([cv-bits  (byte->bit-list msb)]
       [last-two (list-tail cv-bits 6)])
    (byte->hex-string (bit-list->byte (append static-bits last-two)))))

;Since pom does not require the loc to be on the prog track the loc's address needs to be specified (besides cv address and value)
(define (make-cv-pom-write-byte-msg loc-add-lsb loc-add-msb cv-add-lsb cv-add-msb value)
  (let
      ([len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "30"]
       [db1      loc-add-msb]
       [db2      loc-add-lsb]
       [db3      (generate-cv-msb (hex-string->byte cv-add-msb) '(1 1 1 0 1 1))]
       [db4      cv-add-lsb]
       [db5      value]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db1 db2 db3 db4 db5 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom write bit MSG (6.7) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;similar to the cv write message (see 6.2) except that programming mode is not enabled and track voltage must be on
;furthermore this message can only be used to send a single bit (in contrast to the regular byte)
;pom = programming on main track. Feedback will not be provided by the z21


;6.7 only writes a single bit, the value bit and it's bit position inside the cv byte need to be assembled before transmission
(define (generate-value value bit-pos)
  (let
      ([position-bits (integer->bit-list bit-pos)])
    (if (> (length position-bits) 3) ;position inside a byte cannot be bigger than 7
        (raise (string-append "Illegal bit position: " (number->string bit-pos)))
        (byte->hex-string (bit-list->byte (append '(0 0 0 0) (cons value position-bits)))))))

;Since pom does not require the loc to be on the prog track the loc's address needs to be specified (besides cv address and value)
;Since this message will only set a single bit of a given cv, an extra argument indicating the bit's position in the cv-byte is needed
(define (make-cv-pom-write-bit-msg loc-add-lsb loc-add-msb cv-add-lsb cv-add-msb value bit-pos)
  (let
      ([len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "30"]
       [db1      loc-add-msb]
       [db2      loc-add-lsb]
       [db3      (generate-cv-msb (hex-string->byte cv-add-msb) '(1 1 1 0 1 0))]
       [db4      cv-add-lsb]
       [db5      (generate-value value bit-pos)]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom read byte MSG (6.8) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is only supported for z21 firmware version >= 1.22
;similar to the cv read message (see 6.1) except that programming mode is not enabled and track voltage must be on
;pom = programming on main track. z21 answers with cv-nack (see 6.4) or cv-result (see 6.5)

(define (make-cv-pom-read-msg loc-add-lsb loc-add-msb cv-add-lsb cv-add-msb)
  (let
      ([len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "30"]
       [db1      loc-add-msb]
       [db2      loc-add-lsb]
       [db3      (generate-cv-msb (hex-string->byte cv-add-msb) '(1 1 1 0 1 0))]
       [db4      cv-add-lsb]
       [db5      "00"]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom accessory write byte MSG (6.9) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is only supported for z21 firmware version >= 1.22
;message is used to write a cv's byte of an accessory decoder (such as signalisation light)
;no answer is provided by the z21

(define (create-db2 dec-add whole-decoder output)
  (let
      ([add-bits (byte->bit-list (byte<< (byte-and dec-add (hex-string->byte "ff")) 4))])
    (if whole-decoder
        (byte->hex-string (bit-list->byte (append add-bits '(0 0 0 0))))
        (byte->hex-string (bit-list->byte (append add-bits (cons 1 (integer->bit-list (car output)))))))))

;whole-decoder is a boolean which indicates whether the cv refers to the whole decoder. If this is not the case one needs to specify the output port to programme
(define (make-cv-pom-acc-write-byte-msg dec-add-lsb dec-add-msb whole-decoder cv-add-lsb cv-add-msb value . output-port)
  (let*
      ([dec-add (car (integer->byte-list (read-word dec-add-lsb dec-add-msb)))]
       [len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "31"]
       [db1      dec-add-msb]
       [db2      (create-db2 dec-add whole-decoder output-port)]
       [db3      (generate-cv-msb cv-add-msb '(1 1 1 0 1 1))]
       [db4      cv-add-lsb]
       [db5      value]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom accessory write bit MSG (6.10) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;similar to the previous message (see 6.9), except that only a single bit of the cv will be written

;whole-decoder is a boolean which indicates whether the cv refers to the whole decoder. If this is not the case one needs to specify the output port to programme
;Since only a single bit of the cv will be written, the position of this bit needs to be specified
(define (make-cv-pom-acc-write-bit-msg dec-add-lsb dec-add-msb whole-decoder cv-add-lsb cv-add-msb value bit-pos . output-port)
  (let*
      ([dec-add (car (integer->byte-list (read-word dec-add-lsb dec-add-msb)))]
       [len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "31"]
       [db1      dec-add-msb]
       [db2      (create-db2 dec-add whole-decoder output-port)]
       [db3      (generate-cv-msb cv-add-msb '(1 1 1 0 1 0))]
       [db4      cv-add-lsb]
       [db5      (generate-value value bit-pos)]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;CV pom accessory read byte MSG (6.11) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Reads the byte of a particular accessory decoder's cv
;z21 answers with either cv-nack (see 6.4) or cv-result (see 6.5)
(define (make-cv-pom-acc-read-byte-msg dec-add-lsb dec-add-msb whole-decoder cv-add-lsb cv-add-msb . output-port)
  (let*
      ([dec-add (car (integer->byte-list (read-word dec-add-lsb dec-add-msb)))]
       [len-lsb  "0c"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "e6"]
       [db0      "31"]
       [db1      dec-add-msb]
       [db2      (create-db2 dec-add whole-decoder output-port)]
       [db3      (generate-cv-msb cv-add-msb '(1 1 1 0 1 0))]
       [db4      cv-add-lsb]
       [db5      "00"]
       [xor      "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 db3 db4 db5 xor)))
        


