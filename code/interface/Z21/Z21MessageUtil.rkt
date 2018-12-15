#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt")
(provide get-msg-length get-msg-header get-msg-data-byte get-msg-data template-wildcard make-msg-template msg-eq? make-dec-msg make-hex-msg read-byte read-bytes read-word 32-convert 8-convert pad)

;;;;;;;;;;;;;;;;;;;;;;;;;;
;General Message layout ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;All words are little endian unless specified otherwise 

;Message length byte indices
(define msg-length-lsb 0)
(define msg-length-msb 1)

;Message header byte indices
(define msg-header-lsb 2)
(define msg-header-msb 3)

;Message data byte start index
(define msg-data-start 4)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;General Message operations ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;ACCESSORS
(define (get-msg-length msg)
  (read-word (bytes-ref msg msg-length-lsb) (bytes-ref msg msg-length-msb)))

(define (get-msg-header msg)
  (read-word (bytes-ref msg msg-header-lsb) (bytes-ref msg msg-header-msb)))

;Return the data byte at position byte-index starting from the data-area (i.e. offset 4 in total udp-packet). Data-indices start at offset 0
(define (get-msg-data-byte msg byte-index)
  (bytes-ref msg (+ msg-data-start byte-index)))

;Return a byte string with the given length of data
(define (get-msg-data msg length offset)
  (define (loop idx res)
    (if (>= idx length)
        res
        (loop (+ idx 1) (bytes-append res (bytes (get-msg-data-byte msg (+ idx offset)))))))
  (loop 0 (bytes)))

;MESSAGE EQUALITY

(define template-wildcard '*)

;Either provide a string representing a hexadecimal value or a template wildcard. This function constructs a template to match for by msg-eq
(define (make-msg-template . bts)
  (define (loop rest res)
    (if (empty? rest)
        res
        (loop (cdr rest) (if (eq? (car rest) template-wildcard) (append res (list template-wildcard)) (append res (list (hex-string->byte (car rest))))))))
  (loop bts '()))

;Two messages are equal if their content is equal (except for variable parts)
(define (msg-eq? received template)
  (define (iter rec-index temp-rest)
    (cond
      ((or (= rec-index (- (bytes-length received) 1)) (empty? temp-rest)) true)
      ((eq? (car temp-rest) template-wildcard) (iter (+ rec-index 1) (cdr temp-rest)))
      ((= (bytes-ref received rec-index) (car temp-rest)) (iter (+ rec-index 1) (cdr temp-rest)))
      (else false)))
  (iter 0 template))

;CREATORS

;Create a generic message. This converts passed values into a byte string
(define (make-msg converter len-lsb len-msb head-lsb head-msb data)
    (define (make-data rest res)
    (if (empty? rest)
        res
        (make-data (cdr rest) (bytes-append res (bytes (converter (car rest)))))))
    (define meta-data (bytes (converter len-lsb) (converter len-msb) (converter head-lsb) (converter head-msb)))
    (bytes-append meta-data (make-data data (bytes))))

;Create a message with the provided decimal values 
(define (make-dec-msg len-lsb len-msb head-lsb head-msb . data)
  (make-msg (lambda (val) val) len-lsb len-msb head-lsb head-msb data)) ;Racket converts decimal values to bytes automatically (no need to convert)

;Create a message with the provided hexadecimal string values
(define (make-hex-msg len-lsb len-msb head-lsb head-msb . data)
  (make-msg (lambda (val) (hex-string->byte val)) len-lsb len-msb head-lsb head-msb data))


;;;;;;;;;;;;;;;;;;;;;;;;;
;Reading Messages      ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

(define (read-byte bt)
  (bit-list->integer (byte->bit-list bt)))

(define (read-word lsb msb)
  (define lsb-bits (byte->bit-list lsb))
  (define msb-bits (byte->bit-list msb))
  (bit-list->integer (append msb-bits lsb-bits)))

(define (read-bytes bts)
  (define (loop rest-index res)
    (if (= rest-index (- (bytes-length bts) 1))
        res
        (loop (+ rest-index 1) (append res (byte->bit-list (bytes-ref bts rest-index))))))
  (bit-list->integer (loop 0 '())))

;;;;;;;;;;;;;;
;Byte Utils ;;
;;;;;;;;;;;;;;

;Convert a 32 bit value (i.e. 4 bytes) from big to little endian or vice versa
(define (32-convert bts)
  (if (not (= (bytes-length bts) 4))
      (raise "Incorrect byte length")
      (let
          ([ret (make-bytes 4)])
           (bytes-set! ret 0 (bytes-ref bts 3))
           (bytes-set! ret 1 (bytes-ref bts 2))
           (bytes-set! ret 2 (bytes-ref bts 1))
           (bytes-set! ret 3 (bytes-ref bts 0))
           ret)))

;Convert a 8 bit value from big to little endian or vice versa
(define (8-convert bt)
  (if (not (byte? bt))
      (raise "Incorrect byte length")
      (let
          ([bits       (byte->bit-list bt)])
        (define-values (bits-lsb bits-msb) (split-at bits 4))
        (bit-list->byte (append bits-msb bits-lsb)))))

;Pad the given list of bits with zero's (at the front) until the desired length is reached
(define (pad bits len)
  (if (= (length bits) len)
      bits
      (pad (cons 0 bits) len)))



