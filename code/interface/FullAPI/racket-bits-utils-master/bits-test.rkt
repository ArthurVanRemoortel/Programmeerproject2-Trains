#lang racket
(require "bits.rkt")
  
(define (output-int value)
  (display value)
  (newline)
  (display "bits : ")
  (display (byte->bit-list value))
  (newline)
  (display "hex-string : ")
  (display (byte->hex-string value)))

(output-int 255)

(define (output-bytes bytes)
  (let ((l (bytes->list bytes)))
    (for-each 
     (lambda (v)
       (display "0x")
       (display (byte->hex-string v))
       (display " "))
     l)))

(newline)
(output-bytes (bytes 1 2 3 4 5 5 6 78 35 45  32 23 23 3 4 4 5 5 66 6 6 66 6 6))