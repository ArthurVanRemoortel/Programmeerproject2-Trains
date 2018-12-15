#lang racket
(require "bits.rkt")
(require "xxtea.rkt")

(define gc-key 
  (foldr bytes-append (bytes)
         (map int32->bytes 
              '(#xfd345311 #xfd345311 #xfd345311 #xfd345311))))

(define test-bytes (make-bytes 16))

(for-each (lambda (v)
            (display (format "~x " v)))
          (bytes->list test-bytes))

((xxtea-encoder gc-key) test-bytes 4)
(newline)
(display "after encode")
(newline)
(for-each (lambda (v)
            (display (format "~x " v)))
          (bytes->list test-bytes))
             

((xxtea-decoder gc-key) test-bytes 4)
(newline)
(display "after decode")
(newline)
(for-each (lambda (v)
            (display (format "~x " v)))
          (bytes->list test-bytes))
             