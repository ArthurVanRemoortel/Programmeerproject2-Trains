#lang racket

(provide byte->bit-list
         bit-list->byte
         byte->hex-string
         hex-string->byte
         integer->hex-char
         hex-char->integer
         byte-and
         byte-or
         byte-xor
         byte>>
         byte<<
         
         integer->bit-list
         bit-list->integer
         hex-string->integer
         
         integer-and
         integer-or
         integer-xor
         integer>>
         integer<<
         integer->byte-list
         
         int32-add
         int32-minus
         int32-multiplier
         int32>>
         int32<<
         int32->bytes
         )

;; 字节转换为位表示
(define (byte->bit-list value)
  (define (iter p value count)
    (if (= count 0)
        p
        (iter (cons (remainder value 2) p) (quotient value 2) (- count 1))))
  (cond 
    ((byte? value)
     (iter '() value 8))
    ((char? value)
     (iter '() (char->integer value) 8))
    (else
     (error "please input a byte : " value))))

;; 字节的位表示转换为byte
(define (bit-list->byte bit-list)
  (define (iter product l count)
    (if (or (null? l) 
            (= count 0))
        product
        (iter (+ (* 2 product) (car l)) (cdr l) (- count 1))))
  (iter 0 bit-list 8))

;; 字节转化为十六进制字符串表示
(define (byte->hex-string value)
  (define (sub-byte->char subbyte)
    (define (iter product bytes)
      (if (null? bytes)
          product
          (iter (+ (* product 2) (car bytes)) (cdr bytes))))
    (iter 0 subbyte))
  (let ((bit-list (byte->bit-list value)))
    (string 
     (integer->hex-char (sub-byte->char 
                     (list (first bit-list)
                           (second bit-list)
                           (third bit-list)
                           (fourth bit-list))))
     (integer->hex-char (sub-byte->char
                     (list-tail bit-list 4))))))

;; 字节的十六进制表示转换为字节值
(define (hex-string->byte hex-string)
  (let ((first-char 
         (if (> (string-length hex-string) 1)
             (string-ref hex-string 0)
             #\0))
        (second-char (string-ref hex-string (min 1 (- (string-length hex-string) 1)))))
    (+ (* 16 (hex-char->integer first-char))
       (hex-char->integer second-char))))
         
;;一个十六进制字符的值转化为对应的十六进制字符
(define (integer->hex-char hex-value)
  (cond ((and (>= hex-value 0)
              (<= hex-value 9))
         (integer->char (+ hex-value (char->integer #\0))))
        ((and (>= hex-value 10)
              (<= hex-value 15))
         (integer->char (+ (- hex-value 10) (char->integer #\A))))
        (else 
         (error "unknown integer->hex-char arg : " hex-value))))


;;一个十六进制字符转化为对应值
(define (hex-char->integer hex-char)
  (cond ((and (char>=? hex-char #\0)
              (char<=? hex-char #\9))
         (- (char->integer hex-char) 
            (char->integer #\0)))
        ((and (char>=? hex-char #\A)
              (char<=? hex-char #\F))
         (- (char->integer hex-char)
            (char->integer #\A)
            -10))
        ((and (char>=? hex-char #\a)
              (char<=? hex-char #\f))
         (- (char->integer hex-char)
            (char->integer #\a)
            -10))
        (else
         (error "unknown hex-char->integer arg: " hex-char))))
         
;; 字节的与操作（位运算) 
(define (byte-and byte1 byte2)
  (bitwise-and byte1 byte2 #xFF))
(define (byte-or byte1 byte2)
  (bitwise-and (bitwise-ior byte1 byte2) #xFF))
(define (byte-xor byte1 byte2)
  (bitwise-and (bitwise-xor byte1 byte2) #xFF))

(define (byte<< byte count) 
  (bitwise-and #xFF
               (arithmetic-shift byte count)))

(define (byte>> byte count)
  (bitwise-and #xFF
               (arithmetic-shift byte (- count))))

;; integer ->bit-list
(define (integer->bit-list value)
  (define (iter value product)
    (if (= 0 value)
        product
        (iter (quotient value 2)
              (cons (remainder value 2) product))))
  (iter value '()))

  
; bit-list->integer
(define (bit-list->integer bit-list)
  (define (iter product bit-list)
    (if (null? bit-list)
        product
        (iter (+ (* 2 product) (car bit-list))
              (cdr bit-list))))
  (iter 0 bit-list))

;hex-string->integer
(define (hex-string->integer str)
  (foldl (lambda (v result)
           (+ (* 16 result) (hex-char->integer v)))
         0
         (string->list str)))

;; 字节对齐 (前端补全0)
(define (bit-list-byte-align bits)
  (define (iter bits count)
    (if (= 0 count)
        bits
        (iter (cons 0 bits) (- count 1))))
  (if (= 0 (remainder (length bits) 8))
      bits
      (iter bits (- 8 (remainder (length bits) 8)))))

;; right shift
(define (integer>> value count)
  (arithmetic-shift value (- count)))
           
;; left shift
(define (integer<< value count)
  (arithmetic-shift value count))

;; add
(define (integer-and val1 val2)
  (bitwise-and val1 val2))

;; or
(define (integer-or val1 val2)
  (bitwise-ior val1 val2))

;; xor
(define (integer-xor val1 val2)
  (bitwise-xor val1 val2))

; integer->[byte]
(define (integer->byte-list val)
  (define (iter bits product)
    (if (null? bits)
        product
        (iter (list-tail bits 8)
              (append product (list (bit-list->integer (list 
                                        (list-ref bits 0)
                                        (list-ref bits 1)
                                        (list-ref bits 2)
                                        (list-ref bits 3)
                                        (list-ref bits 4)
                                        (list-ref bits 5)
                                        (list-ref bits 6)
                                        (list-ref bits 7))))))))
  (iter (bit-list-byte-align (integer->bit-list val))
        '()))


(define (int32-add x y)
  (bitwise-and (+ x y) #xFFFFFFFF))

(define (int32-multiplier x y)
  (bitwise-and (* x y) #xFFFFFFFF))
(define (int32-minus x y)
  (bitwise-and (- x y) #xFFFFFFFF))

(define (integer->int32 value)
  (bitwise-and value #xFFFFFFFF))

(define (int32>> val count)
  (integer->int32 (arithmetic-shift val (- count))))
(define (int32<< val count)
  (integer->int32 (arithmetic-shift val count)))

(define (int32->bytes val)
  (integer->integer-bytes val 4 #f))