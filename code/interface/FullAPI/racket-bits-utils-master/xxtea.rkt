#lang racket
(require "bits.rkt")

(provide xxtea-encoder
         xxtea-decoder)

;; 往一个int32 的数组中放值
(define (int32-bytes-set! bytes pos value)
  (let ((buf (int32->bytes value)))
    (for-each (lambda (x)
                (bytes-set! bytes (+ (* pos 4) x) (bytes-ref buf x)))
              (range 4))))

;; 从一个int32数组中取值
(define (int32-bytes-ref bytes pos)
  (integer-bytes->integer (subbytes bytes (* pos 4) (+ (* pos 4) 4)) 4 #f))

(define DELTA #x9E3779b9)

(define (xxtea-encoder key)
  (lambda (v n)
    (let ((y 0)
          (z 0)
          (sum 0)
          (p 0)
          (e 0))
      (define (MX)
        (bitwise-xor
         (int32-add 
          (bitwise-xor (int32>> z 5) (int32<< y 2))
          (bitwise-xor (int32>> y 3) (int32<< z 4)))
         (int32-add
          (bitwise-xor sum y)
          (bitwise-xor 
           (int32-bytes-ref key (bitwise-xor (bitwise-and p 3) e))
           z))))
        
      ;; 主循环
      (define (iter-rounds rounds)
        (unless (= 0 rounds)
          (set! sum (int32-add sum DELTA))
          (set! e (bitwise-and (int32>> sum 2) 3))
          ;; iter
          (define (iter-step)
            (when (< p (- n 1))
              (set! y (int32-bytes-ref v (+ p 1)))
              (let ((mxval (MX)))
                (int32-bytes-set! v p (+ (int32-bytes-ref v p) mxval))
                (set! z (int32-bytes-ref v p)))
              (set! p (+ p 1))
              (iter-step)))
          (set! p 0)
          (iter-step)
          (set! y (int32-bytes-ref v 0))
          (let ((mx-val (MX)))
            (int32-bytes-set! v (- n 1) (+ (int32-bytes-ref v (- n 1)) mx-val))
            (set! z (int32-bytes-ref v (- n 1))))
          (iter-rounds (- rounds 1))))
        
      (set! z (int32-bytes-ref v (- n 1)))
      (set! sum 0)
      (iter-rounds (+ 6 (quotient 52 n))))))



      
(define (xxtea-decoder key)
  (lambda (v n)
    (let ((y 0)
          (z 0)
          (sum 0)
          (p 0)
          (e 0))
      (define (MX)
        (bitwise-xor
         (int32-add 
          (bitwise-xor (int32>> z 5) (int32<< y 2))
          (bitwise-xor (int32>> y 3) (int32<< z 4)))
         (int32-add
          (bitwise-xor sum y)
          (bitwise-xor 
           (int32-bytes-ref key (bitwise-xor (bitwise-and p 3) e))
           z))))

      (define (iter-sum)
        (set! e (bitwise-and (int32>> sum 2) 3))
        (define (iter-p)
          (when (> p 0)
            (set! z (int32-bytes-ref v (- p 1)))
            (int32-bytes-set! v p (- (int32-bytes-ref v p) (MX)))
            (set! y (int32-bytes-ref v p))
            (set! p (- p 1))
            (iter-p)))
        (set! p (- n 1))
        (iter-p)
        (set! z (int32-bytes-ref v (- n 1)))
        (int32-bytes-set! v 0 (- (int32-bytes-ref v 0) (MX)))
        (set! y (int32-bytes-ref v 0))
        (set! sum (int32-minus sum DELTA))
        (when (not (= 0 sum))
          (iter-sum)))
        
      (set! sum (int32-multiplier (+ 6 (quotient 52 n)) DELTA))
      (set! y (int32-bytes-ref v 0))
      (iter-sum))))
