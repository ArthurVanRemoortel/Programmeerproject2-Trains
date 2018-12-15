#lang racket
(require "lzss.rkt")

(define (lzss-file infile outfile)
  (let ((input (open-input-file infile))
        (output (open-output-file outfile #:exists 'replace)))
    ((make-lzss-default-decoder) input output)
    (close-input-port input)
    (close-output-port output))
  )
  
(lzss-file "lzss.c.out.c" "lzss.c.decoder.c")