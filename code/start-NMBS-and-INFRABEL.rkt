#lang racket
(require "NMBS.rkt" "configuration.rkt" "INFRABEL.rkt")

(thread
 (lambda ()
   (println "Starting INFRABEL")
   (make-object INFRABEL%)
   (println "INFRABEL is running ...")))
(sleep 1)
(define nmbs (make-object NMBS%))
(send nmbs start-gameloop)



;(begin (send nmbs make-path 'L1 'D4) (send nmbs make-path 'L2 'D5))