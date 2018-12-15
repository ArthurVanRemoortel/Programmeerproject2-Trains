#lang racket

(require "NMBS.rkt" "configuration.rkt")

(define nmbs (make-object NMBS%))
(send nmbs start-gameloop)
