#lang racket

(require "NMBS.rkt" "configuration.rkt" "railway.rkt" "INFRABEL.rkt")

(println "Created INFRABEL server")
(define INFRABEL (make-object INFRABEL%))