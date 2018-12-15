#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(require racket/udp)
(require data/queue)
(provide setup listen send get-message get-message* read-messages)

;This is a queue containing all answers from the z21
(define answer-queue (make-queue))

;Create a socket to communicate with the z21
(define (setup)
  (begin
    (define z21 (udp-open-socket #f #f))
    (udp-bind! z21 #f 21105) ;Sockets need to be bound to port 22105 (same as port used by z21). The address does not matter
    (udp-connect! z21 "192.168.0.111" 21105) ;z21's default address is 192.168.0.111 and listens on port 22015
    z21))

(define (send socket message)
  (udp-send socket message))

;Listen for answers from z21 and enqueue all received messages. This version is multi-threaded to avoid blocking
;When an optional reader is provided, the message will be read by the reader before being queued
(define (listen socket . reader)
  (define (listener)
    (define answer (make-bytes 65535))
    (udp-receive! socket answer)
    (when (not (empty? reader))
      ((car reader) answer))
    (enqueue! answer-queue answer)
    (listener))
  (thread listener)
  #t)

;Read all messages given a reader-lambda taking a single message as argument
(define (read-messages reader)
  (when (not (queue-empty? answer-queue))
      (begin
        (reader (dequeue! answer-queue))
        (read-messages))))

;Generic message getter
(define (create-getter get-method socket length)
  (define answer (make-bytes length))
  (get-method socket answer)
  answer)

;Block until a message is received from the z21
(define (get-message socket [length 65535])
  (create-getter udp-receive! socket length))

;Get a return message from the z21 (non-blocking)
(define (get-message* socket [length 65536])
  (create-getter udp-receive!* socket length))


