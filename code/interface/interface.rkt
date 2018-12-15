#lang racket

(require racket/dict)

(require (prefix-in Z21: "Z21/Z21Socket.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageDriving.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageSwitches.rkt"))
(require (prefix-in Z21: "Z21/Z21MessageLocation.rkt"))
(require "Z21/racket-bits-utils-master/bits.rkt")

(provide start-simulator
         stop-simulator
         
         add-loco
         
         get-loco-speed
         set-loco-speed!
         get-loco-detection-block
         
         get-switch-position
         set-switch-position!)

;;;  GLOBALS  ;;;

;; ========================================================================== ;;
;;                                                                            ;;
;; TRAINS                                                                     ;;
;;                                                                            ;;
;; Map the names of the trains (a symbol) to their address.                   ;;
;;                                                                            ;;
;; Note that the trains as represented using symbols (e.g. 'T-3), while       ;;
;; their addresses are numbers (e.g. 3).                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define TRAINS
  #hash((T-3 . 3)))

;; ========================================================================== ;;
;;                                                                            ;;
;; SWITCHES                                                                   ;;
;;                                                                            ;;
;; Map the names of the switches (a symbol) to their address.                 ;;
;;                                                                            ;;
;; Note that the switches as represented using symbols 'S-1 to 'S-28, while   ;;
;; their addresses are numbers 0 to 27.                                       ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define SWITCHES
  #hash((S-1  . 0)
        (S-2  . 1)
        (S-3  . 2)
        (S-4  . 3)
        (S-5  . 4)
        (S-6  . 5)
        (S-7  . 6)
        (S-8  . 7)
        (S-9  . 8)
        (S-10 . 9)
        (S-11 . 10)
        (S-12 . 11)
        (S-16 . 15)
        (S-20 . 19)
        (S-23 . 22)
        (S-24 . 23)
        (S-25 . 24)
        (S-26 . 25)
        (S-27 . 26)
        (S-28 . 27)))

;;; INTERFACE ;;;

;; ========================================================================== ;;
;;                                                                            ;;
;; (start-simulator)                                                          ;;
;;                                                                            ;;
;; Creates the TCP connection to communicate with the hardware.               ;;
;;                                                                            ;;
;; Note: this function is called start-simulator only to be compatible with   ;;
;; the existing simulator.                                                    ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (start-simulator)
  (set! socket (Z21:setup))
  (Z21:listen socket Z21:handle-msg))

;; ========================================================================== ;;
;;                                                                            ;;
;; (stop-simulator)                                                           ;;
;;                                                                            ;;
;; Stop. When using the real hardware, this doesn't do anything.              ;;
;;                                                                            ;;
;; Note: this function is called stop-simulator only to be compatible with    ;;
;; the existing simulator.                                                    ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (stop-simulator)
  #f)

;; ========================================================================== ;;
;;                                                                            ;;
;; (add-loco TRAIN-ID PREVIOUS-SEGMENT-ID CURRENT-SEGMENT-ID)                 ;;
;;                                                                            ;;
;; Adds a train to the segment with id CURRENT-SEGMENT-ID. The direction of   ;;
;; the train is determined by PREVIOUS-SEGMENT-ID.                            ;;
;;                                                                            ;;
;; This doesn't do anything when using the real hardware: you have to put the ;;
;; train on the tracks yourself :)                                            ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (add-loco 'T-1 'S-27 '1-3)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (add-loco id previous-segment-id current-segment-id)
  #f)

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-speed TRAIN-ID)                                                  ;;
;;                                                                            ;;
;; Returns the current speed of the train with id TRAIN-ID. If the train is   ;;
;; going backwards, returns a negative number.                                ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-speed 'T-1)                                                      ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-loco-speed id)
  (let*  ((addr  (dict-ref TRAINS id))
          (lsb   (byte->hex-string addr))
          (msb   "00")
          (msg   (Z21:make-get-loco-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-loco-info-msg?))
    (Z21:send-msg msg)
    (let* ((resp  (Z21:get-msg type? time))
           (fwd?  (Z21:get-loco-info-forward? resp))
           (speed (Z21:get-loco-info-speed resp)))
      (if fwd? speed (- speed)))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-loco-speed! TRAIN-ID SPEED)                                           ;;
;;                                                                            ;;
;; Sets the speed of the train with id TRAIN-ID to SPEED. Setting the         ;;
;; speed to a negative number will make the train go backwards.               ;;
;; The speed can be between -127 and 127.                                     ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-loco-speed! 'T-1 100)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-loco-speed! id speed)
  (let* ((addr  (dict-ref TRAINS id))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         (fwd?  (> speed 0))
         (range Z21:high-speed-range)
         (speed (abs speed))
         (msg   (Z21:make-set-loco-drive-msg lsb msb range fwd? speed)))
    (Z21:send-msg msg)))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-loco-detection-block TRAIN-ID)                                        ;;
;;                                                                            ;;
;; Returns the id of the detection block that is currently occupied.          ;;
;; If no detection blocks are occupied, this will return #f.                  ;;
;; Note that the id is ignored: the hardware does not allow you to know which ;;
;; train is on which detection block!                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-loco-detection-block 'T-1)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-loco-detection-block id)
  (let* ((msg   (Z21:make-rmbus-get-data-msg "00"))
         (time  (current-inexact-milliseconds))
         (type? Z21:is-rmbus-datachanged-msg?))
    (Z21:send-msg msg)
    (let loop ((data (Z21:get-rmbus-data (Z21:get-msg type? time))))
      ; data is e.g. '((10) (9) (8) (7) (6) (5) (4) (3) (2) (1 5)) if there's
      ; a train on detection-block 1-5.
      (if (null? data)
          #f
          ; Deconstruct e.g. '(1 5) into module 1, occups '(5)
          (let ((module (number->string (Z21:get-module (car data))))
                (occups (Z21:get-occupancies (car data))))
            (if (null? occups)
                (loop (cdr data))
                ; Only the first result is returned, as the expected return
                ; value of this function is only one detection block.
                (string->symbol (~a module "-" (car occups)))))))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; Returns the ids of the detection blocks that are currently occupied.       ;;
;; If no detection blocks are occupied, this will return '().                 ;;
;; Note: a train may occupy two detection blocks as it is passing from one to ;;
;; the next.                                                                  ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-occupied-detection-blocks)                                            ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-occupied-detection-blocks)
  (let* ((msg   (Z21:make-rmbus-get-data-msg "00"))
         (time  (current-inexact-milliseconds))
         (type? Z21:is-rmbus-datachanged-msg?))
    (Z21:send-msg msg)
    (let ((data (Z21:get-rmbus-data (Z21:get-msg type? time))))
      ; data is e.g. '((10) (9) (8) (7) (6) (5) (4) (3) (2 1) (1 5 6)) if
      ; there are trains on blocks 2-1, 1-5, and 1-6.
      (foldl (lambda (module-occups result)
               (let* ((module (Z21:get-module module-occups))
                      (occups (Z21:get-occupancies module-occups))
                      (symbls (map (lambda (occup)
                                     (string->symbol (~a module "-" occup)))
                                   occups)))
                 (append symbls result)))
             '()
             data))))

;; ========================================================================== ;;
;;                                                                            ;;
;; (set-switch-position! SWITCH-ID POSITION)                                  ;;
;;                                                                            ;;
;; Sets the switch with id SWITCH-ID to POSITION. Only 1 and 2 are valid      ;;
;; position numbers. Consult opstelling_schema.pdf to get an overview of all  ;;
;; switch positions. Consult railway.rkt for an overview of all switch ids.   ;;
;;                                                                            ;;
;; When switching the same switch back, you need to wait at least 3 seconds.  ;;                                                                            ;;
;; Example usage:                                                             ;;
;; (set-switch-position! 'S-9 2)                                              ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (set-switch-position! id position)
  (let* ((addr  (dict-ref SWITCHES id))
         (lsb   (byte->hex-string addr))
         (msb   "00")
         (msg   (Z21:make-set-switch-msg lsb msb #t position)))
    (Z21:send-msg msg)))


;; ========================================================================== ;;
;;                                                                            ;;
;; (get-switch-position SWITCH-ID)                                            ;;
;;                                                                            ;;
;; Returns the position of the switch with id SWITCH-ID. Consult railway.rkt  ;;
;; for an overview of all switch ids.                                         ;;
;;                                                                            ;;
;; Example usage:                                                             ;;
;; (get-switch-position 'S-9)                                                 ;;
;;                                                                            ;;
;; ========================================================================== ;;

(define (get-switch-position id)
  (let*  ((addr  (dict-ref SWITCHES id))
          (lsb   (byte->hex-string addr))
          (msb   "00")
          (msg   (Z21:make-get-switch-info-msg lsb msb))
          (time  (current-inexact-milliseconds))
          (type? Z21:is-switch-info-msg?))
    (Z21:send-msg msg)
    (Z21:get-switch-info-position (Z21:get-msg type? time))))

;;; UNEXPORTED ;;;

(define socket #f)

(define messages '())

(define (Z21:send-msg msg)
  (Z21:send socket msg)
  (sleep 0.1))

(define (Z21:handle-msg msg)
  (define time (current-inexact-milliseconds))
  (set! messages (cons (cons time msg) messages)))

(define (Z21:get-msg type? request-time)
  (define time car)
  (define content cdr)
  (let loop ((messages messages))
    (if (or (null? messages) (> request-time (time (car messages))))
        (Z21:get-msg type? request-time)
        (if (type? (cdar messages))
            (cdar messages)
            (loop (cdr messages))))))
