#lang racket

(require racket/gui/base
         "INFRABEL.rkt"
         "railway.rkt"
         "helpers.rkt"
         "GUI.rkt"
         "configuration.rkt")

(provide NMBS%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;                               NMBS                                     ;;                  
;;  NMBS has two primary functions:                                       ;;                                       
;;       1) Calculating train routes.                                     ;;                         
;;       2) Providen a GUI so the user can interract with the trains.     ;;                                                          
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define NMBS%
  (class object%
    (super-new)
    (define railway (make-object railway%))
    (define INFRABEL-connection (make-object INFRABEL-connection% this hardware-mode railway))
    
    (define/public (gameloop)
      (let* ([wait-per-frame 30]
             [previous-time (current-inexact-milliseconds)]
             [delta-time 0]
             [game-loop-timer '()])
        (define (loop)
          (set! delta-time (- (current-inexact-milliseconds) previous-time))
          (set! previous-time (current-inexact-milliseconds))
          (send game-loop-timer start wait-per-frame))

        (set! game-loop-timer
              (new timer% [notify-callback loop]
                   [interval wait-per-frame]
                   [just-once? #t]))))

    (define/public (make-path loco-id dest-id)
      ;; Creates the path that a loco should follow to reach a destination.
      ;; This functions is not the search algorithm. The algorithm can be found in the railway-search.rkt file.
      ;; This functions also adds breakpoint to a path. See the documentation for more details on breakpoints.
      (define loco (send railway get-loco loco-id))
      (define loco-detection-block-id (send (send (send railway get-loco loco-id) get-current-detection-block) get-id))
      (cond [(and loco-detection-block-id (null? (send loco get-path))) ;; You can only calculate a new path if that loco does not have a path already.
             (define path-instructions (send railway find-path loco-detection-block-id dest-id))
             (define path (make-object path% railway loco-detection-block-id dest-id loco))
             (send path load-instructions! path-instructions)
             (for ([(lid other-loco) (send railway get-locos)])
               (define other-loco-path (send other-loco get-path))
               (define intersection (if (null? other-loco-path) #f (send path intersects? other-loco-path)))
               (when (and intersection (not disable-breakpoints))
                 (printf "Intersects: Setting a breakpoint for ~a at ~a \n" (send loco get-id) (send intersection get-id))
                 (send path insert-break!
                       intersection    ;; The point where the intersection happens.
                       other-loco      ;; The other loco with witch the intersection happens.
                       (send (send other-loco get-path) get-end-block))
                 ))
             (send path finish!)
             (send INFRABEL-connection set-loco-path! loco path)]
            [else (println "Can not make a path for this loco at this time. Try again later.")]))
    
    (define/public (get-railway) railway)
    (define/public (get-INFRABEL-connection) INFRABEL-connection)
    (define/public (start-gameloop) (gameloop))

    ;; Creates a seperate thread to update the GUI.
    (thread
     (lambda ()
       (define GUI (make-object GUI% INFRABEL-connection this))
       (let loop ()
         (sleep 0.5)
         (send GUI update)
         (loop))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;                               INFRABEL-connection                           ;;
;;  Is a part of NMBS that handes the communication with INFRABEL.             ;;                                   
;;       1) Calculating train routes.                                          ;;                         
;;       2) Providen a GUI so the user can interract with the trains.          ;;
;;                                                                             ;;
;;  If INFRABEL and NMBS are running on the same device, INFRABEL-connection   ;;
;;  creates the INFRABEL object and directly acesses it's methods.             ;;
;;                                                                             ;;
;;  If INFRABEL runs on a different device, INFRABEL-connection/NMBS behaves   ;;
;;  like a client and communicates with INFRABEL using TCP.                    ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INFRABEL-connection%
  (class object%
    (super-new)
    (init-field NMBS)
    (init-field hardware-mode)
    (init-field railway)
    
    (define server-in #f)
    (define server-out #f)
    (set!-values (server-in server-out) (tcp-connect INFRABEL-IP TCP-PORT))
    
    (define/public (get-loco-speed loco)
      ; Does not actually communicate with INFRABEL/Hardware.
      ; The loco speed stored in the local loco object should always be synchronised with the actual speed.  
      (send loco get-speed))

    (define/public (set-loco-speed! loco speed)
      (send loco set-speed! speed)
      (send-message-to-INFRABEL (list 'set-loco-speed! (send loco get-id) speed)))

    (define/public (get-switch-position switch)
      (send switch get-positions))

    (define/public (set-switch-position! switch pos)
      (send switch set-position! pos)
      (send-message-to-INFRABEL (list 'set-switch-position! (send switch get-id) pos)))

    (define/public (get-loco-detection-block loco)
      (send loco get-current-detection-block))

    (define/public (set-loco-path! loco path)
      (send loco set-path! path)
      (define instruction-list (send path get-instructions-list))
      (define instruction-list-with-ids (objects->ids instruction-list))
      (define breakpoints-list-with-ids (deep-map
                                         (lambda (obj) (send obj get-id))
                                         (send path get-breakpoints)))
      (define msg (list 'set-loco-path!
                        (send loco get-id)
                        (send (send path get-start-block) get-id)
                        (send (send path get-end-block) get-id)
                        instruction-list-with-ids
                        breakpoints-list-with-ids))
        (send-message-to-INFRABEL msg))

    (define (send-message-to-INFRABEL msg)
      (write msg server-out)
      (flush-output server-out))

    (define (connection-listener)
      ;; Listens to the TCP connection and evaluates the commands comming from INFRABEL.
      ;; These commands are required to keep INFRABEL and and NMBS synchronised.
      (let ((message (read server-in)))
        (define function (car message))
        (define args (cdr message))
        (cond [(eq? function 'set-current-detection-block!)
               (define loco-id (car args))
               (define block-id (cadr args))
               (define block (send railway get-detection-block block-id))
               (define loco (send railway get-loco loco-id))
               (send loco set-current-detection-block! block)]
              [(eq? function 'set-signal-color!)
               (define block-id (car args))
               (define color (cadr args))
               (define block (send railway get-detection-block block-id))
               (send block set-signal-color! color)]
              [(eq? function 'set-switch-position!)
               (define switch-id (car args))
               (define pos (cadr args))
               (define switch (send railway get-switch switch-id))
               (send switch set-position! pos)]
              [(eq? function 'set-loco-speed!)
               (define loco-id (car args))
               (define speed (cadr args))
               (define loco (send railway get-loco loco-id))
               (send loco set-speed! speed)]
              [(eq? function 'delete-path)
               (define loco-id (car args))
               (define loco (send railway get-loco loco-id))
               (send loco set-path! null)]
              [else (error "INFRABEL-connection received an unknown command: " function)])))

    ;; Start a new thread that listens to the TCP connection.
    (thread
     (lambda ()
       (let loop ()
         (sleep 0.1)
         (connection-listener)
         (loop))))))
