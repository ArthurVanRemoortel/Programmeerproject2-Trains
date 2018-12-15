#lang racket
(require racket/gui/base
         racket/udp)
(require (prefix-in sim: "interface.rkt")
         "railway.rkt"
         "helpers.rkt"
         "railway-search.rkt"
         "configuration.rkt")
(require (prefix-in hardware: "interface/interface.rkt"))

(provide INFRABEL%)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; 
;;                               INFRABEL                                           ;;                  
;;  INFRABEL has three  functions:                                                  ;;                                       
;;       1) Communication with the hardware/simulator.                              ;;                         
;;       2) Controlling the locos, switches and signalisation to avoid collisions.  ;;
;;       3) Moving the locos so they reach their destinations.                      ;;
;;  When INFRABEL runs on a seperate device from NMBS, it becomes a server and      ;;
;;  communicates with NMBS using TCP. Every time INFRABEL changes the railway it    ;;
;;  also needs to notify NMBS about that change so they remain synchronised.        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define INFRABEL%
  (class object%
    (super-new)
    
    (define railway (make-object railway%))
    (define tcp-listener (tcp-listen TCP-PORT))
    (define client-in #f)
    (define client-out #f)
    (set!-values (client-in client-out) (tcp-accept tcp-listener))

    ;; Abstraction between INFRABEL and the hardware/simulator.
    (define hardware-connection (make-object hardware-connection% hardware-mode))

    (define/public (get-loco-speed loco)
      ((hardware-connection 'get-loco-speed) (send loco get-id)))
    
    (define/public (set-loco-speed! loco speed)
      (send loco set-speed! speed)
      (send hardware-connection set-loco-speed! (send loco get-id) speed)
      (send-message-to-NMBS (list 'set-loco-speed! (send loco get-id) speed)))

    (define/public (get-switch-position switch)
      (send hardware-connection get-switch-position (send (send switch get-middle-node) get-id)))

    (define/public (set-switch-position! switch position)
      (send hardware-connection set-switch-position! (send (send switch get-middle-node) get-id) position)
      (send switch set-position! position)
      (send-message-to-NMBS (list 'set-switch-position! (send switch get-id) position)))

    (define/public (get-loco-detection-block loco)
      (send hardware-connection get-loco-detection-block (send loco get-id)))
    
    (define (set-signal! block new-color)
      (define original-color (send block get-signal-color))
      (send block set-signal-color! new-color)
      (send-message-to-NMBS (list 'set-signal-color! (send block get-id) new-color)))


    (define (listener)
      (let ((message (read client-in)))
        (define command message)
        (define function (car command))
        (define args (cdr command))
        ;; Evaluate the incomming commands.
        (cond [(eq? function 'set-loco-speed!)
               (define loco-id (car args))
               (define loco (send railway get-loco loco-id))
               (define speed (cadr args))
               (set-loco-speed! loco speed)]
                
              [(eq? function 'set-switch-position!)
               (define switch-id (car args))
               (define switch (send railway get-switch switch-id))
               (define pos (cadr args))
               (set-switch-position! switch pos)]

              [(eq? function 'set-loco-path!)
               (define loco-id (car args))
               (define loco (send railway get-loco loco-id))
               (define start-block-id (cadr args))
               (define end-block-id (caddr args))
               (define instruction-list-with-ids (cadddr args))
               (define breakpoints-list-with-ids (list-ref args 4))
               ;; Convert the ID's in the instruction-list-with-ids with the segments they represent.
               (define instruction-list (map
                                         (lambda (item)
                                           (cond [(member (car item) '(reverse speed end))
                                                  item]
                                                 [(eq? 'switch (car item))
                                                  (list 'switch (send railway get-switch (cadr item)) (caddr item))]
                                                 [(eq? 'track (car item))
                                                  (list 'track (send railway get-track (cadr item)))]
                                                 [(eq? 'detection-block (car item))
                                                  (list 'detection-block (send railway get-detection-block (cadr item)))]))
                                         instruction-list-with-ids))
               (define path (make-object path% railway start-block-id end-block-id loco))
               (send path load-instructions! instruction-list)
               (when (not (null? breakpoints-list-with-ids))
                 (for ([breakpoint-args breakpoints-list-with-ids])
                   (send path insert-break!
                         (send railway get-detection-block (car breakpoint-args))
                         (send railway get-loco (cadr breakpoint-args))
                         (send railway get-detection-block (caddr breakpoint-args)))))
               (send loco set-path! path)]
              [else (error "INFRABEL - Unknown command.  ~a" message)])))

    
    (define/public (gameloop)
      (let* ([wait-per-frame 30]
             [previous-time (current-inexact-milliseconds)]
             [delta-time 0]
             [game-loop-timer '()])
        
        (define (loop)
          (set! delta-time (- (current-inexact-milliseconds) previous-time))
          (set! previous-time (current-inexact-milliseconds))
          (move-locos!)
          (send game-loop-timer start wait-per-frame))
        
        (set! game-loop-timer
              (new timer% [notify-callback loop]
                   [interval wait-per-frame]
                   [just-once? #t]))))

    (define (conform-loco-to-block-signal! block loco)
      ;; Changes the behavious of a loco depending on the block it's positionned on.
      (cond [(is-red? block)  ; The loco moved to a red block and stop the loco. This shoul donly happen when controlling the locos manually.
             (when (null? (send loco get-path))
               (set-loco-speed! loco 0))]
            [(is-green? block)   ; The loco moved to a green block. Let the loco continue.
             (define loco-path (send loco get-path))
             (define speed-sign
               (if (= (send loco get-speed) 0)
                   (sgn (send loco get-last-speed))
                   (sgn (send loco get-speed))))
             (when (and (= speed-sign 0) (not (null? loco-path)))
               (set! speed-sign (sgn (send loco-path get-dirrection))))
             (if (not (null? loco-path))
                 (if (eq? (send loco-path get-end-block) (send loco-path get-first-block-after block))
                     (set-loco-speed! loco (* speed-sign 1.5))  ; The loco is close to the destination. Slow it down.
                     (set-loco-speed! loco (* speed-sign 2.5)))
                 (set-loco-speed! loco (* speed-sign 2.5)))] ; Let the loco move at full speed.
              
            [(is-orange? block) ; The loco is to close to another loco. Stop the loco untill the block is green again.
             (set-loco-speed! loco 0)]))


    (define (find-dangerous-blocks loco) ;; TODO: Also call this when setting loco speed manually.
      (define loco-path (send loco get-path))
      (define current-loco-block (send loco get-current-detection-block))
      (define dangerous-blocks '())
        
      (let ((current-loco-block (send loco get-current-detection-block))
            (loco-next-blocks (send loco get-next-detection-blocks)))
        (define other-locos (remove loco (hash-map (send railway get-locos) (lambda (id l) l))))
        (for ([other-loco other-locos])
          (define dangerous-blocks-temp '())
          (define other-loco-current-block (send other-loco get-current-detection-block))
          (define other-loco-path (send other-loco get-path))
          (define other-loco-neighboring-blocks (send other-loco get-nearest-detection-blocks))
          (define potentially-dangerous-block (set-intersect loco-next-blocks other-loco-neighboring-blocks))
          (if (not (null? loco-path))
              (begin ;; Smart Checks
                (let ((path-next-block (send loco-path get-first-block-after current-loco-block)))
                  (if (eq? path-next-block other-loco-current-block)
                      (set! dangerous-blocks-temp (cons current-loco-block dangerous-blocks-temp))
                      (when (and (member path-next-block other-loco-neighboring-blocks)
                                 (send loco-path is-block-in-path? other-loco-current-block))
                        (set! dangerous-blocks-temp (cons path-next-block dangerous-blocks-temp))))))
              (set! dangerous-blocks-temp potentially-dangerous-block))
          (set! dangerous-blocks (append dangerous-blocks dangerous-blocks-temp))))
      dangerous-blocks)

    (define (set-dangerous-block-to-orange! loco dangerous-blocks)
      ;; Sets blocks to orange, unless a block is the final destination of a loco. Setting such a block to orange would not be necessary.
      (define loco-path (send loco get-path))
      (for-each (lambda (next-block)
                  (cond [(not (null? loco-path))
                         (define loco-final-block (send loco-path get-end-block))
                         (when (not (eq? loco-final-block next-block))
                           (set-signal! next-block 'orange))]
                        [else
                         (set-signal! next-block 'orange)]))
                dangerous-blocks))

    (define/public (move-locos!)
      ;; Moves the trains.
      (for ([(id loco) (send railway get-locos)])
        (define hardware-loco-block-id (get-loco-detection-block loco))
        (when hardware-loco-block-id
          (let ([hardware-loco-block (send railway get-detection-block hardware-loco-block-id)] ;; The block detected on the hardware. Always equals the location of a loco.
                [current-loco-block (send loco get-current-detection-block)]                    ;; The block stored in the loco object. Not always the actual location.
                [loco-path (send loco get-path)])
            
            ;; The loco is on a new block.
            (when (and hardware-loco-block (not (eq? current-loco-block hardware-loco-block)))
              (conform-loco-to-block-signal! hardware-loco-block loco)     
              (define old-loco-block current-loco-block)
              (send old-loco-block remove-loco! loco)
              (send hardware-loco-block set-loco! loco)
              (send loco set-current-detection-block! hardware-loco-block)
              (set! current-loco-block hardware-loco-block)
              (when (is-green? current-loco-block)
                (set-signal! current-loco-block 'red))
              ;(when raspberry-pi?
              (send-message-to-NMBS (list 'set-current-detection-block! (send loco get-id) hardware-loco-block-id))
              (when (is-red? old-loco-block)
                (set-signal! old-loco-block 'green))

              ;; Set dangerous blocks to orange.
              (let ((dangerous-blocks (find-dangerous-blocks loco)))
                (when (not (null? dangerous-blocks))
                  (set-dangerous-block-to-orange! loco dangerous-blocks)))

              ;; Set orange blocks back to green is possible, and restart waiting locos.
              (when (not (is-orange? current-loco-block))
                (for-each (lambda (previous-neighboring-block)
                            (when (is-orange? previous-neighboring-block) ;; TODO: Always use is-color? abstraction.
                              (set-signal! previous-neighboring-block 'green)
                              (define loco-on-orange-block (send previous-neighboring-block get-loco))
                              (when loco-on-orange-block
                                ;; Before restarting a loco, verify if the next blocks have to be orange.
                                (define loco-on-orange-block-path (send loco-on-orange-block get-path))
                                (let ((dangerous-blocks (find-dangerous-blocks loco-on-orange-block)))
                                  (when (not (null? dangerous-blocks))
                                    (set-dangerous-block-to-orange! loco-on-orange-block dangerous-blocks)))
                                (when (not (null? loco-on-orange-block-path))
                                  (for ([next (cdr (send loco-on-orange-block-path get-remaining-path))])
                                    #:break (is-detection-block? (car next))
                                    (when (is-switch? (car next))
                                      (define switch (car next))
                                      (set-switch-position! (car next) (cadr next)))))
                                (conform-loco-to-block-signal! (send loco-on-orange-block get-current-detection-block) loco-on-orange-block))))
                          (send (send loco get-previous-detection-block) get-nearest-detection-blocks))))

            ;; Automatically move locos and toggle switches so they follow their paths.
            (when (not (null? loco-path))
              (define current-instruction (send loco-path get-current))
              (define current-breakpoint (send loco-path get-current-breakpoint))
              (define instruction-type (car current-instruction))
              (cond [(and (eq? instruction-type 'speed) (null? current-breakpoint)) ;; Start the movement of a loco.
                     (define speed (cadr current-instruction))
                     (if (not (= 0 speed))
                         (begin (if (eq? (send loco-path get-end-block) (send loco-path get-first-block-after hardware-loco-block))
                                    (set! speed (* (sgn (send loco-path get-dirrection)) 1.7))
                                    (set! speed (cadr current-instruction)))
                                (let ((dangerous-blocks (find-dangerous-blocks loco)))
                                  (when (not (null? dangerous-blocks))
                                    (set-dangerous-block-to-orange! loco dangerous-blocks)))
                                (if (not (is-orange? hardware-loco-block))
                                    (set-loco-speed! loco speed)
                                    (conform-loco-to-block-signal! hardware-loco-block loco)))
                         (set-loco-speed! loco speed))
                     (send loco-path move!)
                     (set! current-instruction (send loco-path get-current))
                     (set! instruction-type (car current-instruction))]
                    
                    [(and (eq? instruction-type 'reverse) (null? current-breakpoint)) ;; Reverse a loco.
                     (define new-speed (- (send loco get-speed)))
                     (send loco-path set-dirrection! (sgn new-speed))
                     (if (eq? (send loco-path get-end-block) (send loco-path get-first-block-after hardware-loco-block))
                         (set! new-speed (* (sgn new-speed) 1.7))
                         (set! new-speed new-speed))
                     (when (not (is-orange? hardware-loco-block))
                       (set-loco-speed! loco new-speed))
                     (send loco-path move!)
                     (set! current-instruction (send loco-path get-current))]
                    [(eq? instruction-type 'end)     ;; Stop a loco.
                     (send loco set-path! null)
                     (send-message-to-NMBS (list 'delete-path (send loco get-id)))])
              
              ;; Evaluate other instructions.
              (cond [(or (is-switch? instruction-type) (is-track? instruction-type))
                     (when (not (is-orange? hardware-loco-block))
                       ;; Don't do anything, just continue. 
                       (send loco-path move!))]
                    [(is-detection-block? instruction-type)
                     ;; Whenever a loco has reached a detection block.
                     ;; 1) If a loco is waiting at a breakpoint, check if the breakpoint has been removed.
                     ;; 2) Check if there is a breakpoint on that block, Stop the train if this is the case.
                     ;; 3) Set all switches between this block and the next block in the correct position.
                     (when (eq? instruction-type current-loco-block)
                       (define reached-new-breakpoint (send loco-path reached-breakpoint?))
                       (cond [(not (null? current-breakpoint))
                              (when (send loco-path finished-current-breakpoint?)
                                ;; Restart a loco if the breakpoint is deactivated.
                                (set-signal! (send loco get-current-detection-block) 'green)
                                (conform-loco-to-block-signal! (send loco get-current-detection-block) loco)
                                (set! current-breakpoint null)
                                (let ((dangerous-blocks (find-dangerous-blocks loco)))
                                  (when (not (null? dangerous-blocks))
                                    (set-dangerous-block-to-orange! loco dangerous-blocks)))
                                ;; Make sure the next switches are on the corrent position.
                                (for ([next (cdr (send loco-path get-remaining-path))])
                                  #:break (is-detection-block? (car next))
                                  (when (is-switch? (car next))
                                    (define switch (car next))
                                    (set-switch-position! (car next) (cadr next))))
                                (send loco set-speed! (* 2.5 (sgn (send loco-path get-dirrection))))
                                (send loco-path move!))]
                             
                             ;; Verify if a loco has reached a breakpoint and stop the loco if this is true.
                             [reached-new-breakpoint
                              (set-loco-speed! loco 0)
                              (set! current-breakpoint reached-new-breakpoint)]
                               
                             [else
                              ;; Make sure the next switches are on the correct position.
                              (define switch-instructions '())
                              (for ([next (cdr (send loco-path get-remaining-path))])
                                #:final (is-detection-block? (car next))
                                (cond [(is-detection-block? (car next))
                                       (define block (car next))
                                       (define item-after-block (send loco-path get-item-after block))
                                       (when (and (not (is-orange? hardware-loco-block)) (eq? 'reverse (car item-after-block)))
                                         (set-loco-speed! loco (if (< (send loco get-speed) 0) -1.7 1.7)))]
                                      [(is-switch? (car next))
                                       (set! switch-instructions (cons next switch-instructions))]))
                              (for-each (lambda (switch-instruction) (set-switch-position! (car switch-instruction) (cadr switch-instruction)))
                                        (sort switch-instructions (lambda (a b) (symbol<? (send (car a) get-id) (send (car b) get-id)))))
                                        
                              (send loco-path move!)]))]))))))

    (define/public (send-message-to-NMBS msg)
      (write msg client-out)
      (flush-output client-out))
    
    (if (eq? hardware-mode 'hardware)
        (begin 
          (hardware:start-simulator) ;; Creates the socket to communicate with the real hardware, not the simulator.
          (for ([(id switch) (send railway get-switches)]) ;; Make sure the positions of the hardware match the ones loaded in the railway ADT.
            (set-switch-position! switch (send switch get-position))))
        (sim:start-simulator))

    (gameloop)   ;; Start the gameloop that moves the locos.
    (thread      ;; Seperate thread that listens to the TCP connection.
     (lambda ()
       (let loop ()
         (sleep 0.1)
         (listener)
         (loop))))))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;                           Hardware Connection                                   ;;
;;    handles the communication between INFRABEL and the Hardware/Simulator        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define hardware-connection%
  (class object%
    (super-new)
    (init-field hardware-mode)

    (define is-hardware? (eq? hardware-mode 'hardware))
    ;; This application and the Z21-hardware use different units of speed.
    (define speed-multiplier (if is-hardware? hardware-speed-multiplier 1))

    (define/public (get-loco-speed loco-id)
      (if is-hardware?
          (hardware:get-loco-speed loco-id)
          (sim:get-loco-speed loco-id)))

    (define/public (set-loco-speed! loco-id speed)
      (if is-hardware?
          (hardware:set-loco-speed! loco-id (inexact->exact (floor (* speed speed-multiplier))))
          (sim:set-loco-speed! loco-id speed)))

    (define/public (get-switch-position switch-id)
      (if is-hardware?
          (hardware:get-switch-position switch-id)
          (sim:get-switch-position switch-id)))

    (define/public (set-switch-position! switch-id pos)
      (if is-hardware?
          (hardware:set-switch-position! switch-id pos)
          (sim:set-switch-position! switch-id pos)))

    (define/public (get-loco-detection-block block-id)
      (if is-hardware?
          (hardware:get-loco-detection-block block-id)
          (sim:get-loco-detection-block block-id)))))


                                    
