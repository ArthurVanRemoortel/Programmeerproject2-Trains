#lang racket


;;; INTERFACE ;;;
;; The interface could employ a simplified and
;; slightly different version of the Z21 API.

(require "simulation.rkt"
         (rename-in "railwaymodel.rkt"
                    (set-loco-speed! setlocospeed!)))

(provide start-simulator
         stop-simulator

         all-locos
         get-loco-speed
         set-loco-speed!
         get-loco-detection-block

         get-switch-position
         set-switch-position!

         model
         )


;;; UTILITIES ;;;

(define (rand-elt l)
  (list-ref l (random (length l))))

(define (check-model)
  (unless model
    (error "Model not initialized.")))


;;; SIMULATOR ;;;

(define (start-simulator)
  (displayln "Starting simulator...")
  (start)
  (displayln "Simulator is running."))

(define (stop-simulator)
  (displayln "Stopping simulator...")
  (stop)
  (displayln "Simulator has stopped."))


;;; LOCOMOTIVES ;;;

; Dictionary would be more efficient.
(define (get-loco lid mdl)
  (define loco #f)
  (for-each (lambda (l)
              (when (eqv? (loco-id l) lid)
                (set! loco l)))
            (rwm-ls mdl))
  (unless loco
    (error "No locomotive with ID" lid))
  loco)

; Returns a list of all locomotive ID's.
(define (all-locos)
  (check-model)
  (map loco-id (rwm-ls model)))

; Returns the loco's speed (in "km/h", but not
; really...), given the loco ID. Negative speed
; means the loco is going backwards.
(define (get-loco-speed lid)
  (check-model)
  (loco-speed (get-loco lid model)))

; Set the loco's speed.
(define (set-loco-speed! lid speed)
  (check-model)
  (setlocospeed! (get-loco lid model) speed))

; Returns the detection block on which the
; locomotive is beeing detected. Returns '|-1| if
; the locomotive is not on a detection block.
(define (get-loco-detection-block lid)
  (check-model)
  (let ([pos (loco-position (get-loco lid model))])
    (find-db model (position-n1 pos) (position-n2 pos))))

;;; SWITCH ;;;

; Get the switch position (1 or 2), given the
; switch ID.
(define (get-switch-position snode)
  (check-model)
  (define swx
    (findf (lambda (s)
             (eqv? (switch-middle-node s) snode))
           (rwm-ss model)))
  (unless swx
    (error "No switch on node" snode))
  (switch-mode swx))

; Set the switch position
(define (set-switch-position! snode pos)
  (unless (member pos '(1 2))
    (error "Not a valid switch position" pos))
  (check-model)
  (define swx
    (findf (lambda (s)
             (eqv? (switch-middle-node s) snode))
           (rwm-ss model)))
  (unless swx
    (error "No switch on node" snode))
  (set-switch-mode! swx pos))

; Traffic lights to be implemented in infrabel?

; Information concerning the railway model
; infrastructure to be re-implemented in infrabel?
