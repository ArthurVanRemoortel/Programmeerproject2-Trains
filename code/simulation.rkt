#lang racket


;;; SIMULATION ;;;
;; The simulation with threading.

(require "railwaymodel.rkt" "configuration.rkt")

(provide model

         start
         stop)



;;; MODEL ;;;

(define model #f)

(define current-thread #f)

(define running? #f)

(define loop-wait 0.01)

(define loop-virtual-time 1.0)

;;; SIMULATION ;;;

(define (start)
  (when running?
    (error "Simulation is already running."))
  (set! running? #t)
  ;(set! model (load-rwm "setup_real.txt"))
  (set! model (load-rwm layout-file))
  (set! current-thread
        (thread simul-loop)))

(define (stop)
  (unless (and running? current-thread)
    (error "Simulation is not running."))
  (set! running? #f)
  (let while ()
    (unless (thread-dead? current-thread)
      (sleep 0.1)
      (while))))

(define (simul-loop)
  (when running?
    (for-each (lambda (loco)
                (displace-loco loco))
              (rwm-ls model))
    (sleep loop-wait)
    (simul-loop)))

(define (displace-loco loco)
  (let ([forward? (>= (loco-speed loco) 0)]
        [dx (* (abs (loco-speed loco)) loop-virtual-time)])
    ; displacing to next track
    (let while ()
      (define pos (loco-position loco))
      (define tlen (track-length (track (position-n1 pos)
                                        (position-n2 pos))))
      (unless (< dx (if forward?
                        (- tlen (position-distance pos))
                        (position-distance pos)))
        (define n1 (position-n1 pos))
        (define n2 (position-n2 pos))
        (define n3 (if forward?
                       (next-node n1 n2)
                       (next-node n2 n1)))
        (unless n3
          (error "loco derailed" (loco-id loco)))
        (check-collision (loco-id loco) (track n1 n2)
                         (position-distance pos) (if forward? tlen 0))
        (set! dx (- dx (if forward?
                           (- tlen (position-distance pos))
                           (position-distance pos))))
        (define new-track (track (if forward? n2 n3)
                                 (if forward? n3 n1)))
        (set-loco-position! loco (position (track-n1 new-track)
                                           (track-n2 new-track)
                                           (if forward?
                                               0
                                               (track-length new-track))))
        (while)))
    ; displacement within track length
    (define pos (loco-position loco))
    (define d1 (position-distance pos))
    (define d2 (if forward? (+ d1 dx) (- d1 dx)))
    (check-collision (loco-id loco)
                     (track (position-n1 pos)
                            (position-n2 pos))
                     d1 d2)
    (set-loco-position! loco
                        (position (position-n1 pos)
                                  (position-n2 pos)
                                  d2))))

(define (check-collision lid track d1 d2)
  (for-each (lambda (op)
              ; check for crossing
              (when (not (= (sign (abs (- d1 op)))
                            (sign (abs (- d2 op)))))
                (error "loco collided with antother loco" lid)))
            (other-locos-track-pos track lid)))

(define (sign x)
  (cond [(> x 0) +1]
        [(< x 0) -1]
        [(= x 0) 0]
        [else (error "number without a sign" x)]))

(define (track-length track)
  (let* ([n1 (findf (lambda (n)
                      (eqv? (node-id n) (track-n1 track)))
                    (rwm-ns model))]
         [n2 (findf (lambda (n)
                      (eqv? (node-id n) (track-n2 track)))
                    (rwm-ns model))])
    ; Euclidean distance
    (sqrt (+ (expt (- (node-x n1) (node-x n2)) 2)
             (expt (- (node-y n1) (node-y n2)) 2)))))

(define (other-locos-track-pos t1 lid)
  (define others '())
  (for-each (lambda (l)
              (define pos (loco-position l))
              (define t2 (track (position-n1 pos)
                                (position-n2 pos)))
              (when (and (not (eqv? (loco-id l) lid))
                         (track-eqv? t1 t2))
                (if (eqv? (track-n1 t1) (track-n1 t2))
                    (set! others
                          (cons (position-distance pos)
                                others))
                    (set! others
                          (cons (- (track-length t2)
                                   (position-distance pos))
                                others)))))
            (rwm-ls model))
  ; in same track direction!
  others)

(define (next-node n1 n2)
  (define swx
    (findf (lambda (s)
             (eqv? (switch-middle-node s) n2))
           (rwm-ss model)))
  (cond [swx
         ; case where n2 is the center of a switch
         (cond [(eqv? n1 (switch-n0 swx))
                (case (switch-mode swx)
                  [(1) (switch-n1 swx)]
                  [(2) (switch-n2 swx)]
                  [else #f])]
               [(or (eqv? n1 (switch-n1 swx))
                    (eqv? n1 (switch-n2 swx)))
                (switch-n0 swx)]
               [else #f])]
        [else
         ; case where there can be only one next node (no switch)
         (let ([tracks '()])
           ; add tracks
           (for-each (lambda (t)
                       (when (or (eqv? (track-n1 t) n2)
                                 (eqv? (track-n2 t) n2))
                         (set! tracks (cons t tracks))))
                     (rwm-ts model))
           ; add detection-blocks
           (for-each (lambda (d)
                       (let ([t (detection-block-track d)])
                         (when (or (eqv? (track-n1 t) n2)
                                   (eqv? (track-n2 t) n2))
                           (set! tracks (cons t tracks)))))
                     (rwm-ds model))
           ; add switches
           (for-each (lambda (s)
                       (when (or (eqv? n2 (switch-n0 s))
                                 (and (eqv? n2 (switch-n1 s))
                                      (= (switch-mode s) 1))
                                 (and (eqv? n2 (switch-n2 s))
                                      (= (switch-mode s) 2)))
                         (set! tracks (cons (track n2 (switch-middle-node s)) tracks))))
                     (rwm-ss model))
           (and (= (length tracks) 2)
                ; there can be only two tracks through n2 (since its not the center of a switch)
                (begin (set! tracks (filter (lambda (t)
                                              (not (track-eqv? t (track n1 n2))))
                                            tracks))
                       #t)
                ; after removing (n1 n2), there is only one track left
                (= (length tracks) 1)
                (let ([t (car tracks)])
                  ; return n3 as the node of the track that is not n2
                  (cond [(eqv? (track-n1 t) n2)
                         (track-n2 t)]
                        [(eqv? (track-n2 t) n2)
                         (track-n1 t)]
                        [else #f]))))]))
