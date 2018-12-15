#lang racket
(provide
 pythagoras
 coordinate%
 calculate-center
 collision?
 append-item
 objects->ids
 deep-map
 is-orange? is-red? is-green? is-node? is-track? is-detection-block? is-switch? is-loco? symbols-list->symbol get-segment-between)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;   Various helper functions used throughout the application.        ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; Coordinate abstraction mostly used by the gui.
(define coordinate%
  (class object%
    (super-new)
    (init-field x y)
    (define/public (get-x) x)
    (define/public (get-y) y)))

;; Coordinate the point centered between two other points.
(define (calculate-center point1 point2)
  (let* ((x1 (send point1 get-x))
         (y1 (send point1 get-y))
         (x2 (send point2 get-x))
         (y2 (send point2 get-y))
         (center-x (+ x1 (/ (- x2 x1) 2)))
         (center-y (+ y1 (/ (- y2 y1) 2))))
    (make-object coordinate% center-x center-y)))

;; Are two point positioned within a margin?
(define (collision? point1 point2 precision)
  (let ((x1 (send point1 get-x))
        (y1 (send point1 get-y))
        (x2 (send point2 get-x))
        (y2 (send point2 get-y)))
      (and (< (abs (- x1 x2)) precision)
           (< (abs (- y1 y2)) precision))))

(define (append-item lst item)
  (append lst (list item)))

;; Converts a list of symbols to a single symbol.
(define (symbols-list->symbol symbols)
  (string->symbol (apply string-append (map symbol->string symbols))))

;; Various function used with detection blocks. 
(define (is-red? block) (eq? (send block get-signal-color) 'red))
(define (is-green? block) (eq? (send block get-signal-color) 'green))
(define (is-orange? block) (eq? (send block get-signal-color) 'orange))

(define (is-node? obj) (and (object? obj) (eq? (send obj get-tag) 'node)))
(define (is-track? obj) (and (object? obj) (eq? (send obj get-tag) 'track)))
(define (is-detection-block? obj) (and (object? obj) (eq? (send obj get-tag) 'detection-block)))
(define (is-switch? obj) (and (object? obj) (eq? (send obj get-tag) 'switch)))
(define (is-loco? obj) (and (object? obj) (eq? (send obj get-tag) 'loco)))

(define (get-segment-between n1 n2)
    (define n1-connections (send n1 get-connections))
    (define n2-connections (send n2 get-connections))
    (car (set-intersect n1-connections n2-connections)))

(define (pythagoras p1 p2)
  (sqrt (+ (abs (- (send p1 get-x) (send p2 get-x))) (abs (- (send p1 get-y) (send p2 get-y))))))

;; Converts all the segments in a (nested) list with their id's. 
(define (objects->ids lst)
  (map (lambda (item)
         (cond [(list? item)
                (define type (car item))
                (cond
                  [(eq? 'reverse type)
                   (list 'reverse)]
                  [(eq? 'speed type)
                   (list 'speed (cadr item))]
                  [(eq? 'end type)
                   (list 'end)]
                  [(is-switch? type)
                   (list 'switch (send type get-id) (cadr item))]
                  [(is-detection-block? type)
                   (list 'detection-block (send type get-id))]
                  [(is-track? type)
                   (list 'track (send type get-id))])]
               [else item]))
       lst))

(define (deep-map f l)
  (define (deep x)
    (cond ((null? x) x)
          ((pair? x) (map deep x))
          (else (f x))))
  (map deep l))



