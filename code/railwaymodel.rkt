#lang racket


;;; RAILWAY MODEL ;;;
;; ADT for a railway model, which can be read from
;; a text file.

; (require ...)

(provide (struct-out position)
         (struct-out loco)
         mk-loco

         (struct-out node)
         (struct-out switch)
         (struct-out track)
         (struct-out detection-block)
         (struct-out rwm)
         load-rwm
         track-eqv?
         find-db)


;;; LOCOMOTIVE ;;;

; A position is represented a track (n1 and n2
; must correspond to a track but the direction may
; be different), and the distance from n1 on the
; track.
(struct position (n1 n2 distance))

; A locomotive is represented by an ID, a position
; and a speed.
; L
(struct loco (id [position #:mutable] [speed #:mutable]))

(define (mk-loco lid n1 n2)
  (loco lid (position n1 n2 0) 0))



;;; RAILWAY MODEL ;;;

; N
(struct node (id x y))
; S
(struct switch (middle-node n0 n1 n2 [mode #:mutable]))
; T
(struct track (n1 n2))
; D
(struct detection-block (id track))

; A railway model is composed of 5 lists:
; locomotives, nodes, swithes, tracks, and
; detection blocks.
(struct rwm (ls ns ss ts ds))

; Reads a railway model from a text file.
(define (load-rwm filename)
  (let ([lines (map string-split (file->lines filename))]
        [ls '()]
        [ns '()]
        [ss '()]
        [ts '()]
        [ds '()])
    (for-each
     (lambda (l)
       (case (string->symbol (car l))
         [(L) (let* ([lid (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (mk-loco lid n1 n2)])
                (set! ls (cons res ls)))]
         [(N) (let* ([id (string->symbol (list-ref l 1))]
                     [x (string->number (list-ref l 2))]
                     [y (string->number (list-ref l 3))]
                     [res (node id x y)])
                (set! ns (cons res ns)))]
         [(S) (let* ([nm (string->symbol (list-ref l 1))]
                     [n0 (string->symbol (list-ref l 2))]
                     [n1 (string->symbol (list-ref l 3))]
                     [n2 (string->symbol (list-ref l 4))]
                     [res (switch nm n0 n1 n2 1)])
                (set! ss (cons res ss)))]
         [(T) (let* ([n1 (string->symbol (list-ref l 1))]
                     [n2 (string->symbol (list-ref l 2))]
                     [res (track n1 n2)])
                (set! ts (cons res ts)))]
         [(D) (let* ([id (string->symbol (list-ref l 1))]
                     [n1 (string->symbol (list-ref l 2))]
                     [n2 (string->symbol (list-ref l 3))]
                     [res (detection-block id (track n1 n2))])
                (set! ds (cons res ds)))]))
     lines)
    (rwm ls ns ss ts ds)))

; (define rwm-be (load-rwm "be_simple.txt"))


; Checks whether two tracks are equivalent.
(define (track-eqv? t1 t2)
  (or (and (eqv? (track-n1 t1) (track-n1 t2))
           (eqv? (track-n2 t1) (track-n2 t2)))
      (and (eqv? (track-n1 t1) (track-n2 t2))
           (eqv? (track-n2 t1) (track-n1 t2)))))

; Find a detection block in a railway model.
(define (find-db rwm n1 n2)
  (define d (findf (lambda (db)
                     (let ([t1 (track n1 n2)]
                           [t2 (detection-block-track db)])
                       (track-eqv? t1 t2)))
                   (rwm-ds rwm)))
  (if d
      (detection-block-id d)
      #f))
