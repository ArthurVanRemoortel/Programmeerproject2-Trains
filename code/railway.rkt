#lang racket

(require racket/dict
         data/queue
         "configuration.rkt"
         "helpers.rkt"
         "railway-search.rkt")

(provide railway% path%)


(define node%
  (class object%
    (super-new)
    (init-field id pos)
    (define tag 'node)
    ;; Nodes don't know which segments are on which side upon initialisation.
    ;; These need to be set later using add-connection. (see load-railway-from-file).
    (define connections null)

    (define/public (add-connection segment)
      (set! connections (cons segment connections)))

    (define/public (get-next from) (remove from connections))

    (define/public (get-connections) connections)
    (define/public (get-id) id)
    (define/public (get-pos) pos)
    (define/public (get-tag) tag)
    (define/public (get-x) (send pos get-x))
    (define/public (get-y) (send pos get-y))))


(define detection-block%
  (class object%
    (super-new)
    (init-field id node1 node2)
    
    (define tag 'detection-block)
    (define connections (list node1 node2))
    (define signal-color 'green)
    (define center (calculate-center node1 node2))
    (define length (pythagoras node1 node2))
    (define nearest-detection-blocks '())
    (define loco #f)

    (define/public (set-nearest-detection-blocks! blocks)
      (set! nearest-detection-blocks blocks))

    (define/public (get-nearest-detection-blocks)
      nearest-detection-blocks)

    (define/public (set-signal-color! new-color)
      (set! signal-color new-color))

    (define/public (set-loco! l) (set! loco l))
    
    (define/public (remove-loco! l)
      (if (or (nor loco) (eq? l loco)) (set! loco #f) (error "remove-loco! error")))
    
    (define/public (is-loco-on-block? l) (eq? l loco))

    (define/public (get-loco) loco)

    (define/public (get-next from) (remove from connections))

    (define/public (get-id) id)
    (define/public (get-connections) connections)
    (define/public (get-center) center)
    (define/public (get-tag) tag)
    (define/public (get-signal-color) signal-color)
    (define/public (get-length) length)
    
    (define/public (draw-on-gui dc)
      (send dc set-pen "black" 3 'solid)
      (send dc draw-line (send node1 get-x) (send node1 get-y) (send node2 get-x) (send node2 get-y))
      (send dc set-pen (symbol->string signal-color) 15 'solid)
      (define center (calculate-center (send node1 get-pos) (send node2 get-pos)))
      (send dc draw-point (- (send center get-x) 0) (- (send center get-y) 0))
      (send dc draw-text (symbol->string id) (- (send center get-x) 20) (- (send center get-y) 20))
      )))


(define track%
  (class object%
    (super-new)
    (init-field id node1 node2)

    (define tag 'track)
    (define connections (list node1 node2))
    (define center (calculate-center node1 node2))
    (define length (pythagoras node1 node2))

    (define/public (get-next from) (remove from connections))

    (define/public (get-id) id)
    (define/public (get-tag) tag)
    (define/public (get-center) center)
    (define/public (get-connections) connections)
    (define/public (get-length) length)
    
    (define/public (draw-on-gui dc)
      (send dc set-pen "black" 3 'solid)
      (send dc draw-line (send node1 get-x) (send node1 get-y) (send node2 get-x) (send node2 get-y)))))


(define switch%
  (class object%
    (super-new)
    (init-field id middle-node in-node out-node1 out-node2)
    
    (define tag 'switch)
    (define connections (list in-node out-node1 out-node2))
    (define position 1)
    (define center (send middle-node get-pos))
    (define length-pos-1 (pythagoras in-node out-node1))
    (define length-pos-2 (pythagoras in-node out-node2))

    (define/public (set-position! new-position)
      (set! position new-position))

    (define/public (get-next from)
      (if (eq? from in-node)
          (list out-node1 out-node2)
          in-node))

    (define/public (find-switch-position in out)
      ;; Calculates the switch position basen on the in-node and out-node.
      (cond [(eq? in out-node1) 1]
            [(eq? in out-node2) 2]
            [(and (eq? in in-node) (eq? out out-node1)) 1]
            [(and (eq? in in-node) (eq? out out-node2)) 2]))

    (define/public (get-id) id)
    (define/public (get-x) (send middle-node get-x))
    (define/public (get-y) (send middle-node get-y))
    (define/public (get-center) center)
    (define/public (get-middle-node) middle-node)
    (define/public (get-in-node) in-node)
    (define/public (get-out-node1) out-node1)
    (define/public (get-out-node2) out-node2)
    (define/public (get-tag) tag)
    (define/public (get-connections) connections)
    (define/public (get-position) position)
    (define/public (get-length pos) (if (= pos 1) length-pos-1 length-pos-2))
     
    (define/public (draw-on-gui dc)
      (send dc set-pen "black" 3 'solid)
      (send dc draw-line (send in-node get-x) (send in-node get-y) (send middle-node get-x) (send middle-node get-y))
      (cond [(= position 1)
             (send dc draw-line (send middle-node get-x) (send middle-node get-y) (send out-node1 get-x) (send out-node1 get-y))
             (send dc set-pen "grey" 1.5 'short-dash)
             (send dc draw-line (send middle-node get-x) (send middle-node get-y) (send out-node2 get-x) (send out-node2 get-y))]
            [else
             (send dc draw-line (send middle-node get-x) (send middle-node get-y) (send out-node2 get-x) (send out-node2 get-y))
             (send dc set-pen "grey" 1.5 'short-dash)
             (send dc draw-line (send middle-node get-x) (send middle-node get-y) (send out-node1 get-x) (send out-node1 get-y))])
      (send dc set-pen "red" 6 'solid)
      (send dc draw-point (send center get-x) (send center get-y)))))


(define loco%
  (class object%
    (super-new)
    (init-field id start-block)

    (define tag 'loco)
    (define speed 0)
    (define last-speed 1.7)
    (define current-detection-block start-block)
    (define previous-detection-block #f)
    (send start-block set-signal-color! 'red)
    (define path null)

    (define/public (get-nearest-detection-blocks)
      (send current-detection-block get-nearest-detection-blocks))

    (define/public (set-speed! new-speed)
                                     
      (set! last-speed speed)
      (set! speed new-speed))
    
    (define/public (set-current-detection-block! new-block)
      (set! previous-detection-block current-detection-block)
      (set! current-detection-block new-block))

    (define/public (get-next-detection-blocks)
      (remove previous-detection-block (get-nearest-detection-blocks)))
    
    (define/public (set-path! new-path)
      (set! path new-path))


    (define/public (get-path) path)

    (define/public (get-id) id)
    (define/public (get-tag) tag)
    (define/public (get-current-detection-block) current-detection-block)
    (define/public (get-previous-detection-block) previous-detection-block)
    (define/public (get-speed) speed)
    (define/public (get-center) (send current-detection-block get-center))
    (define/public (get-last-speed) last-speed)

    (define/public (draw-on-gui color dc)
        (let* ((n1 (car (send current-detection-block get-connections)))
               (n2 (cadr (send current-detection-block get-connections)))
               (n1-x (send n1 get-x))
               (n1-y (send n1 get-y))
               (n2-x (send n2 get-x))
               (n2-y (send n2 get-y))
               (dy (- n2-y n1-y))
               (dx (- n2-x n1-x)))
          (send dc set-pen color 12 'solid)
          (send dc draw-line (+ n1-x (/ dx 4)) (+ n1-y (/ dy 4)) (- n2-x (/ dx 4)) (- n2-y (/ dy 4)))))))


(define railway%
  (class object%
    (super-new)
    (define start-node null)
    (define nodes (make-hash))
    (define tracks (make-hash))
    (define detection-blocks (make-hash))
    (define switches (make-hash))
    (define locos (make-hash))

    (define/private (load-railway-from-file)
      (let ([lines (map string-split (file->lines layout-file))])
        (for-each 
         (lambda (l)
           (case (string->symbol (car l))
             [(L) (let* ([id (string->symbol (list-ref l 1))]
                         [n1-id (string->symbol (list-ref l 2))]
                         [n2-id (string->symbol (list-ref l 3))]
                         [n1 (hash-ref nodes n1-id)]
                         [n2 (hash-ref nodes n2-id)]
                         [detection-block (get-segment-between n1 n2)]
                         [loco (make-object loco% id detection-block)])
                    (dict-set! locos id loco))]
           
             [(N) (let* ([id (string->symbol (list-ref l 1))]
                         [x (string->number (list-ref l 2))]
                         [y (string->number (list-ref l 3))]
                         [node (make-object node% id (make-object coordinate% (* x railway-scale-x) (* y railway-scale-y)))])
                    (when (null? start-node) (set! start-node node))
                    (dict-set! nodes id node))]
           
             [(T) (let* ([n1-id (string->symbol (list-ref l 1))]
                         [n2-id (string->symbol (list-ref l 2))]
                         [n1 (hash-ref nodes n1-id)]
                         [n2 (hash-ref nodes n2-id)]
                         [track-id (symbols-list->symbol (list n1-id n2-id))]
                         [track (make-object track% track-id n1 n2)])
                    (dict-set! tracks track-id track)
                    (send n1 add-connection track)
                    (send n2 add-connection track))]
           
             [(S) (let* ([node-middle-id (string->symbol (list-ref l 1))]
                         [node-in-id (string->symbol (list-ref l 2))]
                         [node-out1-id (string->symbol (list-ref l 3))]
                         [node-out2-id (string->symbol (list-ref l 4))]
                         [node-middle (hash-ref nodes node-middle-id)]
                         [node-in (hash-ref nodes node-in-id)]
                         [node-out1 (hash-ref nodes node-out1-id)]
                         [node-out2 (hash-ref nodes node-out2-id)]
                         [switch-id (symbols-list->symbol (list node-middle-id node-in-id node-out1-id node-out2-id))]
                         [switch (make-object switch% switch-id node-middle node-in node-out1 node-out2)])
                    (dict-set! switches switch-id switch)
                    (send node-in add-connection switch)
                    (send node-out1 add-connection switch)
                    (send node-out2 add-connection switch))]
           
             [(D) (let* ([id (string->symbol (list-ref l 1))]
                         [n1-id (string->symbol (list-ref l 2))]
                         [n2-id (string->symbol (list-ref l 3))]
                         [n1 (hash-ref nodes n1-id)]
                         [n2 (hash-ref nodes n2-id)]
                         [detection-block (make-object detection-block% id n1 n2)])
                    (dict-set! detection-blocks id detection-block)
                    (send n1 add-connection detection-block)
                    (send n2 add-connection detection-block))]))
         lines))
      (dict-for-each
       detection-blocks
       (lambda (block-id block)
         (send block set-nearest-detection-blocks! (find-nearest-detection-blocks block)))
       )
      )

    (define/public (find-path start dest)
      ;; Calculates a path using a temporary implementation found in railway-search.rkt
      (find-path-temporary this start dest))

    (define/public (get-detection-block id)
      (hash-ref detection-blocks id))
    
    (define/public (get-switch id)
      (hash-ref switches id))
    
    (define/public (get-track id)
      (hash-ref tracks id))
    
    (define/public (get-loco id)
      (hash-ref locos id))

    (define/public (get-start-node) start-node)
    (define/public (get-nodes) nodes)
    (define/public (get-tracks) tracks)
    (define/public (get-detection-blocks) detection-blocks)
    (define/public (get-switches) switches)
    (define/public (get-locos) locos)
    (load-railway-from-file)))



