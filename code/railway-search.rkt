#lang racket

;; This file contains various functions used to navigate the railway and making paths.

(require "helpers.rkt")
(provide find-path-temporary path% find-nearest-detection-blocks)

(define (path<? lst1 lst2)
  (< (calculate-path-length lst1) (calculate-path-length lst2)))

(define (calculate-path-length path)
  (define l 0)
  (for ([path-item path])
    (define tag (car path-item))
    (cond [(or (eq? tag 'track) (eq? tag 'detection-block)) (set! l (+ l (send (cadr path-item) get-length)))]
          [(eq? tag 'switch) (set! l (+ l (send (cadr path-item) get-length (caddr path-item))))]))
  l)

;; Reverses the order of the path and also changes the switch positions if required.
(define (reverse-path p)
  (define first (cadr (car p)))
  (reverse (map (lambda (instruction)
                  (cond [(and (eq? (car instruction) 'switch) (eq? (cadr instruction) first))
                         (list 'switch (cadr instruction) (+ (modulo (caddr instruction) 2) 1))]
                        [else instruction]))
                p)))

;; Merges two lists in one list.
;; (combine-lists '(1 2 3) '(4 5)) -> '((1 2 3) '(4 5))
;; (combine-lists '(1 2 3) '()) -> '((1 2 3))
(define (combine-lists lst1 lst2)
  (cond [(null? lst1) (list lst2)]
        [(null? lst2) (list lst1)]
        [else (list lst1 lst2)]))

;; The path list created by the algorithm contain "instruction" thay hold the relevant segment plus some metadata.
;; (e.g. '('switch #switch-object 2))
(define (make-instruction label object . args)
  (append (list label object) args))

(define (make-block-instruction obj)
  (make-instruction 'detection-block obj))

(define (make-track-instruction obj)
  (make-instruction 'track obj))

(define (get-sucessors segment previous)
  (remove previous (send segment get-connections)))

;; Find the neighboring detection blocks of a given block.
(define (find-nearest-detection-blocks start-block)
  (define start-neighbors (send start-block get-connections))
  (define results '())
  (define visited '())
  (define (loop current previous)
    (set! visited (cons current visited))
    (cond [(is-detection-block? current)
           (set! results (cons current results))]
          [else
           (define next (send current get-next previous))
           (when (not (null? next))
             (if (pair? next)
                 (for ([connection next])
                   (when (not (member connection visited))
                     (set! visited (cons current visited))
                     (loop connection current)))
                 (loop next current)))]))
  (loop (car start-neighbors) start-block)
  (loop (cadr start-neighbors) start-block)
  results)

(define (calculate-shortest-path p1 p2)
  (cond [(null? p1) p2]
        [(null? p2) p1]
        [else (if (path<? p1 p2) p1 p2)]))


(define path%
  (class object%
    (super-new)
    (init-field railway src-id dst-is loco)
    (define instructions-list '())
    (define path-instruction-length 0)
    (define path-length 0)
    (define current-position 0)
    (define remaining-path '())
    (define current null)
    (define next-item null)
    (define breakpoints '())
    (define start-block (send railway get-detection-block src-id))
    (define end-block (send railway get-detection-block dst-is))
    (define current-breakpoint null)
    (define detection-blocks '())
    (define dirrection #f)

    (define/public (add-switch! object position)
      (set! path-instruction-length (+ path-instruction-length 1))
      (set! instructions-list (append-item instructions-list (list object position)))
      (set! remaining-path instructions-list))

    (define/public (add-detection-block! object)
      (set! path-instruction-length (+ path-instruction-length 1))
      (set! instructions-list (append-item instructions-list (list object)))
      (set! detection-blocks (cons object detection-blocks))
      (set! remaining-path instructions-list))

    (define/public (add-track! object)
      (set! path-instruction-length (+ path-instruction-length 1))
      (set! instructions-list (append-item instructions-list (list object)))
      (set! remaining-path instructions-list))

    (define/public (add-speed! speed)
      (set! path-instruction-length (+ path-instruction-length 1))
      (set! instructions-list (append-item instructions-list (list 'speed speed)))
      (set! remaining-path instructions-list))

    (define/public (add-reverse!)
      (set! path-instruction-length (+ path-instruction-length 1))
      (set! instructions-list (append-item instructions-list (list 'reverse)))
      (set! remaining-path instructions-list))

    (define/public (finish!)
      (add-speed! 0)
      (set! instructions-list (append-item instructions-list (list 'end)))
      (set! remaining-path instructions-list)
      (set! path-length (calculate-path-length instructions-list))
      (set! current (car instructions-list)))

    (define/public (set-dirrection! dir) ;; TODO: used?
      (set! dirrection dir))

    (define/public (get-dirrection)
      dirrection)

    (define/public (move!)
      (set! current-position (+ current-position 1))
      (set! remaining-path (cdr remaining-path))
      (set! current (car remaining-path))
      (if (or (eq? (caar remaining-path) 'end) (eq? (cadr remaining-path) 'end))
          (set! next-item #f)
          (set! next-item (cadr remaining-path)))
      (when (eq? (car current) 'speed)
        (set! dirrection (sgn (cadr current)))))

    (define/public (reverse-path! . reverse-switches)
      ;; By default the positions of switches remain unmodified when reversing the path, unless ...
      ;; ... the switch is in the reverse-switches list. 
      (set! instructions-list
            (reverse
             (map (lambda (instruction)
                    (cond [(and (is-switch? (car instruction)) (member  (car instruction) reverse-switches))
                           (list (car instruction) (+ (modulo (caddr instruction) 2) 1))]
                          [else instruction]))
                  instructions-list))))

    (define/public (print-path)
      (for ([instruction instructions-list])
        (define object (car instruction))
        (cond [(or (eq? object 'reverse) (eq? object 'speed) (eq? object 'end))
               (println instruction)]
              [(is-switch? object)
               (display (send object get-id)) (display " -> ") (println (cadr instruction))]
              [else (println (send object get-id))])))

    (define/public (intersects? other-path) ;; TODO: Are multiple intersections detected?
      (define other-instructions-list (send other-path get-instructions-list))
      (define this-path-objects (filter object? (map car instructions-list)))
      (define other-path-objects (filter object? (map car other-instructions-list)))
      (let loop
        ((remaining this-path-objects)
         (previous-db (send loco get-current-detection-block)))
        (define current (car remaining))
        (cond [(member current other-path-objects)
               previous-db]
              [(null? (cdr remaining)) #f]
              [else (loop (cdr remaining) (if (is-detection-block? current) current previous-db))])))

    (define/public (insert-break! block other-loco other-loco-block)
      (set! breakpoints (cons (list block other-loco other-loco-block) breakpoints)))

    (define/public (reached-breakpoint?)
      (define break? #f)  ;; TODO: Recursion or use of #:break by putting the set! in in break condition.
      (for ([breakpoint breakpoints])              ;; TODO: Breakpoint betters abstraction.
        (define trigger-block (car breakpoint))
        (when (eq? trigger-block (send loco get-current-detection-block))
          (set! break? #t)
          (set! current-breakpoint breakpoint)))
      break?)

    (define/public (finished-current-breakpoint?) ;; TODO: Ugly nested if.
      (if (null? current-breakpoint)
          #f
          (let ([other-loco (cadr current-breakpoint)]
                [end-breakpoint-block (caddr current-breakpoint)])
            (if (eq? (send other-loco get-current-detection-block) end-breakpoint-block)
                (begin (set! breakpoints (remove current-breakpoint breakpoints))
                       (set! current-breakpoint null)
                       #t)
                #f))))

    (define/public (is-block-in-path? block)
      (member block detection-blocks))

    ;(define/public (delete-breakpoint! breakpoint)
    ;  (println "Deleting breakpont")
    ;  (println breakpoints)
    ;  (set! breakpoints (remove breakpoint breakpoints))
    ;  (println breakpoints)
    ;  (println "------------")
    ;  )

    (define/public (load-instructions! instructions)
      (for [(instruction instructions)]
        (define type (car instruction))
        (cond [(eq? type 'detection-block) (add-detection-block! (cadr instruction))]
              [(eq? type 'switch) (add-switch! (cadr instruction) (caddr instruction))]
              [(eq? type 'track) (add-track! (cadr instruction))]
              [(eq? type 'reverse) (add-reverse!)]
              [(eq? type 'speed)
               (add-speed! (cadr instruction))
               (when (not dirrection)
                 (set! dirrection (sgn (cadr instruction))))]
              [(eq? type 'end) (finish!)]
              )))

    (define/public (get-next-detection-block)
      (let loop
        ((remaining remaining-path))
        (cond [(null? remaining) #f]
              [(and (object? (caar remaining)) (is-detection-block? (caar remaining))) (caar remaining)]
              [else (loop (cdr remaining))])))

    (define/public (get-first-block-after block) ;; TODO: Fix this shit.
      (define passed-current #f) ;; TODO: Better recursion.
      (let loop
        ((remaining remaining-path))
        (cond [(null? remaining) #f]
              [(and (object? (caar remaining)) (is-detection-block? (caar remaining))) ;; TODO: Add is-object? to is-block?
               (if passed-current
                   (caar remaining)
                   (begin
                     (when (eq? block (caar remaining))
                       (set! passed-current #t))
                     (loop (cdr remaining))))]
              [else (loop (cdr remaining))])))

    (define/public (get-first-block-before block)
      (let loop
        ((remaining remaining-path)
         (previous #f))
        (cond [(null? remaining) #f]
              [(and (object? (caar remaining)) (is-detection-block? (caar remaining)))
               (if (eq? (caar remaining) block)
                   previous
                   (loop (cdr remaining) (caar remaining)))] ;; TODO: Add is-object? to is-block?
              [else (loop (cdr remaining) previous)])))

    (define/public (get-item-after item)
      (let loop
        ((remaining remaining-path))
        (cond [(eq? (caar remaining) 'end)
               #f]
              [(or (equal? (car remaining) item) (eq? (caar remaining) item))
               (cadr remaining)]
              [else (loop (cdr remaining))])))

    (define/public (get-remaining-path) remaining-path)
    (define/public (get-current-position) current-position)
    (define/public (length) path-length)
    (define/public (get-path-instruction-length) path-instruction-length)
    (define/public (get-current) current)
    (define/public (get-next-item) next-item)
    (define/public (get-instructions-list) instructions-list)
    (define/public (get-start-block) start-block)
    (define/public (get-end-block) end-block)
    (define/public (get-current-breakpoint) current-breakpoint)
    (define/public (get-breakpoints) breakpoints)
    (define/public (get-detection-blocks) detection-blocks)))







(define (find-path-temporary railway src dst)
  (define (walk path visited current previous start end-f)  ;; End-f is a function that tests if a condition is met.  
    (cond [(is-detection-block? current)
           (if (not (member current visited))
               (if (end-f current)
                   (append-item path (make-block-instruction current))
                   (let ([sucessor (car (get-sucessors current previous))])
                     (walk (append-item path (make-block-instruction current))
                           (cons current visited)
                           sucessor
                           current
                           start
                           end-f)))
               '())]

          [(is-node? current)
           (let ([sucessors (get-sucessors current previous)])
             (if (not (null? sucessors))
                 (walk path
                       visited
                       (car sucessors)
                       current
                       start
                       end-f)
                 '()))]

          [(is-track? current)
           (if (not (member current visited))
               (let ([sucessor (car (get-sucessors current previous))])
                 (walk (append-item path (make-track-instruction current))
                       (cons current visited)
                       sucessor
                       current
                       start
                       end-f))
               '())]

          [(is-switch? current)
           (if (not (member current visited))
               (let ([sucessors (send current get-next previous)])
                 (if (pair? sucessors)
                     ;; Entering the switch from the in-node -> there are 2 possible sucessors.
                     (let* ([sucessor1 (car sucessors)]
                            [sucessor2 (cadr sucessors)]
                            [switch-pos1 (send current find-switch-position previous sucessor1)]
                            [switch-pos2 (send current find-switch-position previous sucessor2)])
                       (calculate-shortest-path (walk (append-item path (make-instruction 'switch current switch-pos1))
                                                      (cons current visited)
                                                      sucessor1
                                                      current
                                                      start end-f)
                                                (walk (append-item path (make-instruction 'switch current switch-pos2))
                                                      (cons current visited)
                                                      sucessor2
                                                      current
                                                      start end-f)))
                     ;; Entering the switch from an out-node -> 1 sucessor + path by reversing loco after the switch.
                     (let* ([straight-sucessor sucessors]
                            [straight-switch-pos (send current find-switch-position previous straight-sucessor)]
                            [reverse-switch-pos (+ (modulo straight-switch-pos 2) 1)])
                       
                       ;; Continue walking to the next sucessor (always the in-node of the switch)
                       (define walk-straight (lambda () (walk (append-item path (make-instruction 'switch current straight-switch-pos))
                                                              (cons current visited)
                                                              straight-sucessor
                                                              current
                                                              start end-f)))
                       ;; Also find a path to the next detection-block, where the loco can reverse.
                       ;; This allows for for complex routes.
                       (define walk-with-reverse (lambda () '()))
                       (define next-detection-block-path (walk (list (make-instruction 'switch current straight-switch-pos))
                                                               (cons current visited)
                                                               straight-sucessor
                                                               current
                                                               start
                                                               (lambda (b) (is-detection-block? b))))
                       (when (nor (null? next-detection-block-path))
                         ;(error "Failed to find next-db path from" (send current get-id))
                         (define reverse-next-detection-block-path (reverse-path next-detection-block-path))
                         (define full-turn-around-path (append next-detection-block-path
                                                               (list (list 'reverse))
                                                               (cdr reverse-next-detection-block-path)))
                         (define new-visited '())
                         (for ([new-item next-detection-block-path])
                           (when (member (car new-item) '('detection-block 'switch 'track))
                             (set! new-visited (cons (cadr new-item) new-visited))))
                         (define reverse-out-node (if (eq? previous (send current get-out-node1))
                                                      (send current get-out-node2)
                                                      (send current get-out-node1)))
                         (set! walk-with-reverse (lambda () (walk (append path full-turn-around-path)
                                                                  (append visited new-visited)
                                                                  reverse-out-node
                                                                  current
                                                                  start end-f))))
                       
                       
                       (calculate-shortest-path (walk-straight)
                                                (walk-with-reverse)))))
               '())]))

  ;; Start the search.
  (define src-block (send railway get-detection-block src))
  (define instruction (make-block-instruction src-block))

  (define paths (combine-lists
                 (walk (list instruction) (list src-block) (car (send src-block get-connections)) src-block src (lambda (b) (eq? (send b get-id) dst)))
                 (walk (list instruction) (list src-block) (cadr (send src-block get-connections)) src-block src (lambda (b) (eq? (send b get-id) dst)))))

  (when (andmap null? paths)
    (println "ERROR:")
    (println "No paths found. This is probably a bug with the algorithm or the railway layout."))
  
  (define shortest-path
    (if (= (length paths) 1)
        (car paths)
        (calculate-shortest-path (car paths) (cadr paths))))
  
  (define first-item (cadr (list-ref shortest-path 0)))
  (define second-item (cadr (list-ref shortest-path 1)))
      
  (define first-node (car (send first-item get-connections)))
  (define next-node-in-path (get-segment-between first-item second-item))
  (define start-speed (if (symbol<? (send first-node get-id) (send next-node-in-path get-id)) ; TODO: This method does not always work. SOLUTION: Locos need to know what the next segment should be if the keep going in the same dirrections.
                          2.5
                          -2.5))
  (set! shortest-path (cons (list 'speed start-speed) shortest-path))
  shortest-path)













