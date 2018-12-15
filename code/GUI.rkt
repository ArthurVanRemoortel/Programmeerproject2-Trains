#lang racket

(require "railway.rkt"
         "helpers.rkt"
         "configuration.rkt"
         racket/gui/base)

(provide GUI%)
;;;;;;;;;;;;;;;
;; Constants ;;
;;;;;;;;;;;;;;;

(define window-width (exact-round (* 730 window-scale-w)));gui-scale-x)))         ; Default = 730
(define window-height (exact-round (* 430 window-scale-h)))      ; Default = 430

(define selected-panel-height 50)  ; Default = 50
(define railway-canvas-height (- window-height selected-panel-height))

(define loco-colors (list->vector (list "Blue" "Purple" "LightBlue" "Magenta")))
(define last-color-i 0)

(define GUI%
  (class object%
    (super-new)
    (init-field INFRABEL-connection NMBS)

    (define railway (send NMBS get-railway))

    (define window (new (class frame%
                          (super-new
                           [label "GUI"]
                           [min-width window-width]
                           [min-height window-height])
                          (define/augment (on-close)
                            (exit)))))
    
    (define rail-canvas #f)
    (define selected-canvas #f) ; The canvas that shows a preview of the selected widget.
    (define selected-panel #f) ; The panel with the selected widget's buttons.
    (define selected-panel-widgets '()) ; Stores the selected widget's buttons.
    (define selected-item #f)
    (define making-path-for-loco #f)
    (define static-dc #f)
    (define animated-dc #f)

    (define bitmaps '())
    (define bitmap-dcs '())

    
    (create-buffer-bitmap 'static)
    (create-buffer-bitmap 'animated)
    

    (define (init) ; TODO: Move this to the start of the class.
      (send window show #t)

      (define my-canvas%
        (class canvas%
          (define/override (on-event event)
            (handle-rail-canvas-mouse-event event))
          (super-new)))

      (set! rail-canvas (new my-canvas%
                             (parent window)
                             (min-height railway-canvas-height)))  ; = 460
      (set! selected-panel (new horizontal-panel%
                                (parent window)
                                (alignment (list 'left 'top))
                                (min-height selected-panel-height)
                                (style (list 'border))))
      (set! selected-canvas (new canvas%
                                 (parent selected-panel)
                                 (min-width selected-panel-height)
                                 (style (list 'border))))

      (set! static-dc (get-bitmap-dc 'animated))
      (set! animated-dc (get-bitmap-dc 'animated)))
    
    (define/public (create-buffer-bitmap id)
      (let* ((buffer-bitmap (make-object bitmap% window-width window-height #f #t))
             (buffer-bitmap-dc (make-object bitmap-dc% buffer-bitmap)))
        (send buffer-bitmap-dc set-brush (make-object brush% "BLACK" 'transparent))
        (set! bitmaps (cons (cons id buffer-bitmap) bitmaps))
        (set! bitmap-dcs (cons (cons id buffer-bitmap-dc) bitmap-dcs))))
    
    (define (get-bitmap-dc id)
      (cdr (assoc id bitmap-dcs)))

    (define/public (clear-dc id)
      (define the-dc (get-bitmap-dc id))
      (send the-dc clear))

    (define/public (clear dc-id)
      (send (get-bitmap-dc dc-id) clear)
      (send (get-bitmap-dc dc-id) erase))

    (define/public (refresh-all)
      (define rail-dc (send rail-canvas get-dc))
      (send rail-dc clear)
      (for-each
       (lambda (buffer-bitmap)
         (send rail-dc draw-bitmap (cdr buffer-bitmap) 0 0))
       (reverse bitmaps)))

    (define/public (refresh id)
      (send rail-canvas draw-bitmap (cdr (assoc id bitmaps)) 0 0))

    ; Update gets called from the NMBS gameloop. Redraws the gui every frame.
    (define/public (update)
      (define rail-dc (send rail-canvas get-dc))
      (draw-railway rail-dc)
      (when selected-item
        (define selected-dc (send selected-canvas get-dc))
        (draw-selected-preview selected-dc)))

    (define (draw-railway dc)
      (send dc clear)
      
      (dict-for-each (send railway get-switches) (lambda (id switch) (send switch draw-on-gui dc)))
      (dict-for-each (send railway get-tracks) (lambda (id track) (send track draw-on-gui dc)))
      (dict-for-each (send railway get-detection-blocks) (lambda (id detection-block) (send detection-block draw-on-gui dc)))
      (dict-for-each (send railway get-locos) (lambda (id loco) (send loco draw-on-gui (vector-ref loco-colors last-color-i) dc) (set! last-color-i (+ last-color-i 1))))
      (set! last-color-i 0)
      (define dc2 (get-bitmap-dc 'animated))
      (send dc2 set-pen (make-object pen% "BLUE" 5 'solid))
      (for ([i 100])
        (define x (random 0 400))
        (define y (random 0 400))
        (define x2 (random 0 400))
        (define y2 (random 0 400))
        (send dc2 draw-line x y x2 y2)
        )
      ;(refresh-all)


      )

    (define (draw-selected-preview dc)
      (send dc clear)
      (cond ((is-loco? selected-item)
             (send dc set-scale 3 3)
             (send dc draw-text (symbol->string (send selected-item get-id)) 0 0)
             (define speed-string (string-append "Speed: " (number->string (send selected-item get-speed))))
             (send dc draw-text speed-string 25 0))
            ((is-switch? selected-item)
             (send dc set-scale 3 3)
             (send dc draw-text (symbol->string (send (send selected-item get-middle-node) get-id)) 0 0)
             (send dc draw-text (string-append "Pos: " (number->string (send selected-item get-position))) 50 0))
            ((is-detection-block? selected-item)
             (send dc set-scale 3 3)
             (send dc draw-text (symbol->string (send selected-item get-id)) 0 0)
             (define signal-color (symbol->string (send selected-item get-signal-color)))
             (send dc set-pen signal-color 5 'solid)
             (send dc draw-ellipse 25 4 6 6))))

    (define (select-loco loco)
      (clear-selected-panel)
      (set! selected-item loco)
      (define increse-speed-btn
        (new button%
             (parent selected-panel)
             (label "+")
             (min-width 100)
             (callback (lambda (button event)
                         (when (send loco get-current-detection-block)
                           (define loco-speed (send loco get-speed))
                           (send INFRABEL-connection set-loco-speed! loco (+ loco-speed 0.5)))))))
      ;; TODO: Problems when setting loco speed if it's not on a detection block (/invisible)
      (define decrease-speed-btn
        (new button%
             (parent selected-panel)
             (label "-")
             (min-width 100)
             (callback (lambda (button event)
                         (when (send loco get-current-detection-block)
                           (define loco-speed (send loco get-speed))
                           (send INFRABEL-connection set-loco-speed! loco (- loco-speed 0.5)))))))

      (define make-path-btn
        (new button%
             (parent selected-panel)
             (label "Path")
             (min-width 100)
             (callback (lambda (button event)
                         (set! making-path-for-loco loco)))))
      ; Store all the new buttons in a list for later reference. This is needed to delete them later.
      (set! selected-panel-widgets (list increse-speed-btn decrease-speed-btn make-path-btn)))

    (define (select-switch switch)
      (clear-selected-panel)
      (set! selected-item switch)
      (define toggle-btn
        (new button%
             (parent selected-panel)
             (label "Toggle")
             (min-width 100)
             (callback (lambda (button event)
                         (define switch-pos (send switch get-position))
                         (send INFRABEL-connection set-switch-position! switch (+ (modulo switch-pos 2) 1))))))
      (set! selected-panel-widgets (list toggle-btn)))

    (define (select-detection-block block)
      (cond [making-path-for-loco
             (send NMBS make-path (send making-path-for-loco get-id) (send block get-id))
             (set! making-path-for-loco #f)]
            [else
             (clear-selected-panel)
             (set! selected-item block)
             (define toggle-btn
               (new button%
                    (parent selected-panel)
                    (label "Toggle Signal")
                    (min-width 100)
                    (callback (lambda (button event)
                                (if (is-red? block)
                                    (send block set-signal-color! 'green)
                                    (send block set-signal-color! 'red))))))
             (set! selected-panel-widgets (list toggle-btn))]))
          
      

    (define (clear-selected-panel)
      (for-each
       (lambda (wid) (send selected-panel delete-child wid))
       selected-panel-widgets))

    (define (handle-rail-canvas-mouse-event event)
      (define event-type (send event get-event-type))
      (when (eq? event-type 'left-down)
        (define event-handled? #f)
        (define mouse-coordinate (make-object coordinate% (send event get-x) (send event get-y)))

        (for ([(id loco) (send railway get-locos)])
          (when (and (collision? mouse-coordinate (send loco get-center) 15)
                     (not (eq? loco selected-item)))
            (select-loco loco)
            (set! event-handled? #t)))
        
        (when (not event-handled?)
          (for ([(id switch) (send railway get-switches)])
            (when (collision? mouse-coordinate (send switch get-center) 15)
              (select-switch switch)
              (set! event-handled? #t))))

        (when (not event-handled?)
          (for ([(id detection-block) (send railway get-detection-blocks)])
            (when (collision? mouse-coordinate (send detection-block get-center) 15)
              (select-detection-block detection-block)
              (set! event-handled? #t))))))

    
    (init)))