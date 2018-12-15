#lang racket

(provide
 layout-file
 hardware-mode
 railway-scale-x
 railway-scale-y
 window-scale-h
 window-scale-w
 TCP-PORT
 INFRABEL-IP
 hardware-speed-multiplier
 disable-breakpoints)

;; The path to the text file from where the railway layout will be loaded.
(define layout-file "layouts/setup_real.txt")
;(define layout-file "layouts/cross_intersection.txt")

;; This determines wheather tha aplication uses the simulator on the hardware. options = 'simulator, 'hardware
(define hardware-mode 'simulator)

;; When using larger screen, it might be uselfull to scale the default size of the gui window.
(define window-scale-h 1.5)
(define window-scale-w 1.5)

;; Scales the size of the railway drawing without scaling the gui.
(define railway-scale-x 1.5)
(define railway-scale-y 1.5)


;; Connection information used for the TCP connection between Infrabel and NMBS.
(define TCP-PORT 12347)
(define INFRABEL-IP "127.0.0.1")

;; The hardware uses different units of speed. 
(define hardware-speed-multiplier 40)  ;; Default = 40. Increasing this value over 40 is not recommended.

(define disable-breakpoints #t)