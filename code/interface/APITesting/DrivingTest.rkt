#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "../FullAPI/Z21Socket.rkt" "../FullAPI/Z21MessageSysStat.rkt" "../FullAPI/Z21MessageDriving.rkt" "./MessageHandler.rkt")

;Setup
(define z (setup))

(listen z handle-msg)

(define (to-list msg)
  (define (loop ind res)
    (if (> ind (- (bytes-length msg) 1))
        res
        (loop (+ ind 1) (append res (list (bytes-ref msg ind))))))
  (loop 0 '()))

;Messages

(define get-loco-info-message (make-get-loco-info-msg "03" "00")) ;We assume that there is a loc with address 3 on the track

(define set-loco-drive-forward-message (make-set-loco-drive-msg "03" "00" 128 true 60))

(define set-loco-drive-stop-message (make-set-loco-drive-msg "03" "00" 128 true 0)) ;Setting the speed to zero makes the loc stop (i.e. break)

(define set-loco-drive-backward-message (make-set-loco-drive-msg "03" "00" 128 false 60))

;Send messages, prints read values to console (testing for return values depends on physical layout for most cases)
;Sleeping between messages is needed to avoid flooding of Z21

(send z get-loco-info-message) ;Expect Z21 to answer with loco info message

(sleep 1)

(send z set-loco-drive-forward-message) ;Expect the Z21 to answer with the loco info message  and see the train physically move

(sleep 6)

(send z set-loco-drive-stop-message) ;Expect the Z21 to answer with the loco info message  and see the train physically stop

(sleep 6)

(send z set-loco-drive-backward-message) ;Expect the Z21 to answer with the loco info message  and see the train physically move in the opposite direction of first message

(sleep 6)

(send z set-loco-drive-stop-message) 

(sleep 1)