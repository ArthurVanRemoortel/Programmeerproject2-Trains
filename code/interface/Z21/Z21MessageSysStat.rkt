#lang racket
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Florian Myter          ;;
;; Software Languages lab ;;
;; fmyter@vub.ac.be       ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(require "./racket-bits-utils-master/bits.rkt" "./Z21MessageUtil.rkt")
(provide make-serial-msg is-serial-answer? get-serial-number make-logoff-msg make-x-bus-msg is-x-bus-answer? get-x-bus-version get-x-bus-central make-get-status-msg make-set-track-power-off-msg make-set-track-power-on-msg is-bc-track-power-off-msg? is-bc-track-power-on-msg? is-bc-programming-mode-msg? is-bc-shortcut-msg? is-unknown-command-msg?
         is-status-changed-msg? is-emergency-status-msg?  is-voltage-off-status-msg? is-short-circuit-status-msg? is-programming-mode-status-msg? make-stop-msg is-bc-stopped-msg? make-firmware-msg is-firmware-answer? get-firmware-version general-broadcast-flag position-broadcast-flag system-broadcast-flag make-broadcast-msg make-get-broadcast-msg 
         is-get-broadcast-answer? get-broadcast-flags is-system-state-changed-msg? get-system-state-main-current get-system-state-prog-current get-system-state-filtered-current get-system-state-temperature get-system-state-supply-voltage get-system-state-vcc-voltage is-system-state-emergency? is-system-state-voltage-off? is-system-state-short-circuit? 
         is-system-state-programming-mode? is-system-stateex-temp-high? is-system-stateex-power-lost? is-system-stateex-short-circuit-ex? is-system-stateex-short-circuit-in? make-get-system-state-msg)

;Implementation of the System state, update and version part of the protocol (based on document version 1.02)


;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Serial Number MSG (2.1) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(define (make-serial-msg)
  (let
      ([len-lsb  "04"]
       [len-msb  "00"]
       [head-lsb "10"]
       [head-msb "00"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb)))

(define (is-serial-answer? msg)
  (let*
      ([len-lsb  "08"]
       [len-msb  "00"]
       [head-lsb "10"]
       [head-msb "00"]
       [data0    template-wildcard]
       [data1    template-wildcard]
       [data2    template-wildcard]
       [data3    template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb data0 data1 data2 data3)])
  (msg-eq? msg template)))

;Requesting the z21's serial number results in a response message contain a 32bit little endian serial number as data part
;In order to correctly read out the number, first convert it to big endian 
(define (get-serial-number msg)
  (let
      ([little-endian (bytes (get-msg-data-byte msg 0) (get-msg-data-byte msg 1) (get-msg-data-byte msg 2) (get-msg-data-byte msg 3))])
    (read-bytes (32-convert little-endian))))


;;;;;;;;;;;;;;;;;;;;
;Logoff MSG (2.2) ;;
;;;;;;;;;;;;;;;;;;;;

;Log out the sending client (logging in is done implicitly with first request send to the z21)
;No response from z21

(define (make-logoff-msg)
  (let
      ([len-lsb  "04"]
       [len-msb  "00"]
       [head-lsb "30"]
       [head-msb "00"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;X get version MSG (2.3) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;

; Request the x-bus version of the z21

(define (make-x-bus-msg)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "21"]
       [db0      "21"]
       [xor      "00"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 xor)))

(define (is-x-bus-answer? msg)
  (let*
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "63"]
       [db0      "21"]
       [db1      template-wildcard]
       [db2      template-wildcard]
       [xor      "60"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)])
    (msg-eq? msg template)))

(define (get-x-bus-version msg)
  (if (= (get-msg-data-byte msg 2) (hex-string->byte "30"))
      '3.0
      'UnknownVersion))

(define (get-x-bus-central msg)
  (if (= (get-msg-data-byte msg 3) (hex-string->byte "12"))
      'z21
      'UnknownCentral))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;X get status MSG (2.4) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;Requests the z21's status. The data received is a subset of data obtained through the systemstata-datachanged message (2.18)
;The z21 answers with the status changed message (2.12)

(define (make-get-status-msg)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "21"]
       [db0      "24"]
       [xor      "05"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set track power off MSG (2.5) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;z21 answers with the bc-track-power-off message (2.7)

(define (make-set-track-power-off-msg)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "21"]
       [db0      "80"]
       [xor      "a1"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set track power on MSG (2.6) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message turns the track power on. This automatically switches emergency and programming mode off
;z21 answers with the bc-track-power-on message (2.8)

(define (make-set-track-power-on-msg)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "21"]
       [db0      "81"]
       [xor      "a0"])
  (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 xor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BC track power off MSG (2.7) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 to the client if:
; - The client sent the set-track-power-off message  (2.5)
; - The track has been power off by another device (such as a MultiMaus)
; - The client enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-bc-track-power-off-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "00"]
       [xor      "61"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BC track power on MSG (2.8) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 to the client if:
; - The client sent the set-track-power-on message  (2.6)
; - The track has been power off by another device (such as a MultiMaus)
; - The client enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-bc-track-power-on-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "01"]
       [xor      "60"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BC programming mode MSG (2.9) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 if the z21 was set in programming mode by the cv-read or cv-read messages (6.1 or 6.2)
;and the client enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-bc-programming-mode-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "02"]
       [xor      "63"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;BC track short circuit MSG (2.10) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 if a short circuit has happened and the client has enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-bc-shortcut-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "08"]
       [xor      "69"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Unknown command MSG (2.11) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 if it received an unknown command from the client

(define (is-unknown-command-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "61"]
       [db0      "82"]
       [xor      "e3"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Status Changed MSG (2.12) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 as a response to a get-status message (2.4)
;The status can be in four states: 1) emergency stop (0x01), 2) track voltage off (0x02), 3) short circuit (0x04) or 4) programming mode (0x20)
;This information is also a subset of the larger systemstate-datachanged message (see 2.18)

(define (is-status-changed-msg? msg)
  (let*
      ([len-lsb  "08"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "62"]
       [db0      "22"]
       [db1      template-wildcard]
       [xor      template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 xor)])
    (msg-eq? msg template)))

(define (is-emergency-status-msg? msg)
  (= (get-msg-data-byte msg 2) (hex-string->byte "01")))

(define (is-voltage-off-status-msg? msg)
  (= (get-msg-data-byte msg 2) (hex-string->byte "02")))

(define (is-short-circuit-status-msg? msg)
  (= (get-msg-data-byte msg 2) (hex-string->byte "04")))

(define (is-programming-mode-status-msg? msg)
  (= (get-msg-data-byte msg 2) (hex-string->byte "20")))


;;;;;;;;;;;;;;;;;;;;;;;
;Set stop MSG (2.13) ;;
;;;;;;;;;;;;;;;;;;;;;;;

;This message initiates emergency mode (i.e. all locs stop driving but the voltage remains on)
;The z21 answers with the bc-stopped message (see 2.14)

(define (make-stop-msg)
  (let
      ([len-lsb  "06"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "80"]
       [xor      "80"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head xor)))


;;;;;;;;;;;;;;;;;;;;;;;;;
;BC stopped MSG (2.14) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is sent by the z21 to the client if:
; - The client sent the set-stop message  (2.13)
; - The stop message has been sent by another device (such as a MultiMaus)
; - The client enabled the appropriate broadcast flag (0x00000001) (see 2.16)

(define (is-bc-stopped-msg? msg)
  (let*
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "81"]
       [db0      "00"]
       [xor      "81"]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 xor)])
    (msg-eq? msg template)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get firmware version MSG (2.15) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Request the z21's firmware version

(define (make-firmware-msg)
  (let
      ([len-lsb  "07"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "f1"]
       [db0      "0a"]
       [xor      "fb"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb x-head db0 xor)))

;Response from the z21. The db1 contains the version number's msb and db2 contains the lsb. The version is given in bcd-format (i.e. 0x01 msb and 0x23 lsb is version 1.23)

(define (is-firmware-answer? msg)
  (let*
      ([len-lsb  "09"]
       [len-msb  "00"]
       [head-lsb "40"]
       [head-msb "00"]
       [x-head   "f3"]
       [db0      "0a"]
       [db1      template-wildcard]
       [db2      template-wildcard]
       [xor      template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb x-head db0 db1 db2 xor)])
    (msg-eq? msg template)))

(define (get-firmware-version msg)
  (let
      ([major  (byte->hex-string (get-msg-data-byte msg 2))]
       [minor  (byte->hex-string (get-msg-data-byte msg 3))])
    (string-append major "." minor)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Set broadcast flags MSG (2.16) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;This message is transmitted to the z21 in order to set various broadcast flags (which determine what information is sent to the client).
;Broadcast flags are set on a per-client basis
;(i.e. ip + port number) and must be re-set after each logoff
;The broadcast flags are the following (multiple broadcast flags can be set at once by OR'ing them into one 32-bit flag)
;0x00000001 => Automatically generated messages regarding genral driving and switching info. This concerns following z21-to-client messages: 2.7, 2.8, 2.9, 2.10, 2.14, 4.4, 5.3
; The 4.4 message is only be received if the appropriate loc-address has registered itself
;0x00000002 => Concerns changes in the position indicator (i.e. train positions on tracks). This concerns following z21-to-client message: 7.1
;0x00000100 => Changes to the z21 system itself. This concerns following z21-to-client message: 2.18

(define general-broadcast-flag "00000001")
(define position-broadcast-flag  "00000002")
(define system-broadcast-flag  "00000100")

;Will _OR_ the list of flags and return 4 bytes in little endian order (required to send the message)
(define (combine-flags flags)
  (define (or-flags rest)
    (if (= (length rest) 1)
        (hex-string->integer  (car rest))
        (integer-or (hex-string->integer (car rest)) (or-flags (cdr rest)))))
  (define flag-bytes (int32->bytes (or-flags flags))) ;Big endian representation of the final 32 bit flag
  (32-convert flag-bytes)
  )
  

(define (make-broadcast-msg flags)
  (let*
      ([len-lsb  "08"]
       [len-msb  "00"]
       [head-lsb "50"]
       [head-msb "00"]
       [flgs     (combine-flags flags)]
       [data0    (byte->hex-string (bytes-ref flgs 0))]
       [data1    (byte->hex-string (bytes-ref flgs 1))]
       [data2    (byte->hex-string (bytes-ref flgs 2))]
       [data3    (byte->hex-string (bytes-ref flgs 3))])
    (make-hex-msg len-lsb len-msb head-lsb head-msb data0 data1 data2 data3)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;Get broadcast flags MSG (2.17) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Request the broadcast flags for this client

(define (make-get-broadcast-msg)
  (let
      ([len-lsb  "04"]
       [len-msb  "00"]
       [head-lsb "51"]
       [head-msb "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb)))

;z21 responds with 32-bit little endian OR'd flags

(define (is-get-broadcast-answer? msg)
  (let*
      ([len-lsb  "08"]
       [len-msb  "00"]
       [head-lsb "51"]
       [head-msb "00"]
       [data1    template-wildcard]
       [data2    template-wildcard]
       [data3    template-wildcard]
       [data4    template-wildcard]
       [template (make-msg-template len-lsb len-msb head-lsb head-msb data1 data2 data3 data4)])
    (msg-eq? msg template)))


;Return a list of the flags that are set for the requesting client
(define (get-broadcast-flags msg)
  (let*
       ([high-flag (get-msg-data-byte msg 2)]
        [low-flag  (get-msg-data-byte msg 3)]
        [flags     '()])
    (when (= high-flag 1)
          (set! flags (cons system-broadcast-flag flags)))
    (when (= low-flag 1)
          (set! flags (cons general-broadcast-flag flags)))
    (when (= low-flag 2)
          (set! flags (cons position-broadcast-flag flags)))
    (when (= low-flag 3)
          (set! flags (cons position-broadcast-flag (cons  general-broadcast-flag flags))))
    flags))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;System state data changed MSG (2.18) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Message sent by the z21 to the client containing various information (see below) concerning the system. The client receives this message if:
; - the client enabled the appropriate broadcast flag (0x00000100) (see 2.16)
; - the client explicitly requested the information (see 2.19)
;The message contains 16 bytes of data which are organised as follows:
;+-------------+--------+---------------------+---------+--------------------------------------------+
;| Byte Offset |  Type  |        Name         |  Unit   |                  Comment                   |
;+-------------+--------+---------------------+---------+--------------------------------------------+
;|           0 | INT16  | MainCurrent         | mA      | Current on main track                      |
;|           2 | INT16  | ProgCurrent         | mA      | Current on programming track               |
;|           4 | INT16  | FilteredMainCurrent | mA      | Filtered current on main track             |
;|           6 | INT16  | Temperature         | °C      | Internal temperature of the z21            |
;|           8 | UINT16 | SupplyVoltage       | mV      | Supply voltage                             |
;|          10 | UINT16 | VCCVoltage          | mV      | Internal voltage, identical to track power |
;|          12 | UINT8  | CentralState        | bitmask | See below                                  |
;|          13 | UINT8  | CentralStateEx      | bitmask | See below                                  |
;|          14 | UINT8  | Reserved            |         |                                            |
;|          15 | UINT8  | Reserved            |         |                                            |
;+-------------+--------+---------------------+---------+--------------------------------------------+
;
;The CentralState bitmask works as follows:
;Emergency state = 0x01, track voltage off = 0x02, short circuit = 0x04, programming mode active = 0x20
;
;The CentralStateEx bitmask works as follows:
;Too high temperature = 0x01, Power lost = 0x02, short circuit external = 0x04, short circuit internal = 0x08

(define (is-system-state-changed-msg? msg)
  (let*
      ([len-lsb      "14"]
       [len-msb      "00"]
       [head-lsb     "84"]
       [head-msb     "00"]
       [data0        template-wildcard]
       [data1        template-wildcard]
       [data2        template-wildcard]
       [data3        template-wildcard]
       [data4        template-wildcard]
       [data5        template-wildcard]
       [data6        template-wildcard]
       [data7        template-wildcard]
       [data8        template-wildcard]
       [data9        template-wildcard]
       [data10       template-wildcard]
       [data11       template-wildcard]
       [data12       template-wildcard]
       [data13       template-wildcard]
       [data14       template-wildcard]
       [data15       template-wildcard]
       [template     (make-msg-template len-lsb len-msb head-lsb head-msb data0 data1 data2 data3 data4 data5 data6 data7 data8 data9 data10 data11 data12 data13 data14 data15)])
    (msg-eq? msg template)))

(define (get-system-state-main-current msg)
  (let
      ([current-lsb (get-msg-data-byte msg 0)]
       [current-msb (get-msg-data-byte msg 1)])
    (read-word current-lsb current-msb)))

(define (get-system-state-prog-current msg)
  (let
      ([current-lsb (get-msg-data-byte msg 2)]
       [current-msb (get-msg-data-byte msg 3)])
    (read-word current-lsb current-msb)))

(define (get-system-state-filtered-current msg)
  (let
      ([current-lsb (get-msg-data-byte msg 4)]
       [current-msb (get-msg-data-byte msg 5)])
    (read-word current-lsb current-msb)))

(define (get-system-state-temperature msg)
  (let
      ([temp-lsb (get-msg-data-byte msg 6)]
       [temp-msb (get-msg-data-byte msg 7)])
    (read-word temp-lsb temp-msb)))

(define (get-system-state-supply-voltage msg)
  (let
      ([volt-lsb (get-msg-data-byte msg 8)]
       [volt-msb (get-msg-data-byte msg 9)])
    (read-word volt-lsb volt-msb)))

(define (get-system-state-vcc-voltage msg)
  (let
      ([volt-lsb (get-msg-data-byte msg 10)]
       [volt-msb (get-msg-data-byte msg 11)])
    (read-word volt-lsb volt-msb)))

(define (is-system-state-emergency? msg)
  (let
      ([state (get-msg-data-byte msg 12)])
    (= state (hex-string->byte "01"))))

(define (is-system-state-voltage-off? msg)
  (let
      ([state (get-msg-data-byte msg 12)])
    (= state (hex-string->byte "02"))))

(define (is-system-state-short-circuit? msg)
  (let
      ([state (get-msg-data-byte msg 12)])
    (= state (hex-string->byte "04"))))

(define (is-system-state-programming-mode? msg)
  (let
      ([state (get-msg-data-byte msg 12)])
    (= state (hex-string->byte "20"))))

(define (is-system-stateex-temp-high? msg)
  (let
      ([state (get-msg-data-byte msg 13)])
    (= state (hex-string->byte "01"))))

(define (is-system-stateex-power-lost? msg)
  (let
      ([state (get-msg-data-byte msg 13)])
    (= state (hex-string->byte "02"))))

(define (is-system-stateex-short-circuit-ex? msg)
  (let
      ([state (get-msg-data-byte msg 13)])
    (= state (hex-string->byte "04"))))

(define (is-system-stateex-short-circuit-in? msg)
  (let
      ([state (get-msg-data-byte msg 13)])
    (= state (hex-string->byte "08"))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;System state get data MSG (2.19) ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;Request the z21's system state (see 2.18)

(define (make-get-system-state-msg)
  (let
      ([len-lsb  "04"]
       [len-msb  "00"]
       [head-lsb "85"]
       [head-msb "00"])
    (make-hex-msg len-lsb len-msb head-lsb head-msb)))
        
        
        


