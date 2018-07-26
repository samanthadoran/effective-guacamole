(defpackage #:psx-joypads
  (:nicknames #:joypads)
  (:use :cl)
  (:export #:joypads #:make-joypads #:joypads-exception-callback
           #:tick-controllers-and-memory-cards #:tick-joypads
           #:read-joypads #:write-joypads))

(in-package :psx-joypads)

(declaim (optimize (speed 3) (safety 1)))

(declaim (boolean *debug-joypads*))
(defparameter *debug-joypads* t)

; Controller is selected by #x01, mem cards by #x81

; TODO(Samantha): This should maybe change to be something along the lines of
; memory card and controller combo?
(defstruct controller
  ; TODO(Samantha): This is just the ID for a digital pad and should be able to
  ; be changed. As far as I'm aware, the high byte is always #x5A, but the
  ; lower byte can change.
  (ack-timer 0f0 :type single-float)
  (id #x5A41 :type (unsigned-byte 16))
  (acknowledge :high :type keyword)
  (transmission-queue (list) :type list))

(defstruct joypad-status
  (transfer-ready t :type boolean)
  (receive-queue-empty t :type boolean)
  (transfer-ready-2 t :type boolean)
  (receive-parity-error nil :type boolean)
  ; Ack will be driven by the controller.
  (has-irq-7 nil :type boolean)
  (baudrate-timer 0 :type (unsigned-byte 21)))

(declaim (ftype (function (joypad-status controller)
                          (unsigned-byte 32))
                joypad-status-to-word))
(defun joypad-status-to-word (joy-stat controller)
  "Converts joypad-status from a structure to its binary representation."
  (logior #x00000000
          (ash (if (joypad-status-transfer-ready joy-stat) 1 0) 0)
          ; The PSX actually has this as not empty, so just invert the value.
          (ash (if (joypad-status-receive-queue-empty joy-stat) 0 1) 1)
          (ash (if (joypad-status-transfer-ready-2 joy-stat) 1 0) 2)
          (ash (if (joypad-status-receive-parity-error joy-stat) 1 0) 3)
          (ash (ecase (controller-acknowledge controller)
                      (:high 0)
                      (:low 1))
               7)
          (ash (if (joypad-status-has-irq-7 joy-stat) 1 0) 9)
          (ash (joypad-status-baudrate-timer joy-stat) 11)))

(defstruct joypad-control
  (transfer-enabled nil :type boolean)
  (output-enabled nil :type boolean)
  ; ???
  (receive-enabled 0 :type (unsigned-byte 1))
  (receive-interrupt-mode 0 :type (unsigned-byte 2))
  (interrupt-on-acknowledge nil :type boolean)
  (desired-slot 0 :type (unsigned-byte 1)))

(declaim (ftype (function (joypad-control)
                          (unsigned-byte 16))
                joypad-control-to-word))
(defun joypad-control-to-word (joy-control)
  "Converts joypad-control from a structure to its binary representation."
  (logior #x0000
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 0)
          (ash (if (joypad-control-output-enabled joy-control) 1 0) 1)
          (ash (if (joypad-control-receive-enabled joy-control) 1 0) 2)
          (ash (joypad-control-receive-interrupt-mode joy-control) 8)
          ; RX and TX interrupts are unsupported right now.
          (ash 0 10)
          (ash 0 11)
          (ash (if (joypad-control-interrupt-on-acknowledge joy-control) 1 0) 12)
          (ash (joypad-control-desired-slot joy-control) 13)))


(defstruct joypads
  (transmission-timer 0f0 :type single-float)
  (fired-interrupt nil :type boolean)
  (write-fifo (list) :type list)
  (received-from-controller #xFF :type (unsigned-byte 8))
  (controllers
   (make-array 2 :element-type 'controller
               :initial-contents `(,(make-controller)
                                   ,(make-controller :id #xDEAD))))
  ; The joypads/memory cards know nothing about the higher up architecture, so
  ; we just use a closure defined in the mmu to fire off any interrupts.
  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8)))
  ; Offset #x4, Read
  (joy-stat (make-joypad-status) :type joypad-status)
  ; Offset #x8, Read/Write
  (joy-mode 0 :type (unsigned-byte 8))
  ; Offset #xA, Read/Write
  (joy-ctrl (make-joypad-control) :type joypad-control)
  ; Offset #xE, Read/Write
  (joy-baud #x88 :type (unsigned-byte 16)))

(declaim (ftype (function (joypads (unsigned-byte 16))
                          joypad-control)
                word-to-joypad-control))
(defun word-to-joypad-control (joypads value)
  (when (ldb-test (byte 1 4) value)
    (setf (joypad-status-has-irq-7 (joypads-joy-stat joypads))
          nil)
    (setf (joypads-fired-interrupt joypads)
          nil)
    (setf
     (joypad-status-receive-parity-error (joypads-joy-stat joypads))
     nil))
  ; TODO(Samantha): Reset most (all?) joypad registers to 0
  (when (ldb-test (byte 1 6) value)
    (setf (joypad-status-receive-queue-empty (joypads-joy-stat
                                              joypads))
          t)
    (setf (joypad-control-desired-slot (joypads-joy-ctrl
                                               joypads))
          0)
    (setf (joypad-control-output-enabled (joypads-joy-ctrl joypads))
          nil)
    (setf (joypads-fired-interrupt joypads)
          nil)
    (setf (joypad-status-has-irq-7 (joypads-joy-stat joypads))
          nil)
    (when *debug-joypads*
      (format t "Should be resetting joypad registers..?~%"))
    nil)
  (make-joypad-control
   :transfer-enabled (ldb-test (byte 1 0) value)
   :output-enabled (ldb-test (byte 1 1) value)
   :receive-enabled (ldb (byte 1 2) value)
   :receive-interrupt-mode (ldb (byte 2 8) value)
   :interrupt-on-acknowledge (ldb-test (byte 1 12) value)
   :desired-slot (ldb (byte 1 13) value)))

(declaim (ftype (function (joypads (unsigned-byte 4))
                          (unsigned-byte 32))
                read-joypads))
(defun read-joypads (joypads offset)
  (let ((result #x00000000))
    (ecase offset
           (0
            (setf (joypad-status-receive-queue-empty (joypads-joy-stat joypads))
                  t)
            (setf result
                  (joypads-received-from-controller joypads))
            (when *debug-joypads*
              (format t "Read 0x~8,'0x from joypads at offset 0x~1,'0x~%" result offset))
            ; Once we read from the FIFO, the next entry is just FF
            (setf (joypads-received-from-controller joypads)
                  #xFF))
           (4 (setf result (joypad-status-to-word
               (joypads-joy-stat joypads)
               (aref (joypads-controllers joypads)
                     (joypad-control-desired-slot (joypads-joy-ctrl joypads))))))
           (8 (setf result (joypads-joy-mode joypads)))
           (#xA (setf result (joypad-control-to-word (joypads-joy-ctrl joypads))))
           (#xE (setf result (joypads-joy-baud joypads))))
    ; (when *debug-joypads*
    ;   (format t "Read 0x~8,'0x from joypads at offset 0x~1,'0x~%" result offset))
    result))

(declaim (ftype (function (joypads (unsigned-byte 4) (unsigned-byte 16))
                          (unsigned-byte 16))
                write-joypads))
(defun write-joypads (joypads offset value)
  "Puts a value at the specified offset in the joypad."
  (case offset
    (#x0
      ; Once we write, the transmission starts. We could transfer this a bit at
      ; a time, but as of right now, we transfer whole bytes at the
      ; end of the timer.
      (when *debug-joypads*
        (format t "~%~%Wrote 0x~4,'0x to joypad ~D at offset 0x~1,'0x~%"
                value
                (joypad-control-desired-slot (joypads-joy-ctrl joypads))
                offset))

      (setf (joypads-transmission-timer joypads)
            (* .5f0 (* 8 (joypads-joy-baud joypads))))
      (setf (joypads-write-fifo joypads) (list value))
      (when (= value #x81)
        (error "Mem card?~%")))
    (#x8 (setf (joypads-joy-mode joypads)
               value))
    (#xA (setf (joypads-joy-ctrl joypads)
               (word-to-joypad-control joypads value)))
    (#xE (setf (joypads-joy-baud joypads)
               value)))
  value)

; TODO(Samantha): Remove this once skitter controls this inputs.
(declaim (boolean *skip*))
(defparameter *skip* t)

(declaim (ftype (function (controller)
                          list)
                make-transmission-queue))
(defun make-transmission-queue (controller)
  (if (/= #xDEAD (controller-id controller))
    (progn
     (setf *skip* (not *skip*))
     (list #xFF (ldb (byte 8 0) (controller-id controller))
           (ldb (byte 8 8) (controller-id controller))
           ; Down on the joypad?
           (if *skip* #xFF #xBF)
           ; Attempt to press x each alternating call?
           (if *skip* #xBF #xFF)))
    (list #xFF)))

(declaim (ftype (function (joypads))
                send-and-receive-byte))
(defun send-and-receive-byte (joypads)
  "Moves a byte from the console to the controller and vice versa. Additionally
   does all the house keeping work involved in doing so, such as flagging for
   an irq to be raised if applicable."

  ; If the console isn't trying to send any data down to the controller/memory
  ; card, this function should essentially be a nop.
  (when (car (joypads-write-fifo joypads))
    ; TODO(Samantha): As far as I'm aware, this is a FIFO, but it only ever
    ; holds one byte?
    (setf (joypads-write-fifo joypads) (list))
    ; The receive FIFO can't be empty
    (setf (joypad-status-receive-queue-empty (joypads-joy-stat joypads)) nil)

    ; None of this matters unless the joypad output is actually enabled.
    (when (joypad-control-output-enabled (joypads-joy-ctrl joypads))
      (let ((controller
             (aref (joypads-controllers joypads)
                   (joypad-control-desired-slot (joypads-joy-ctrl joypads)))))
        ; (error "We ever get here?~%")

        ; We need to start a new transmission in the controller if there is no
        ; pending data.
        (unless (car (controller-transmission-queue controller))
          (setf (controller-transmission-queue controller)
                (make-transmission-queue controller)))

        ; Every time we send data, we get data back, so grab it and move the
        ; transmission queue along.
        (setf (joypads-received-from-controller joypads)
              (car (controller-transmission-queue controller)))

        (setf (controller-transmission-queue controller)
              (cdr (controller-transmission-queue controller)))

        ; So long as the controller has more data to send down to the console,
        ; it will pull ack low in order to notify it.
        (when (car (controller-transmission-queue controller))
          (setf (controller-acknowledge controller) :low)
          (when *debug-joypads*
            (format t "Ack goes low~%"))
          ; TODO(Samantha): This is surely wrong.
          (setf (controller-ack-timer controller) 5f0)

          (when (joypad-control-interrupt-on-acknowledge
                 (joypads-joy-ctrl joypads))
            (when *debug-joypads*
              (format t "irq-7 signalled.~%"))
            (setf (joypad-status-has-irq-7 (joypads-joy-stat joypads)) t))))))
  (values))

(declaim (ftype (function (joypads (unsigned-byte 16))
                          single-float)
                cpu-clocks-to-joypad-clocks))
(defun cpu-clocks-to-joypad-clocks (joypads cpu-clocks)
  "Converts cpu clock ticks into joypad clock ticks."
  ; TODO(Samantha): Implement.
  (declare (ignore joypads))
  (* 1.0 cpu-clocks))

(declaim (ftype (function (controller single-float))
                tick-controller))
(defun tick-controller (controller joypad-clocks)
  (when (> (controller-ack-timer controller) 0)
    (decf (controller-ack-timer controller) joypad-clocks)
    (when (<= (controller-ack-timer controller) 0)
      (when *debug-joypads*
        (format t "Ack goes high.~%"))
      (setf (controller-acknowledge controller) :high)))

  (values))

(declaim (ftype (function (joypads (unsigned-byte 16)))
                tick-joypads))
(defun tick-joypads (joypads cpu-clocks)
  "Step joypad clocks forward according to specified number of cpu clocks."
  (decf (joypads-transmission-timer joypads)
        (cpu-clocks-to-joypad-clocks joypads cpu-clocks))

  (tick-controller
   (aref (joypads-controllers joypads)
         (joypad-control-desired-slot (joypads-joy-ctrl joypads)))
   (cpu-clocks-to-joypad-clocks joypads cpu-clocks))

  (when (< (joypads-transmission-timer joypads) 0f0)
    (setf (joypads-transmission-timer joypads) 0f0)
    (send-and-receive-byte joypads)
    (when (and (not (joypads-fired-interrupt joypads))
               (joypad-status-has-irq-7 (joypads-joy-stat joypads)))
      (setf (joypads-fired-interrupt joypads) t)
      (when *debug-joypads*
        (format t "joy irq. baud: ~A~%" (joypads-joy-baud joypads)))
      (funcall (joypads-exception-callback joypads))))
  (values))
