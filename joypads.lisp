(defpackage #:psx-joypads
  (:nicknames #:joypad)
  (:use :cl)
  (:export #:joypads #:make-joypads #:read-joypads #:write-joypads
           #:joypads-exception-callback #:tick-joypads))

(in-package :psx-joypads)

(declaim (optimize (speed 3) (safety 1)))

(declaim (boolean *debug-joypads*))
(defparameter *debug-joypads* t)

(defstruct joypad-status
  (transfer-ready t :type boolean)
  (receive-queue-empty t :type boolean)
  (transfer-ready-2 t :type boolean)
  (receive-parity-error nil :type boolean)
  (ack-input-level :high :type keyword)
  (has-irq-7 nil :type boolean)
  (baudrate-timer 0 :type (unsigned-byte 21)))

(declaim (ftype (function (joypad-status)
                          (unsigned-byte 32))
                joypad-status-to-word))
(defun joypad-status-to-word (joy-stat)
  (logior #x00000000
          (ash (if (joypad-status-transfer-ready joy-stat) 1 0) 0)
          (ash (if (joypad-status-receive-queue-empty joy-stat) 0 1) 1)
          (ash (if (joypad-status-transfer-ready-2 joy-stat) 1 0) 2)
          (ash (if (joypad-status-receive-parity-error joy-stat) 1 0) 3)
          (ash (ecase (joypad-status-ack-input-level joy-stat)
                      (:high 0)
                      (:low 1))
               7)
          (ash (if (joypad-status-has-irq-7 joy-stat) 1 0) 9)
          (ash (joypad-status-baudrate-timer joy-stat) 11)))

(defstruct joypad-mode
  (baudrate-reload-factor 0 :type (unsigned-byte 2))
  (character-length 5 :type (integer 5 8))
  (parity-enabled nil :type boolean)
  (parity-type :even :type keyword)
  (clock-output-polarity :normal :type keyword))

(declaim (ftype (function (joypad-mode)
                          (unsigned-byte 16))
                joypad-mode-to-word))
(defun joypad-mode-to-word (joy-mode)
  (logior #x0000
          (ash (max 1 (joypad-mode-baudrate-reload-factor joy-mode)) 0)
          (ash (- (joypad-mode-character-length joy-mode) 5) 2)
          (ash (if (joypad-mode-parity-enabled joy-mode) 1 0) 4)
          (ash
           (ecase (joypad-mode-parity-type joy-mode)
                  (:even 0)
                  (:odd 1))
           5)
          (ash
           (ecase (joypad-mode-clock-output-polarity joy-mode)
                  (:normal 0)
                  (:inverse 1))
           8)))

(declaim (ftype (function ((unsigned-byte 16))
                          joypad-mode)
                word-to-joypad-mode))
(defun word-to-joypad-mode (value)
  (make-joypad-mode
   :baudrate-reload-factor (max 1 (ldb (byte 2 0) value))
   :character-length (+ 5 (ldb (byte 2 2) value))
   :parity-enabled (ldb-test (byte 1 4) value)
   :parity-type (ecase (ldb (byte 1 5) value) (0 :even) (1 :odd))
   :clock-output-polarity (ecase (ldb (byte 1 8) value) (0 :normal) (1 :inverse))))

(defstruct joypad-control
  (transfer-enabled nil :type boolean)
  (joypad-output :high :type keyword)
  ; TODO(Samantha): Understand this field.
  (receive-enable 0 :type (unsigned-byte 1))
  (receive-interrupt-mode 0 :type (unsigned-byte 2))
  (transfer-interrupt-enable nil :type boolean)
  (receive-interrupt-enable nil :type boolean)
  (ack-interrupt-enable nil :type boolean)
  (desired-slot-number 0 :type (unsigned-byte 1)))

(declaim (ftype (function (joypad-control)
                          (unsigned-byte 16))
                joypad-control-to-word))
(defun joypad-control-to-word (joy-control)
  (logior #x0000
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 0)
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 1)
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 2)
          (ash (joypad-control-receive-interrupt-mode joy-control) 8)
          (ash (if (joypad-control-transfer-interrupt-enable joy-control) 1 0) 10)
          (ash (if (joypad-control-receive-interrupt-enable joy-control) 1 0) 11)
          (ash (if (joypad-control-ack-interrupt-enable joy-control) 1 0) 12)
          (ash (joypad-control-desired-slot-number joy-control) 13)))

(defstruct joypads
  "Simple container for the controllers and memory cards."
  (joy-stat (make-joypad-status) :type joypad-status)
  (joy-mode (make-joypad-mode) :type joypad-mode)
  (joy-ctrl (make-joypad-control) :type joypad-control)
  (joy-baud #x88 :type (unsigned-byte 16))
  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8))))

(declaim (ftype (function (joypads (unsigned-byte 16))
                          joypad-control)
                word-to-joypad-control))
(defun word-to-joypad-control (joypads value)
  ; TODO(Samantha): Ack interrupt.
  (when (ldb-test (byte 1 4) value)
    (when *debug-joypads*
      (format t "Should be acking an interrupt..?~%"))
    (setf
     (joypad-status-has-irq-7 (joypads-joy-stat joypads))
     nil)
    (setf
     (joypad-status-receive-parity-error (joypads-joy-stat joypads))
     nil))
  ; TODO(Samantha): Reset most (all?) joypad registers to 0
  (when (ldb-test (byte 1 6) value)
    (when *debug-joypads*
      (format t "Should be resetting joypad registers..?~%"))
    nil)
  (make-joypad-control
   :transfer-enabled (ldb-test (byte 1 0) value)
   :joypad-output (ecase (ldb (byte 1 1) value) (0 :high) (1 :low))
   :receive-enable (ldb (byte 1 2) value)
   :receive-interrupt-mode (ldb (byte 2 8) value)
   :transfer-interrupt-enable (ldb-test (byte 1 10) value)
   :receive-interrupt-enable (ldb-test (byte 1 11) value)
   :ack-interrupt-enable (ldb-test (byte 1 12) value)
   :desired-slot-number (ldb (byte 1 13) value)))

(declaim (ftype (function (joypads (unsigned-byte 4))
                          (unsigned-byte 32))
                read-joypads))
(defun read-joypads (joypads offset)
  ; TODO(Samantha): Implement.
  ; TODO(Samantha): Fix this hacky debug.
  (when *debug-joypads*
    (format t "Read 0x~8,'0x to joypads at offset 0x~1,'0x~%" (ecase offset
           ; Receive data from controller
           (0 #xFFFFFFFF)
           ; TODO(Samantha): Actually reading this causes a hang. FIXME.
           (4 (joypad-status-to-word (joypads-joy-stat joypads)) #xFFFFFFFF)
           (8 (joypad-mode-to-word (joypads-joy-mode joypads)))
           (#xA (joypad-control-to-word (joypads-joy-ctrl joypads)))
           (#xE (joypads-joy-baud joypads))) offset))
  (ecase offset
         ; Receive data from controller
         (0 #xFFFFFFFF)
         ; TODO(Samantha): Actually reading this causes a hang. FIXME.
         (4 (joypad-status-to-word (joypads-joy-stat joypads)) #xFFFFFFFF)
         (8 (joypad-mode-to-word (joypads-joy-mode joypads)))
         (#xA (joypad-control-to-word (joypads-joy-ctrl joypads)))
         (#xE (joypads-joy-baud joypads))))

(declaim (ftype (function (joypads (unsigned-byte 4) (unsigned-byte 16))
                          (unsigned-byte 16))
                write-joypads))
(defun write-joypads (joypads offset value)
  "Puts a value at the specified offset in the joypad."
  ; TODO(Samantha): Implement.
  (when *debug-joypads*
    (format t "Wrote 0x~4,'0x to joypads at offset 0x~1,'0x~%" value offset))
  (case offset
    (#x0 (when *debug-joypads*
           (format t "Wrote 0x~4,'0x to joypads to TX data~%" value)))
    (#x8 (setf (joypads-joy-mode joypads)
               (word-to-joypad-mode value)))
    (#xA (setf (joypads-joy-ctrl joypads)
               (word-to-joypad-control joypads value)))
    (#xE (setf (joypads-joy-baud joypads)
               value)))
  value)

(declaim (ftype (function (joypads (unsigned-byte 16))
                          single-float)
                cpu-clocks-to-joypad-clocks))
(defun cpu-clocks-to-joypad-clocks (joypads cpu-clocks)
  "Converts cpu clock ticks into joypad clock ticks."
  ; TODO(Samantha): Implement.
  (declare (ignore joypads))
  (* 1.0 cpu-clocks))

(declaim (ftype (function (joypads (unsigned-byte 16)))
                tick-joypads))
(defun tick-joypads (joypads cpu-clocks)
  "Steps joypad clocks forward according to specified number of cpu clocks."
  ; TODO(Samantha): Implement.
  (cpu-clocks-to-joypad-clocks joypads cpu-clocks)
  (values))
