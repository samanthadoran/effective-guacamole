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
  ; The controller wants more data so that it can finish sending its own.
  (has-irq-7 nil :type boolean)
  (baudrate-timer 0 :type (unsigned-byte 21)))

(declaim (ftype (function (joypad-status)
                          (unsigned-byte 32))
                joypad-status-to-word))
(defun joypad-status-to-word (joy-stat)
  "Converts joypad-status from a structure to its binary representation."
  (logior #x00000000
          (ash (if (joypad-status-transfer-ready joy-stat) 1 0) 0)
          ; The PSX actually has this as not empty, so just invert the value.
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
  "Converts joypad-mode from a structure to its binary representation."
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
   :parity-type (ecase (ldb (byte 1 5) value)
                       (0 :even)
                       (1 :odd))
   :clock-output-polarity (ecase (ldb (byte 1 8) value)
                                 (0 :normal)
                                 (1 :inverse))))

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
  "Converts joypad-control from a structure to its binary representation."
  (logior #x0000
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 0)
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 1)
          (ash (if (joypad-control-transfer-enabled joy-control) 1 0) 2)
          (ash (joypad-control-receive-interrupt-mode joy-control) 8)
          (ash (if (joypad-control-transfer-interrupt-enable joy-control) 1 0) 10)
          (ash (if (joypad-control-receive-interrupt-enable joy-control) 1 0) 11)
          (ash (if (joypad-control-ack-interrupt-enable joy-control) 1 0) 12)
          (ash (joypad-control-desired-slot-number joy-control) 13)))

(defstruct controller
  "Hold the information related to a specific controller and its current state."
  ; TODO(Samantha): This is currently just the ID for a Digital Pad, it
  ; should be changeable.
  (id #x5A41 :type (unsigned-byte 16))
  ; TODO(Samantha): Hook this up to skitter and use an actual controller.
  (button-status-callback
   (lambda () #x0000)
   :type (function () (unsigned-byte 16))))

(defun make-receive-queue (controller)
  "Makes a response FIFO from a controller."
  ; TODO(Samantha): This response should be variably sized depending
  ; on the type of controller we are dealing with.
  (let ((id (controller-id controller))
        (buttons (funcall (controller-button-status-callback controller))))
    (list #xFF
          (ldb (byte 8 0) id)
          (ldb (byte 8 8) id)
          (ldb (byte 8 0) buttons)
          (ldb (byte 8 8) buttons))))

(defstruct joypads
  "Simple container for the controllers and memory cards."
  ; We only want to fire interrupts on the rising edge, so keep a flag.
  (fired-interrupt nil :type boolean)
  ; TODO(Samantha): Actually properly switch between these.
  (controller-1 (make-controller) :type controller)
  (controller-2 (make-controller) :type controller)
  ; We need two items here because double reading the receive queue should not
  ; just move us further into the response.
  (current-receive-item #xFF :type (unsigned-byte 8))
  (receive-queue (list) :type list)
  ; Offset #x4, read only.
  (joy-stat (make-joypad-status) :type joypad-status)
  ; Offset #x8, read/write.
  (joy-mode (make-joypad-mode) :type joypad-mode)
  ; Offset #xA, read/write.
  (joy-ctrl (make-joypad-control) :type joypad-control)
  ; Offset #xE, read/write.
  (joy-baud #x88 :type (unsigned-byte 16))
  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8))))

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
    (setf (joypads-fired-interrupt joypads)
          nil)
    (setf (joypad-status-has-irq-7 (joypads-joy-stat joypads))
          nil)
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
  "Reads a value from a specified offset in the joypad."
  (let ((value #00000000))
    (setf
     value
     (ecase offset
            ; Receive data from controller
            (0 (setf (joypad-status-receive-queue-empty (joypads-joy-stat joypads))
                     t)
               (joypads-current-receive-item joypads))
            (4 (joypad-status-to-word (joypads-joy-stat joypads)))
            (8 (joypad-mode-to-word (joypads-joy-mode joypads)))
            (#xA (joypad-control-to-word (joypads-joy-ctrl joypads)))
            (#xE (joypads-joy-baud joypads))))
    (when *debug-joypads*
      (format t "Read 0x~8,'0x from joypads at offset 0x~1,'0x~%" value offset))
    value))

(declaim (ftype (function (joypads (unsigned-byte 4) (unsigned-byte 16))
                          (unsigned-byte 16))
                write-joypads))
(defun write-joypads (joypads offset value)
  "Puts a value at the specified offset in the joypad."
  (when *debug-joypads*
    (format t "Wrote 0x~4,'0x to joypads at offset 0x~1,'0x~%" value offset))
  (case offset
    (#x0
      ; We finished the previous response and need to start a new one.
      (unless (car (joypads-receive-queue joypads))
        (setf (joypads-receive-queue joypads)
              (make-receive-queue (joypads-controller-1 joypads))))
      ; Keep track of the current item in the receive queue
      (setf (joypads-current-receive-item joypads)
            (car (joypads-receive-queue joypads)))
      ; Move through the response to the next item.
      (setf (joypads-receive-queue joypads)
            (cdr (joypads-receive-queue joypads)))
      ; Writing to TX will get us data no matter what.
      (setf (joypad-status-receive-queue-empty (joypads-joy-stat joypads))
            nil)
      ; As long as there is more left in the response, we fire IRQ7 to ask
      ; for more data.
      (when (car (joypads-receive-queue joypads))
        (setf (joypad-status-has-irq-7 (joypads-joy-stat joypads))
              t)))
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
  ; TODO(Samantha): Not certain I understand the interrupt conditions
  ; for the controller. Does it just wait until the console reads, or does
  ; pressing a button on the controller actually trigger an interrupt?
  ; This is edge triggered, so we keep a status flag in joypads to determine
  ; whether or not we fired it yet.
  (when (and (not (joypads-fired-interrupt joypads))
             (joypad-status-has-irq-7 (joypads-joy-stat joypads)))
    (setf (joypads-fired-interrupt joypads)
          t)
    (funcall (joypads-exception-callback joypads)))
  (values))
