(defpackage #:psx-irq
  (:nicknames #:irq)
  (:use :cl)
  (:export #:irq #:make-irq #:raise-interrupt #:read-irq #:write-irq
           #:irq-exception-callback))

(in-package :psx-irq)
(declaim (optimize (speed 3) (safety 1)))

(defstruct irq
  "Simple structure to hold the irq registers and perform bookkeeping on them."
  (status 0 :type (unsigned-byte 16))
  (mask 0 :type (unsigned-byte 16))
  (exception-callback
   (lambda () 0) :type (function () (unsigned-byte 32))))

(declaim (ftype (function (irq keyword)
                          (unsigned-byte 16))
                raise-interrupt))
(defun raise-interrupt (irq interrupt)
  "Takes an interrupt source and performs the bookkeeping required to
   potentially raise an interrupt."
  (let ((interrupt-index
         (case interrupt
           (:vblank #x0)
           (:cdrom #x2)
           (:timer0 #x4)
           (:timer1 #x5)
           (:timer2 #x6)
           (:joypad #x7)
           (otherwise (error "Unrecognized interrupt: ~A~%" interrupt)))))
    (when (ldb-test (byte 1 interrupt-index) (irq-mask irq))
      (setf (irq-status irq)
            (logior (irq-status irq) (ash 1 interrupt-index)))
      ; This calls back to the cpu's trigger-exception method and also checks
      ; if exceptions are disabled by bit 0 of coprocessor 0's status register.
      (funcall (irq-exception-callback irq)))
    interrupt-index))

(declaim (ftype (function (irq (unsigned-byte 8))
                          (unsigned-byte 16))
                read-irq))
(defun read-irq (irq offset)
  "Takes an offset and returns the appropriate slot of irq."
  (case offset
    (0 (irq-status irq))
    (4 (irq-mask irq))
    (otherwise (error "Invalid irq offset #x~1,'0x~%" offset))))

(declaim (ftype (function (irq (unsigned-byte 8) (unsigned-byte 32))
                          (unsigned-byte 16))
                write-irq))
(defun write-irq (irq offset value)
  "Takes an offset and word and sets the value at the requested offset,
   performing required masking on irq-status."
  (case offset
    (0 (setf (irq-status irq)
             (logand (irq-status irq) value #xFFFF)))
    (4 (setf (irq-mask irq) (logand value #xFFFF)))
    (otherwise (error "Invalid irq offset #x~1,'0x~%" offset))))
