(defpackage #:psx-timers
  (:nicknames #:timers)
  (:use :cl)
  (:export #:timers #:make-timers #:read-timers #:write-timers))

(in-package :psx-timers)
(declaim (optimize (speed 3) (safety 1)))

(defstruct timer
  (current-value 0 :type (unsigned-byte 16))
  (mode 0 :type (unsigned-byte 16))
  (target-value 0 :type (unsigned-byte 16))
  ; TODO(Samantha): Make a set of rules for this. Each counter has a different
  ; set of ways it syncs.
  (synchronization-modes nil :type boolean))

(defstruct timers
  (timers
   (make-array 3
               :element-type 'timer
               :initial-contents `(,(make-timer)
                                   ,(make-timer)
                                   ,(make-timer)))
   :type (simple-array timer (3))))

(declaim (ftype (function (timers (unsigned-byte 8))
                          (unsigned-byte 16))
                read-timers))
(defun read-timers (timers offset)
  (let ((timer (aref (timers-timers timers) (ldb (byte 2 4) offset))))
    (case (ldb (byte 4 0) offset)
      (0 (timer-current-value timer))
      (4 (timer-mode timer))
      (8 (timer-target-value timer))
      (otherwise
       (error "Invalid timer register index: #x~1,'0x with timer ~
               number: #x~1,'0x"
              (ldb (byte 4 0) offset) (ldb (byte 2 4) offset))))))

(declaim (ftype (function (timers (unsigned-byte 8) (unsigned-byte 32))
                          (unsigned-byte 16))
                write-timers))
(defun write-timers (timers offset value)
  (let ((timer (aref (timers-timers timers) (ldb (byte 2 4) offset)))
        (value (ldb (byte 16 0) value)))
    (case (ldb (byte 4 0) offset)
      (0 (setf (timer-current-value timer) value))
      (4 (setf (timer-mode timer) value))
      (8 (setf (timer-target-value timer) value))
      (otherwise
       (error "Invalid timer register index: #x~1,'0x with timer ~
                             number: #x~1,'0x"
              (ldb (byte 4 0) offset) (ldb (byte 2 4) offset))))))
