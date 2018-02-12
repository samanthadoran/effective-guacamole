(defpackage #:psx-cdrom
  (:nicknames #:cdrom)
  (:use :cl)
  (:export #:cdrom #:make-cdrom #:read-cdrom-registers #:write-cdrom-registers
           #:cdrom-exception-callback))

(in-package :psx-cdrom)

(declaim (optimize (speed 3) (safety 1)))

(declaim (boolean *debug-cdrom*))
(defparameter *debug-cdrom* nil)

(defstruct status-register
  ; TODO(Samantha): The flags in this register seem awkwardly redundant to me.
  ; I'm not sure I completely understand.
  (index 0 :type (unsigned-byte 2))
  (xa-adpcm-fifo-has-contents nil :type boolean)
  (parameter-fifo-is-empty t :type boolean)
  (parameter-fifo-is-full nil :type boolean)
  (response-fifo-is-empty t :type boolean)
  (data-fifo-is-empty t :type boolean)
  (busy nil :type boolean))

(declaim (ftype (function (status-register)
                          (unsigned-byte 8))
                status-register-to-word))
(defun status-register-to-word (status-register)
  (logior
   (ash (status-register-index status-register) 0)
   (ash (if (status-register-xa-adpcm-fifo-has-contents status-register) 1 0) 2)
   (ash (if (status-register-parameter-fifo-is-empty status-register) 1 0) 3)
   (ash (if (status-register-parameter-fifo-is-full status-register) 0 1) 4)
   (ash (if (status-register-response-fifo-is-empty status-register) 0 1) 5)
   (ash (if (status-register-data-fifo-is-empty status-register) 0 1) 6)
   (ash (if (status-register-busy status-register) 1 0) 7)))

(declaim (ftype (function ((unsigned-byte 8) status-register)
                          (unsigned-byte 8))
                word-to-status-register))
(defun word-to-status-register (word status-register)
  ; Only the index is writable in the status register, ignore everything else.
  (setf (status-register-index status-register) (ldb (byte 2 0) word)))

(defstruct interrupt-enable
  (int1 nil :type boolean)
  (int2 nil :type boolean)
  (int3 nil :type boolean)
  (int4 nil :type boolean)
  (int5 nil :type boolean))

(declaim (ftype (function (interrupt-enable)
                          (unsigned-byte 8))
                interrupt-enable-to-word))
(defun interrupt-enable-to-word (interrupt-enable)
  (logior
   (ash (if (interrupt-enable-int1 interrupt-enable) 1 0) 0)
   (ash (if (interrupt-enable-int2 interrupt-enable) 1 0) 1)
   (ash (if (interrupt-enable-int3 interrupt-enable) 1 0) 2)
   (ash (if (interrupt-enable-int4 interrupt-enable) 1 0) 3)
   (ash (if (interrupt-enable-int5 interrupt-enable) 1 0) 4)))

(declaim (ftype (function ((unsigned-byte 8))
                          interrupt-enable)
                word-to-interrupt-enable))
(defun word-to-interrupt-enable (word)
  (make-interrupt-enable
   :int1 (ldb-test (byte 1 0) word)
   :int2 (ldb-test (byte 1 1) word)
   :int3 (ldb-test (byte 1 2) word)
   :int4 (ldb-test (byte 1 3) word)
   :int5 (ldb-test (byte 1 4) word)))

(defstruct cdrom
  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8)))
  (parameter-fifo (list) :type list)
  (parameter-fifo-tail (list) :type list)
  (parameter-fifo-length 0 :type (unsigned-byte 8))
  (response-fifo (list) :type list)
  (response-fifo-tail (list) :type list)
  (response-fifo-length 0 :type (unsigned-byte 8))
  (interrupts-pending
   (make-array 11 :element-type 'boolean :initial-element nil)
   :type (simple-array boolean (11)))
  (status (make-status-register) :type status-register)
  (interrupt-enable (make-interrupt-enable) :type interrupt-enable))

(declaim (ftype (function (cdrom (unsigned-byte 8))
                          (unsigned-byte 8))
                acknowledge-interrupts))
(defun acknowledge-interrupts (cdrom word )
  (when (/= (ldb (byte 3 0) word) 7)
    (error "We're not clearing all the irq bits? Uhhh....Word is #x~8,'0x~%"
           word))
  (loop for i from 1 to 7
    do (setf (aref (cdrom-interrupts-pending cdrom) i) nil))
  (when (ldb-test (byte 1 3) word)
    (setf (aref (cdrom-interrupts-pending cdrom) 8) nil))
  (when (ldb-test (byte 1 4) word)
    (setf (aref (cdrom-interrupts-pending cdrom) 10) nil))
  ; Acking the interrupts clears the response fifo.
  ; TODO(Samantha): When you ack all of them, or on any ack?
  (when (ldb-test (byte 8 0) word)
    (clear-response-fifo cdrom))
  word)

(declaim (ftype (function (cdrom))
                clear-parameter-fifo))
(defun clear-parameter-fifo (cdrom)
  (setf (status-register-parameter-fifo-is-empty (cdrom-status cdrom)) t)
  (setf (status-register-parameter-fifo-is-full (cdrom-status cdrom)) nil)
  (setf (cdrom-parameter-fifo cdrom) (list))
  (setf (cdrom-parameter-fifo-tail cdrom) (list))
  (setf (cdrom-parameter-fifo-length cdrom) 0)
  (values))

(declaim (ftype (function (cdrom))
                clear-response-fifo))
(defun clear-response-fifo (cdrom)
  (setf (status-register-response-fifo-is-empty (cdrom-status cdrom)) t)
  (setf (cdrom-response-fifo cdrom) (list))
  (setf (cdrom-response-fifo-tail cdrom) (list))
  (setf (cdrom-response-fifo-length cdrom) 0)
  (values))

(declaim (ftype (function (cdrom (unsigned-byte 8))
                          (unsigned-byte 8))
                word-to-interrupt-flag))
(defun word-to-interrupt-flag (cdrom word)
  (when (ldb-test (byte 5 0) word)
    (acknowledge-interrupts cdrom word))
  (when (ldb-test (byte 1 6) word)
    (clear-parameter-fifo cdrom))
  word)

(declaim (ftype (function (cdrom (unsigned-byte 8))
                          (unsigned-byte 8))
                write-parameter-fifo))
(defun write-parameter-fifo (cdrom word)
  (incf (cdrom-parameter-fifo-length cdrom))
  (setf (status-register-parameter-fifo-is-empty (cdrom-status cdrom)) nil)
  (when (= (cdrom-parameter-fifo-length cdrom) 16)
    (setf (status-register-parameter-fifo-is-full (cdrom-status cdrom)) t))
  (if (not (car (cdrom-parameter-fifo cdrom)))
    (progn
     (setf (cdrom-parameter-fifo cdrom) (list word))
     (setf (cdrom-parameter-fifo-tail cdrom) (cdrom-parameter-fifo cdrom)))
    (progn
     (setf (cdr (cdrom-parameter-fifo-tail cdrom))
           (list word))
     (setf (cdrom-parameter-fifo-tail cdrom)
           (cdr (cdrom-parameter-fifo-tail cdrom)))))
  word)

(declaim (ftype (function (cdrom)
                          (unsigned-byte 8))
                read-response-fifo))
(defun read-response-fifo (cdrom)
  (let ((response 0))
    (when (car (cdrom-response-fifo cdrom))
      (decf (cdrom-response-fifo-length cdrom))
      (setf response (car (cdrom-response-fifo cdrom)))
      (setf (cdrom-response-fifo cdrom) (cdr (cdrom-response-fifo cdrom)))
      (unless (car (cdrom-response-fifo cdrom))
        (clear-response-fifo cdrom)))
    (when *debug-cdrom*
      (format t "Read response #x~2,'0x~%" response))
    response))

(declaim (ftype (function (cdrom (unsigned-byte 8))
                          (unsigned-byte 8))
                write-response-fifo))
(defun write-response-fifo (cdrom word)
  (setf (status-register-response-fifo-is-empty (cdrom-status cdrom)) nil)
  (incf (cdrom-response-fifo-length cdrom))
  (if (not (car (cdrom-response-fifo cdrom)))
    (progn
     (setf (cdrom-response-fifo cdrom) (list word))
     (setf (cdrom-response-fifo-tail cdrom) (cdrom-response-fifo cdrom)))
    (progn
     (setf (cdr (cdrom-response-fifo-tail cdrom))
           (list word))
     (setf (cdrom-response-fifo-tail cdrom)
           (cdr (cdrom-response-fifo-tail cdrom)))))
  word)

(declaim (ftype (function (cdrom)
                          boolean)
                remaining-interrupts))
(defun remaining-interrupts (cdrom)
  (reduce (lambda (x y) (or x y)) (cdrom-interrupts-pending cdrom)))

; TODO(Samantha): I'm not sure why the psx is double sending commands here, it
; might have something to do with the fact that I instantaneously finish
; writing and never set the busy bit? Either way, it seems like every other
; command should be skipped, but test works for now because when it writes #x19
; a second time, the parameter fifo is empty and nothing happens. Fix this so
; this hack isn't required.
; HERE BE DRAGONS.
(defparameter *skip* nil)

(declaim (ftype (function (cdrom (unsigned-byte 8))
                          (unsigned-byte 8))
                write-command-word))
(defun write-command-word (cdrom word)
  (unless (status-register-busy (cdrom-status cdrom))
    (unless (remaining-interrupts cdrom)
      (case word
        (#x1
          (unless *skip*
            (when *debug-cdrom*
              (format t "Command #x1: GetStat~%"))
            ; TODO(Samantha): Spec out the proper cdrom stat register.
            (write-response-fifo cdrom #x10)
            (setf (aref (cdrom-interrupts-pending cdrom) 3) t)
            (funcall (cdrom-exception-callback cdrom)))
          (setf *skip* (not *skip*)))
        (#x19
          (unless *skip*
            (unless (=
                     (the (unsigned-byte 8) (car (cdrom-parameter-fifo cdrom)))
                     #x20)
              (error "Unrecognized test subfunction #x~2,'0x~%"
                     (car (cdrom-parameter-fifo cdrom))))
            (setf *skip* (not *skip*))
            (when *debug-cdrom*
              (format t "Command #x19(#x20): Self Test.~%"))
            (write-response-fifo cdrom #x97)
            (write-response-fifo cdrom #x01)
            (write-response-fifo cdrom #x10)
            (write-response-fifo cdrom #xC2)
            (setf (aref (cdrom-interrupts-pending cdrom) 3) t)
            (funcall (cdrom-exception-callback cdrom))))
        (otherwise (error "Unhandled CDrom command word #x~2,'0x~%" word)))
      (clear-parameter-fifo cdrom)))
  word)

(declaim (ftype (function (cdrom (unsigned-byte 2))
                          (unsigned-byte 8))
                read-cdrom-registers))
(defun read-cdrom-registers (cdrom offset)
  (when *debug-cdrom*
    (format t "Read from cdrom offset #x~1,'0x with index #x~1,'0x~%"
            offset (status-register-index (cdrom-status cdrom))))
  (case offset
    (0 (status-register-to-word (cdrom-status cdrom)))
    (1
      (case (status-register-index (cdrom-status cdrom))
           (1 (read-response-fifo cdrom))
           (otherwise
            (error "Unhandled read from cdrom offset 1 with index #x~1,'0x. ~
                    We only handle index 1 for now.~%"
                   (status-register-index (cdrom-status cdrom))))))
    ; (2)
    (3 (case (status-register-index (cdrom-status cdrom))
         (0 (interrupt-enable-to-word (cdrom-interrupt-enable cdrom)))
         ; TODO(Samantha): Move this out to a function and actually set these
         ; values instead of just putting magic numbers.
         (1 (logior
             (ash (if (remaining-interrupts cdrom) #x3 0) 0)
             (ash 0 3)
             (ash (if (remaining-interrupts cdrom) 0 0) 4)
             (ash #x7 5)))
         (2 (interrupt-enable-to-word (cdrom-interrupt-enable cdrom)))
         (otherwise (error "Unhandled read from cdrom offset 3 with index ~
                            #x~1,'0x. We only handle index 0 and 2 for now.~%"
                           (status-register-index (cdrom-status cdrom))))))
    (otherwise (error "Unhandled cdrom read at offset #x~1,'0x with ~
                       index #x~1,'0x~%"
                      offset (status-register-index (cdrom-status cdrom))))))

(declaim (ftype (function (cdrom (unsigned-byte 2) (unsigned-byte 8))
                          (unsigned-byte 8))
                write-cdrom-registers))
(defun write-cdrom-registers (cdrom offset value)
  (when *debug-cdrom*
    (format t "Write of value #x~2,'0x to cdrom offset #x~1,'0x with ~
               index #x~1,'0x~%"
            value offset (status-register-index (cdrom-status cdrom))))
  (case offset
    (0 (word-to-status-register value (cdrom-status cdrom)))
    (1
      (case (status-register-index (cdrom-status cdrom))
        ; Command word.
        (0
         (write-command-word cdrom value))
        (otherwise (error "Got a write to cdrom offset #x1 with ~
                           index #x~1,'0x. Only Offset #x0 is handled for now.~%"
                          (status-register-index (cdrom-status cdrom))))))
    (2
     (case (status-register-index (cdrom-status cdrom))
       ; Parameter FIFO.
       (0
        (write-parameter-fifo cdrom value))
       (1
        (setf (cdrom-interrupt-enable cdrom)
              (word-to-interrupt-enable value)))
       (otherwise (error "Got a write to cdrom offset #x2 with ~
                          index #x~1,'0x. Only Offset #x1 is handled for now.~%"
                         (status-register-index (cdrom-status cdrom))))))
    (3
     (case (status-register-index (cdrom-status cdrom))
       (1 (word-to-interrupt-flag cdrom value))
       (otherwise (error "Got a write to cdrom offset #x3 with ~
                        index #x~1,'0x. Only Offsets #x1 are handled for now.~%"
                         (status-register-index (cdrom-status cdrom))))))
    (otherwise (error "Unhandled cdrom write of #x~2,'0x at offset #x~1,'0x ~
                       with index #x~1,'0x~%"
                      value offset (status-register-index (cdrom-status cdrom)))))
  value)
