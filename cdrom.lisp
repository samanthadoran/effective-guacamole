(defpackage #:psx-cdrom
  (:nicknames #:cdrom)
  (:use :cl)
  (:export #:cdrom #:make-cdrom #:read-cdrom-registers #:write-cdrom-registers))

(in-package :psx-cdrom)

(declaim (optimize (speed 3) (safety 1)))

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

(declaim (ftype (function (status-register) (unsigned-byte 8))
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

(declaim (ftype (function (interrupt-enable) (unsigned-byte 8))
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
  (interrupts-pending
   (make-array 11 :element-type 'boolean :initial-element nil)
   :type (simple-array boolean (11)))
  (status (make-status-register) :type status-register)
  (interrupt-enable (make-interrupt-enable) :type interrupt-enable))

(declaim (ftype (function ((unsigned-byte 8) cdrom)
                          (unsigned-byte 8))
                acknowledge-interrupts))
(defun acknowledge-interrupts (word cdrom)
  (when (/= (ldb (byte 3 0) word) 7)
    (error "We're not clearing all the irq bits? Uhhh....Word is #x~8,'0x~%" word))
  (loop for i from 1 to 7
    do (setf (aref (cdrom-interrupts-pending cdrom) i) nil))
  (when (ldb-test (byte 1 3) word)
    (setf (aref (cdrom-interrupts-pending cdrom) 8) nil))
  (when (ldb-test (byte 1 4) word)
    (setf (aref (cdrom-interrupts-pending cdrom) 10) nil))
  ; Acking the interrupts clears the response fifo.
  ; TODO(Samantha): When you ack all of them, or on any ack?
  (when (ldb-test (byte 8 0) word)
    (setf (status-register-response-fifo-is-empty (cdrom-status cdrom)) t))
  word)

(declaim (ftype (function ((unsigned-byte 8) cdrom)
                          (unsigned-byte 8))
                word-to-interrupt-flag))
(defun word-to-interrupt-flag (word cdrom)
  (when (ldb-test (byte 5 0) word)
    (acknowledge-interrupts word cdrom))
  (when (ldb-test (byte 1 6) word)
    (setf (status-register-parameter-fifo-is-empty (cdrom-status cdrom)) t)
    (setf (status-register-parameter-fifo-is-full (cdrom-status cdrom)) nil))
  word)

(declaim (ftype (function (cdrom (unsigned-byte 2))
                          (unsigned-byte 8))
                read-cdrom-registers))
(defun read-cdrom-registers (cdrom offset)
  (case offset
    (0 (status-register-to-word (cdrom-status cdrom)))
    ; (1)
    ; (2)
    (3 (case (status-register-index (cdrom-status cdrom))
         (0 (interrupt-enable-to-word (cdrom-interrupt-enable cdrom)))
         (2 (interrupt-enable-to-word (cdrom-interrupt-enable cdrom)))
         (otherwise (error "Unhandled read from cdrom offset 3. We only handle index 0 and 2 for now.~%"))))
    (otherwise (error "Unhandled cdrom read at offset #x~1,'0x with index #x~1,'0x~%" offset (status-register-index (cdrom-status cdrom))))))

(declaim (ftype (function (cdrom (unsigned-byte 2) (unsigned-byte 8))
                          (unsigned-byte 8))
                write-cdrom-registers))
(defun write-cdrom-registers (cdrom offset value)
  (case offset
    (0 (word-to-status-register value (cdrom-status cdrom)))
    ; (1)
    (2
     (case (status-register-index (cdrom-status cdrom))
       ; Parameter FIFO.
       ; (0)
       (1
        (setf (cdrom-interrupt-enable cdrom)
              (word-to-interrupt-enable value)))
       (otherwise (error "Got a write to cdrom offset #x2 with ~
                          index #x~1,'0x. Only Offset #x1 is handled for now.~%"
                         (status-register-index (cdrom-status cdrom))))))
    (3
     (case (status-register-index (cdrom-status cdrom))
       (1 (word-to-interrupt-flag value cdrom))
       (otherwise (error "Got a write to cdrom offset #x3 with ~
                        index #x~1,'0x. Only Offsets #x1 are handled for now.~%"
                         (status-register-index (cdrom-status cdrom))))))
    (otherwise (error "Unhandled cdrom write of #x~2,'0x at offset #x~1,'0x with index #x~1,'0x~%"
                      value offset (status-register-index (cdrom-status cdrom)))))
  value)
