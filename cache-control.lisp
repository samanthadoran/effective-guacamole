(defpackage #:psx-cache-control
  (:nicknames #:cache-control)
  (:use :cl)
  (:export #:cache-control #:make-cache-control #:write-cache-control
           #:read-cache-control #:cache-control-code-cache-enabled
           #:cache-control-scratchpad-enable-1))

(in-package :psx-cache-control)
(declaim (optimize (speed 3) (safety 0)))

(defstruct cache-control
  (0-2-unknown 0 :type (unsigned-byte 3))
  (scratchpad-enable-1 nil :type boolean)
  (4-5-unknown 0 :type (unsigned-byte 2))
  (scratchpad-enable-2 nil :type boolean)
  (8-unknown 0 :type (unsigned-byte 1))
  (crash-if-code-cache-enabled nil :type boolean)
  (code-cache-enabled nil :type boolean))

(declaim (ftype (function (cache-control (unsigned-byte 32)))
                write-cache-control))
(defun write-cache-control (cache-control value)
  (setf (cache-control-0-2-unknown cache-control)
        (ldb (byte 0 3) value))
  (setf (cache-control-scratchpad-enable-1 cache-control)
        (ldb-test (byte 3 1) value))
  (setf (cache-control-4-5-unknown cache-control)
        (ldb (byte 4 2) value))
  (setf (cache-control-scratchpad-enable-2 cache-control)
        (ldb-test (byte 7 1) value))
  (setf (cache-control-8-unknown cache-control)
        (ldb (byte 8 1) value))
  (setf (cache-control-crash-if-code-cache-enabled cache-control)
        (ldb-test (byte 9 1) value))
  (setf (cache-control-code-cache-enabled cache-control)
        (ldb-test (byte 10 1) value))
  (when (and (ldb-test (byte 1 11) value)
             (ldb-test (byte 1 9) value))
    (error "Cache control has set both code cache and the crash bit!~%"))
  (values))

(declaim (ftype (function (cache-control)
                          (unsigned-byte 32))
                read-cache-control))
(defun read-cache-control (cache-control)
  (logior
   (ash (cache-control-0-2-unknown cache-control) 0)
   (ash (if (cache-control-scratchpad-enable-1 cache-control)
          1
          0)
        3)
   (ash (cache-control-4-5-unknown cache-control) 4)
   (ash (if (cache-control-scratchpad-enable-2 cache-control)
          1
          0)
        7)
   (ash (cache-control-8-unknown cache-control) 8)
   (ash (if (cache-control-crash-if-code-cache-enabled cache-control)
          1
          0)
        9)
   (ash (if (cache-control-code-cache-enabled cache-control)
          1
          0)
        10)))
