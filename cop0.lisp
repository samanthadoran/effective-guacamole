(defpackage #:psx-coprocessor0
  (:nicknames #:cop0)
  (:use :cl)
  (:export #:coprocessor0 #:make-coprocessor0
           #:coprocessor0-status-register
           #:coprocessor0-cause-register
           #:coprocessor0-epc-register))

(in-package :psx-coprocessor0)

(declaim (optimize (speed 3) (safety 1)))

(defstruct coprocessor0
  "R3000 Coprocessor 0"
  (status-register 0 :type (unsigned-byte 32))
  (cause-register 0 :type (unsigned-byte 32))
  (epc-register 0 :type (unsigned-byte 32)))
