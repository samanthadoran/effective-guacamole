(defpackage #:psx-coprocessor0
  (:nicknames #:cop0)
  (:use :cl)
  (:export #:cop0 #:make-cop0
           #:cop0-status-register
           #:cop0-cause-register
           #:cop0-epc-register))

(in-package :psx-coprocessor0)

(declaim (optimize (speed 3) (safety 1)))

(defstruct cop0
  "R3000 Coprocessor 0"
  (status-register 0 :type (unsigned-byte 32))
  (cause-register 0 :type (unsigned-byte 32))
  (epc-register 0 :type (unsigned-byte 32)))
