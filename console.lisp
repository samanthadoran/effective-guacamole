(defpackage #:psx-console
  (:nicknames #:psx)
  (:use :cl :psx-cpu)
  (:export #:make-psx))

(in-package :psx-console)
(declaim (optimize (speed 3) (safety 1)))

(defstruct psx
  "A model psx"
  (cpu (psx-cpu:make-cpu) :type psx-cpu::cpu)
  (bios-rom
   (make-array #x8000 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (#x8000))))
