(defpackage #:psx-console
  (:nicknames #:psx)
  (:use :cl :psx-cpu)
  (:export #:make-psx #:load-rom-from-file))

(in-package :psx-console)
(declaim (optimize (speed 3) (safety 1)))

(defstruct psx
  "A model psx"
  (cpu (psx-cpu:make-cpu) :type psx-cpu:cpu)
  (bios-rom
   (make-array #x80000 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (#x80000))))

(defun load-rom-from-file (filepath)
  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
    (let ((rom (make-array (file-length stream)
                           :element-type '(unsigned-byte 8))))
      (read-sequence rom stream)
      rom)))

(declaim (ftype (function (psx)) console-on))
(defun console-on (psx)
  (psx-cpu:power-on (psx-cpu psx))
  (map-memory psx)
  (values))
