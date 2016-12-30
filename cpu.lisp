(defpackage #:psx-cpu
  (:nicknames #:cpu)
  (:use :cl)
  (:export #:make-cpu #:step-cpu))

(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defstruct cpu
  "A model PSX cpu"
  (program-counter 0 :type (unsigned-byte 32))
  (registers
   (make-array 32 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (32)))
  (hi 0 :type (unsigned-byte 32))
  (lo 0 :type (unsigned-byte 32)))

(defstruct instruction
  "PSX instruction"
  (word 0 :type (unsigned-byte 32)))

(declaim (ftype (function ((unsigned-byte 64)) (unsigned-byte 32)) wrap-word))
(defun wrap-word (to-be-wrapped)
  "Takes up to a 64 bit unsigned int and returns the truncated 32 bit
   representation"
  (ldb (byte 32 0) to-be-wrapped))

(declaim (ftype (function (cpu) (unsigned-byte 32)) fetch))
(defun fetch (cpu)
  "Retrieves an instruction for execution as a 32 bit word."
  ; TODO(Samantha): Implement.
  (cpu-program-counter cpu))

(declaim (ftype (function ((unsigned-byte 32)) instruction) decode))
(defun decode (instruction-as-word)
  "Transforms a 32 bit word into an executable instruction."
  ; TODO(Samantha): Implement.
  (make-instruction :word instruction-as-word))

(declaim (ftype (function (cpu instruction) (unsigned-byte 8)) execute))
(defun execute (cpu instruction)
  "Executes a single instruction and returns the number of cycles that this
   took."
  ; TODO(Samantha): Implement.
  (- (cpu-program-counter cpu) (instruction-word instruction)))

(declaim (ftype (function (cpu) (unsigned-byte 8)) step-cpu))
(defun step-cpu (cpu)
  "Steps the cpu through a single fetch decode execute cycle, returning the
   number of cycles it took."
  (let ((instruction (decode (fetch cpu))))
    (setf (cpu-program-counter cpu) (wrap-word (+ 4 (cpu-program-counter cpu))))
    (execute cpu instruction)))
