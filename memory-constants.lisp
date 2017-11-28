; TODO(Samantha): Figure out a better name for this file. memory-helpers?

(defpackage #:memory-constants
  (:nicknames #:memory)
  (:use :cl)
  (:export #:mask-address #:determine-segment #:in-range
           #:ram-begin #:ram-size #:bios-begin-address #:memory-control-begin
           #:memory-control-size #:expansion-1-begin #:expansion-1-size
           #:expansion-2-begin #:expansion-2-size #:ram-size-begin
           #:irq-registers-begin #:irq-registers-size #:cache-control
           #:spu-registers-begin #:spu-registers-size #:timers-begin
           #:timers-size #:dma-registers-begin #:dma-registers-size
           #:gpu-registers-begin #:gpu-registers-size
           #:bios-begin-unmasked-address))

(in-package :memory-constants)
(declaim (optimize (speed 3) (safety 1)))

; The following constants are are all in terms of a masked address unless
; otherwise specified.

; Ram isn't really 8MiB, but the 2MiB is mirrored four times. For simplicity,
; we just say the size is 8MiB and modulo all accesses by 2MiB.
(defconstant ram-begin #x00000000)
(defconstant ram-size (* 4 #x200000))

; It owuld be nice to have a constant BIOS size here, but I'm not certain if all
; psx BIOS are of the same size. An additional unmasked address is used for the
; power on value of the cpu program counter.
(defconstant bios-begin-address #x1FC00000)
(defconstant bios-begin-unmasked-address #xBFC00000)

(defconstant memory-control-begin #x1F801000)
(defconstant memory-control-size 36)

(defconstant expansion-1-begin #x1F000000)
(defconstant expansion-1-size #x80000)

(defconstant expansion-2-begin #x1F802000)
(defconstant expansion-2-size 66)

(defconstant ram-size-begin #x1F801060)

(defconstant irq-registers-begin #x1F801070)
(defconstant irq-registers-size 8)

(defconstant cache-control #xFFFE0130)

(defconstant spu-registers-begin #x1F801C00)
(defconstant spu-registers-size 640)

; Timers seems to be about 0x30 in size, but only has three registers for
; each of the four timers? Weird.
(defconstant timers-begin #x1F801100)
(defconstant timers-size #x30)

(defconstant dma-registers-begin #x1F801080)
(defconstant dma-registers-size #x80)

; TODO(Samantha): Maybe split these into two ranges, one for the readables, one
; for the writables?
(defconstant gpu-registers-begin #x1F801810)
(defconstant gpu-registers-size 8)

(declaim (ftype (function ((unsigned-byte 32))
                          keyword)
                determine-segment))
(defun determine-segment (address)
  "Determines the segment of an address given in virtual terms."
  (case (ldb (byte 3 29) address)
    ; Any count above six and we're in kseg2. While code can't be here, this is
    ; the location where a variety of memory mapped registers live exclusively.
    (7 :kseg2)
    (6 :kseg2)
    (5 :kseg1)
    (4 :kseg0)
    ; Any count lower than 4 and we're just in kuseg.
    (otherwise :kuseg)))

(declaim (ftype (function ((unsigned-byte 32))
                          (unsigned-byte 32))
                mask-address))
(defun mask-address (address)
  "Performs a bitmask of the given address to get an absolute address based
   upon the segment of memory."
  (case (determine-segment address)
    (:kseg2 address)
    (:kseg1 (ldb (byte 29 0) address))
    (:kseg0 (ldb (byte 31 0) address))
    (:kuseg address)
    (otherwise (error "Unreachable!~%"))))

(declaim (ftype (function ((unsigned-byte 32)
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          boolean) in-range))
(defun in-range (base size place)
  (and (>= place base) (< place (+ base size))))
