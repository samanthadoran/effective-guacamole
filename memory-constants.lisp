; TODO(Samantha): Figure out a better name for this file. memory-helpers?

(defpackage #:memory-constants
  (:nicknames #:memory)
  (:use :cl)
  (:export #:mask-address #:determine-segment #:in-range
           #:kuseg-base #:kseg0-base #:kseg1-base
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

(defconstant kuseg-base #x00000000)
(defconstant kseg0-base #x80000000)
(defconstant kseg1-base #xA0000000)
(defconstant ram-begin #x00000000)
(defconstant ram-size (* 4 #x200000))
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
                          (unsigned-byte 32))
                mask-address))
(defun mask-address (address)
  "While the PSX has segments of memory, there is no proper virtual memory and
   we only really care about the segment if it's out of a cached region.
   Performs a bit mask of the address so we can access items in mirrored areas
   without too much hassle."
  ; TODO(Samantha): This could certainly be done with a lookup table, but I'm
  ; not certain it would be clearer.
  (cond
    ; If bits [31:29] are greater than 6, we're in kseg2. The only things here
    ; are some io registers and other miscelanneous things, no need to mask off
    ; bits.
    ((>= (ldb (byte 3 29) address) 6)
     address)
    ; If bits [31:29] are equal to 5, we're in kseg1
    ((= (ldb (byte 3 29) address) 5)
     (ldb (byte 29 0) address))
    ; If bits [31:29] are equal to 4, we're in kseg0
    ((= (ldb (byte 3 29) address) 4)
     (ldb (byte 31 0) address))
    ; Otherwise, we're in kuseg
    (t address)))

(declaim (ftype (function ((unsigned-byte 32))
                          keyword)
                determine-segment))
(defun determine-segment (address)
  (cond
    ; If bits [31:29] are greater than 6, we're in kseg2. The only things here
    ; are some io registers and other miscelanneous things, no need to mask off
    ; bits.
    ((>= (ldb (byte 3 29) address) 6)
     :kseg2)
    ; If bits [31:29] are equal to 5, we're in kseg1
    ((= (ldb (byte 3 29) address) 5)
     :kseg1)
    ; If bits [31:29] are equal to 4, we're in kseg0
    ((= (ldb (byte 3 29) address) 4)
     :kseg0)
    ; Otherwise, we're in kuseg
    (t :kuseg)))

(declaim (ftype (function ((unsigned-byte 32)
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          boolean) in-range))
(defun in-range (base size place)
  (and (>= place base) (< place (+ base size))))
