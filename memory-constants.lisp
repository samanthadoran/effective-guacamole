; TODO(Samantha): Figure out a better name for this file. memory-helpers?

(defpackage #:memory-constants
  (:nicknames #:memory)
  (:use :cl)
  (:export #:mask-address #:determine-segment #:in-range
           #:+ram-begin+ #:+ram-size+ #:+bios-begin-address+ #:+memory-control-begin+
           #:+memory-control-size+ #:+expansion-1-begin+ #:+expansion-1-size+
           #:+expansion-2-begin+ #:+expansion-2-size+ #:+ram-size-begin+
           #:+irq-registers-begin+ #:+irq-registers-size+ #:+cache-control+
           #:+spu-registers-begin+ #:+spu-registers-size+ #:+timers-begin+
           #:+timers-size+ #:+dma-registers-begin+ #:+dma-registers-size+
           #:+gpu-registers-begin+ #:+gpu-registers-size+
           #:+bios-begin-unmasked-address+ #:sign-extend #:sign-extend-byte
           #:wrap-word #:to-signed-byte-32 #:+ram-exception-vector+
           #:+rom-exception-vector+ #:+ram-size-non-mirrored+
           #:+spu-voice-registers-begin+ #:+spu-voice-registers-size+
           #:+spu-control-registers-begin+ #:+spu-control-registers-size+
           #:+cdrom-registers-begin+ #:+cdrom-registers-size+
           #:+joypad-registers-begin+ #:+joypad-registers-size+
           #:write-word-to-byte-array #:write-half-word-to-byte-array
           #:read-word-from-byte-array #:read-half-word-from-byte-array
           #:is-cacheable))

(in-package :memory-constants)
(declaim (optimize (speed 3) (safety 1)))

; The following constants are are all in terms of a masked address unless
; otherwise specified.

; Ram isn't really 8MiB, but the 2MiB is mirrored four times. For simplicity,
; we just say the size is 8MiB and modulo all accesses by 2MiB.
(defconstant +ram-begin+ #x00000000)
(defconstant +ram-size+ (* 4 #x200000))
(defconstant +ram-size-non-mirrored+ #x200000)

; It would be nice to have a constant BIOS size here, but I'm not certain if all
; psx BIOS are of the same size. An additional unmasked address is used for the
; power on value of the cpu program counter.
(defconstant +bios-begin-address+ #x1FC00000)
(defconstant +bios-begin-unmasked-address+ #xBFC00000)

(defconstant +memory-control-begin+ #x1F801000)
(defconstant +memory-control-size+ 36)

(defconstant +expansion-1-begin+ #x1F000000)
(defconstant +expansion-1-size+ #x80000)

(defconstant +expansion-2-begin+ #x1F802000)
(defconstant +expansion-2-size+ 66)

(defconstant +ram-size-begin+ #x1F801060)

(defconstant +irq-registers-begin+ #x1F801070)
(defconstant +irq-registers-size+ 8)

(defconstant +joypad-registers-begin+ #x1F801040)
(defconstant +joypad-registers-size+ #x10)

(defconstant +cache-control+ #xFFFE0130)

(defconstant +spu-registers-begin+ #x1F801C00)
(defconstant +spu-registers-size+ 640)

(defconstant +spu-voice-registers-begin+ #x1F801C00)
(defconstant +spu-voice-registers-size+ #x180)

(defconstant +spu-control-registers-begin+ #x1F801D80)
(defconstant +spu-control-registers-size+ #x40)

(defconstant +data-cache-begin+ #x1F800000)
(defconstant +data-cache-size+ #x400)

; Timers seems to be about 0x30 in size, but only has three registers for
; each of the four timers? Weird.
(defconstant +timers-begin+ #x1F801100)
(defconstant +timers-size+ #x30)

(defconstant +dma-registers-begin+ #x1F801080)
(defconstant +dma-registers-size+ #x80)

; TODO(Samantha): Maybe split these into two ranges, one for the readables, one
; for the writables?
(defconstant +gpu-registers-begin+ #x1F801810)
(defconstant +gpu-registers-size+ 8)

(defconstant +cdrom-registers-begin+ #x1F801800)
(defconstant +cdrom-registers-size+ 4)

(defconstant +rom-exception-vector+ #xBFC00180)
(defconstant +ram-exception-vector+ #x80000080)

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
                          boolean)
                is-cacheable))
(defun is-cacheable (address)
  "Determines whether the given address is cacheable."
  ; The only cacheable locations are kuseg and kseg0, which
  ; occur when [31:29] < 5
  (< (ldb (byte 3 29) address) 5))

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

(declaim (ftype (function ((unsigned-byte 16)) (unsigned-byte 32))
                sign-extend))
(defun sign-extend (to-be-extended)
  (logior to-be-extended
          (if (ldb-test (byte 1 15) to-be-extended)
            ; Left fill 1s
            #xFFFF0000
            ; Left fill 0s
            #x00000000)))

(declaim (ftype (function ((unsigned-byte 8)) (unsigned-byte 32))
                sign-extend-byte))
(defun sign-extend-byte (to-be-extended)
  (logior to-be-extended
          (if (ldb-test (byte 1 7) to-be-extended)
            ; Left fill 1s
            #xFFFFFF00
            ; Left fill 0s
            #x00000000)))

(declaim (ftype (function ((signed-byte 64)) (unsigned-byte 32))
                wrap-word))
(defun wrap-word (to-be-wrapped)
  "Takes up to a 64 bit signed int and returns the truncated 32 bit
     representation."
  (ldb (byte 32 0) to-be-wrapped))

(declaim (ftype (function ((unsigned-byte 32)) (signed-byte 32))
                to-signed-byte-32))
(defun to-signed-byte-32 (to-be-converted)
  "Translates a psx unsigned word into a lisp signed int for easier arithmetic."
  ; If the MSB is set, do the inversions.
  (if (ldb-test (byte 1 31) to-be-converted)
    (* (the (signed-byte 32) -1) (wrap-word (1+ (lognot to-be-converted))))
    to-be-converted))

(declaim (ftype (function ((simple-array (unsigned-byte 8))
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          (unsigned-byte 32))
                write-word-to-byte-array))
(defun write-word-to-byte-array (array offset word)
  (write-half-word-to-byte-array array offset (ldb (byte 16 0) word))
  (write-half-word-to-byte-array array (+ 2 offset) (ldb (byte 16 16) word))
  word)

(declaim (ftype (function ((simple-array (unsigned-byte 8))
                           (unsigned-byte 32)
                           (unsigned-byte 16))
                          (unsigned-byte 16))
                write-half-word-to-byte-array))
(defun write-half-word-to-byte-array (array offset half-word)
  (setf
   (aref array offset)
   (ldb (byte 8 0) half-word))
  (setf
   (aref array (+ 1 offset))
   (ldb (byte 8 8) half-word))
  half-word)

; TODO(Samantha): Consider regions in these functions.
(declaim (ftype (function ((simple-array (unsigned-byte 8)) (unsigned-byte 32))
                          (unsigned-byte 32))
                read-word-from-byte-array))
(defun read-word-from-byte-array (array offset)
  "Performs the necessary shifting to reconstruct a word from a byte-array
       for general use."
  (logior
   (aref array offset)
   (ash (aref array (+ 1 offset)) 8)
   (ash (aref array (+ 2 offset)) 16)
   (ash (aref array (+ 3 offset)) 24)))

(declaim (ftype (function ((simple-array (unsigned-byte 8)) (unsigned-byte 32))
                          (unsigned-byte 16))
                    read-half-word-from-byte-array))
(defun read-half-word-from-byte-array (array offset)
  (logior
   (aref array offset)
   (ash (aref array (+ 1 offset)) 8)))
