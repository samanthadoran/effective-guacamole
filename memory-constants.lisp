(defpackage #:memory-constants
  (:nicknames #:memory)
  (:use :cl)
  (:export #:mirror-size #:kuseg-base #:kseg0-base #:kseg1-base
           #:ram-begin #:ram-size #:bios-begin-address #:memory-control-begin
           #:memory-control-size #:expansion-1-begin #:expansion-1-size
           #:expansion-2-begin #:expansion-2-size #:ram-size-begin
           #:irq-registers-begin #:irq-registers-size #:cache-control
           #:spu-registers-begin #:spu-registers-size #:timers-begin
           #:timers-size #:dma-registers-begin #:dma-registers-size))

(in-package :memory-constants)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): This mirror size is just wrong.
(defconstant mirror-size #x20000000)
(defconstant kuseg-base #x00000000)
(defconstant kseg0-base #x80000000)
(defconstant kseg1-base #xA0000000)
(defconstant ram-begin #x00000000)
(defconstant ram-size (* 4 #x200000))
(defconstant bios-begin-address #x1FC00000)
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
