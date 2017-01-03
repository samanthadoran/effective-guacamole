(in-package :psx-console)

(declaim (optimize (speed 3) (safety 1)))

(defconstant mirror-size #x20000000)
(defconstant kuseg-base #x00000000)
(defconstant kseg0-base #x80000000)
(defconstant kseg1-base #xA0000000)
(defconstant bios-begin-address-kseg1 #xBFC00000)

(declaim (ftype (function ((unsigned-byte 32)
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          boolean) in-range))
(defun in-range (base size place)
  (and (>= place base) (< place (+ base size))))

(declaim (ftype (function ((unsigned-byte 32)) keyword) determine-segment))
(defun determine-segment (address)
  (cond
    ((in-range kuseg-base mirror-size address) :kuseg)
    ((in-range kseg0-base mirror-size address) :kseg0)
    ((in-range kseg1-base mirror-size address) :kseg1)
    (t :invalid-segment)))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 8)) load-byte))
(defun load-byte (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (format t "Segment of address 0x~X is ~A~%"
          address (determine-segment address))
  (cond
    ; BIOS
    ((in-range bios-begin-address-kseg1
               (array-dimension (psx-bios-rom psx) 0)
               address)
     (aref (psx-bios-rom psx) (- address bios-begin-address-kseg1)))
    ; Unimplemented.
    (t (progn (format t "Reads to 0x~X are unimplemented~%" address) 0))))

(declaim (ftype (function (psx) function) map-memory))
(defun map-memory (psx)
  (setf
   (psx-cpu:cpu-memory-get (psx-cpu psx))
   (lambda (address) (load-byte psx address))))
