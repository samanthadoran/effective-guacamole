(in-package :psx-console)

(declaim (optimize (speed 3) (safety 1)))

(defconstant mirror-size #x20000000)
(defconstant kuseg-base #x00000000)
(defconstant kseg0-base #x80000000)
(defconstant kseg1-base #xA0000000)
(defconstant ram-begin-kseg1 #xA0000000)
(defconstant ram-size #x200000)
(defconstant bios-begin-address-kseg1 #xBFC00000)
(defconstant memory-control-begin #x1F801000)
(defconstant memory-control-size 36)
(defconstant expansion-base-1-address #x1F000000)
(defconstant expansion-base-2-address #x1F802000)
(defconstant ram-size-begin #x1F801060)
(defconstant ram-size-size 4)

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

; TODO(Samantha): These cases don't work because the cpu writes words as a
; series of bytes... Fix this? Means having 3 functions in cpu for memory read
; and set or something a bit more elegant.

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 8))
                load-byte*))
(defun load-byte* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (cond
    ; BIOS
    ((in-range bios-begin-address-kseg1
               (array-dimension (psx-bios-rom psx) 0)
               address)
     (aref (psx-bios-rom psx) (- address bios-begin-address-kseg1)))
    ; RAM
    ((in-range ram-begin-kseg1 ram-size address)
     (aref (psx-ram psx) (- address ram-begin-kseg1)))
    ; Unimplemented.
    (t (progn (format t "Reads to 0x~8,'0X are unimplemented~%" address) 0))))

; TODO(Samantha): Figure out a way to fix this shadowing.
(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8))
        write-byte*))
(defun write-byte* (psx address value)
  (cond
    ((in-range memory-control-begin memory-control-size address)
     (cond
       ; Expansion base 1 register
       ((= address memory-control-begin)
        (when (/= value expansion-base-1-address)
          (format t "Unexpected value for expansion-base-1-address, got 0x~8,'0x, expected 0x~8,'0x!~%" value expansion-base-1-address))
        value)
       ((= address (+ memory-control-begin 4))
        (when (/= value expansion-base-2-address)
          (format t "Unexpected value for expansion-base-2-address, got 0x~8,'0x, expected 0x~8,'0x!~%" value expansion-base-2-address))
        value)
       ((= address (+ memory-control-begin #x10))
        (format t "Wrote 0x~8,'0x to bios delay/size!~%" value)
        value)
       (t (progn (format t "Unexpected write of 0x~8,'0x! to Memory Control at 0x~8,'0x!~%" value address) value))))
    ((in-range ram-size-begin ram-size-size address)
     (format t "Wrote 0x~8,'0x to ram size!~%" value)
     value)
    ; RAM
    ((in-range ram-begin-kseg1 ram-size address)
     (format t "Wrote 0x~8,'0x to ram(0x~8,'0X)!~%" value address)
     (setf
      (aref (psx-ram psx) (- address ram-begin-kseg1))
      value))
    ; Unimplemented.
    (t (progn (format t "Writes to 0x~8,'0X are unimplemented!~%" address) value))))

(declaim (ftype (function (psx) function) map-memory))
(defun map-memory (psx)
  (setf
   (psx-cpu:cpu-memory-get (psx-cpu psx))
   (lambda (address) (load-byte* psx address)))
  (setf
   (psx-cpu:cpu-memory-set (psx-cpu psx))
   (lambda (address value) (write-byte* psx address value))))
