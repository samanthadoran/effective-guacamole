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
(defconstant cache-control #xFFFE0130)

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

(declaim (ftype (function ((simple-array (unsigned-byte 8)) (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
                write-word-to-byte-array))
(defun write-word-to-byte-array (array offset word)
  (setf
   (aref array offset)
   (ldb (byte 8 0) word))
  (setf
   (aref array (+ 1 offset))
   (ldb (byte 8 8) word))
  (setf
   (aref array (+ 2 offset))
   (ldb (byte 8 16) word))
  (setf
   (aref array (+ 3 offset))
   (ldb (byte 8 24) word))
  word)

(declaim (ftype (function ((simple-array (unsigned-byte 8)) (unsigned-byte 32)) (unsigned-byte 32))
                read-word-from-byte-array))
(defun read-word-from-byte-array (array offset)
  (logior
   (aref array offset)
   (ash (aref array (+ 1 offset)) 8)
   (ash (aref array (+ 2 offset)) 16)
   (ash (aref array (+ 3 offset)) 24)))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 8))
                load-byte*))
(defun load-byte* (psx address)
  (declare (ignore psx))
  ; TODO(Samantha): Implement more places, simplify the cond.
  (cond
    ; Unimplemented.
    (t (progn (format t "Byte reads to 0x~8,'0X are unimplemented~%" address) (loop) 0))))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 16))
                load-half-word*))
(defun load-half-word* (psx address)
  (declare (ignore psx))
  ; TODO(Samantha): Implement more places, simplify the cond.
  (cond
    ; Unimplemented.
    (t (progn (format t "Half-word reads to 0x~8,'0X are unimplemented~%" address) (loop) 0))))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 32))
                load-word*))
(defun load-word* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (cond
    ; BIOS
    ((in-range bios-begin-address-kseg1
               (array-dimension (psx-bios-rom psx) 0)
               address)
     (read-word-from-byte-array
      (psx-bios-rom psx) (- address bios-begin-address-kseg1)))
    ; RAM
    ((in-range ram-begin-kseg1 ram-size address)
     (read-word-from-byte-array (psx-ram psx) (- address ram-begin-kseg1)))
    ; Unimplemented.
    (t (progn (format t "Word reads to 0x~8,'0X are unimplemented~%" address) (loop) 0))))

; TODO(Samantha): Figure out a way to fix this shadowing.
(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8))
        write-byte*))
(defun write-byte* (psx address value)
  (declare (ignore psx))
  (cond
    ; Unimplemented.
    (t (progn (format t "Byte writes to 0x~8,'0X are unimplemented!~%" address) (loop) value))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 16)) (unsigned-byte 16))
        write-half-word*))
(defun write-half-word* (psx address value)
  (declare (ignore psx))
  (cond
    ; Unimplemented.
    (t (progn (format t "Half-word writes to 0x~8,'0X are unimplemented!~%" address) (loop) value))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
        write-word*))
(defun write-word* (psx address value)
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
    ((= address ram-size-begin)
     (format t "Wrote 0x~8,'0x to ram size!~%" value)
     value)
    ((= address cache-control)
     (format t "Wrote 0x~8,'0x to cache control!~%" value)
     value)
    ; RAM
    ((in-range ram-begin-kseg1 ram-size address)
     (format t "Wrote 0x~8,'0x to ram(0x~8,'0X)!~%" value address)
     (write-word-to-byte-array (psx-ram psx) (- address ram-begin-kseg1) value))
    ; Unimplemented.
    (t (progn (format t "Word writes to 0x~8,'0X are unimplemented!~%" address) (loop) value))))

(declaim (ftype (function (psx) function) map-memory))
(defun map-memory (psx)
  (setf
   (psx-cpu:cpu-memory-get-byte (psx-cpu psx))
   (lambda (address) (load-byte* psx address)))
  (setf
   (psx-cpu:cpu-memory-get-half-word (psx-cpu psx))
   (lambda (address) (load-half-word* psx address)))
  (setf
   (psx-cpu:cpu-memory-get-word (psx-cpu psx))
   (lambda (address) (load-word* psx address)))
  (setf
   (psx-cpu:cpu-memory-set-byte (psx-cpu psx))
   (lambda (address value) (write-byte* psx address value)))
  (setf
   (psx-cpu:cpu-memory-set-half-word (psx-cpu psx))
   (lambda (address value) (write-half-word* psx address value)))
  (setf
   (psx-cpu:cpu-memory-set-word (psx-cpu psx))
   (lambda (address value) (write-word* psx address value))))
