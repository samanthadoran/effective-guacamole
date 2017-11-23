(in-package :psx-console)

(declaim (optimize (speed 3) (safety 1)))

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

(declaim (ftype (function ((unsigned-byte 32)
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          boolean) in-range))
(defun in-range (base size place)
  (and (>= place base) (< place (+ base size))))

; TODO(Samantha): I think these mirror sizes are wrong, also we are blatantly
; ignoring KSEG2.
(declaim (ftype (function ((unsigned-byte 32))
                          keyword)
                determine-segment))
(defun determine-segment (address)
  (cond
    ((in-range kuseg-base mirror-size address) :kuseg)
    ((in-range kseg0-base mirror-size address) :kseg0)
    ((in-range kseg1-base mirror-size address) :kseg1)
    (t :invalid-segment)))

(declaim (ftype (function ((simple-array (unsigned-byte 8))
                           (unsigned-byte 32)
                           (unsigned-byte 32))
                          (unsigned-byte 32))
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

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 8))
                load-byte*))
(defun load-byte* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (let ((address (mask-address address)))
    (cond
      ((in-range bios-begin-address
                 (array-dimension (psx-bios-rom psx) 0)
                 address)
       ; We only care about the top byte.
       (aref (psx-bios-rom psx) (- address bios-begin-address)))
      ((in-range ram-begin ram-size address)
       ; We only care about the top byte.
       (aref (psx-ram psx) (mod address #x200000)))
      ((in-range expansion-1-begin
                 expansion-1-size
                 address)
       ; We don't care about the expansion port just yet, return a dummy value.
       #xFF)
      ; Unimplemented.
      (t (error "Byte reads to 0x~8,'0X are unimplemented~%" address)))))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 16))
                load-half-word*))
(defun load-half-word* (psx address)
  (declare (ignore psx))
  ; TODO(Samantha): Implement more places, simplify the cond.
  (cond
    ; Unimplemented.
    (t (error "Half-word reads to 0x~8,'0X are unimplemented~%" address))))

(declaim (ftype (function (psx (unsigned-byte 32)) (unsigned-byte 32))
                load-word*))
(defun load-word* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (let ((address (mask-address address)))
    (cond
      ; BIOS
      ((in-range bios-begin-address
                 (array-dimension (psx-bios-rom psx) 0)
                 address)
       (read-word-from-byte-array
        (psx-bios-rom psx) (- address bios-begin-address)))
      ; RAM
      ((in-range ram-begin ram-size address)
       (read-word-from-byte-array (psx-ram psx) (mod address #x200000)))
      ; Unimplemented.
      (t (error "Word reads to 0x~8,'0X are unimplemented~%" address)))))

; TODO(Samantha): Figure out a way to fix this shadowing.
(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8))
        write-byte*))
(defun write-byte* (psx address value)
  (let ((address (mask-address address)))
    (cond
      ((in-range expansion-2-begin expansion-2-size address)
       (format t "Wrote 0x~8,'0x to expansion2 @ 0x~8,'0x!~%" value address)
       value)
      ((in-range ram-begin ram-size address)
       (setf
        (aref (psx-ram psx) (mod address #x200000))
        value))
      ; Unimplemented.
      (t (error "Byte writes to 0x~8,'0X are unimplemented!~%" address)))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 16)) (unsigned-byte 16))
        write-half-word*))
(defun write-half-word* (psx address value)
  (declare (ignore psx))
  (let ((address (mask-address address)))
    (cond
      ((in-range spu-registers-begin spu-registers-size address)
       (format t "Wrote 0x~8,'0x to spu @ 0x~8,'0x!~%" value address)
       value)
      ; Unimplemented.
      (t (error "Half-word writes to 0x~8,'0X are unimplemented!~%" address)))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
        write-word*))
(defun write-word* (psx address value)
  (let ((address (mask-address address)))
    (cond
      ((in-range memory-control-begin memory-control-size address)
       (cond
         ; Expansion base 1 register
         ((= address memory-control-begin)
          (when (/= value expansion-1-begin)
            (error "Unexpected value for expansion-1-begin, got 0x~8,'0x, expected 0x~8,'0x!~%" value expansion-1-begin))
          value)
         ((= address (+ memory-control-begin 4))
          (when (/= value expansion-2-begin)
            (error "Unexpected value for expansion-2-begin, got 0x~8,'0x, expected 0x~8,'0x!~%" value expansion-2-begin))
          value)
         ((= address (+ memory-control-begin 8))
          (format t "Wrote 0x~8,'0x to expansion 1 delay/size!~%" value)
          value)
         ((= address (+ memory-control-begin #xC))
          (format t "Wrote 0x~8,'0x to expansion 3 delay/size!~%" value)
          value)
         ((= address (+ memory-control-begin #x10))
          (format t "Wrote 0x~8,'0x to bios delay/size!~%" value)
          value)
         ((= address (+ memory-control-begin #x14))
          (format t "Wrote 0x~8,'0x to spu_delay!~%" value)
          value)
         ((= address (+ memory-control-begin #x18))
          (format t "Wrote 0x~8,'0x to cdrom_delay!~%" value)
          value)
         ((= address (+ memory-control-begin #x1C))
          (format t "Wrote 0x~8,'0x to expansion 2 delay/size!~%" value)
          value)
         ((= address (+ memory-control-begin #x20))
          (format t "Wrote 0x~8,'0x to common delay!~%" value)
          value)
         (t (error "Unexpected write of 0x~8,'0x! to Memory Control at 0x~8,'0x!~%" value address))))
      ((in-range irq-registers-begin irq-registers-size address)
       (format t "Wrote 0x~8,'0x to irq registers at 0x~8,'0x~%" value address)
       value)
      ((= address ram-size-begin)
       (format t "Wrote 0x~8,'0x to ram size!~%" value)
       value)
      ((= address cache-control)
       (format t "Wrote 0x~8,'0x to cache control!~%" value)
       value)
      ; RAM
      ((in-range ram-begin ram-size address)
       ; (format t "Wrote 0x~8,'0x to ram(0x~8,'0X)!~%" value address)
       (write-word-to-byte-array (psx-ram psx) (mod address #x200000) value))
      ; Unimplemented.
      (t (error "Word writes to 0x~8,'0X are unimplemented!~%" address)))))

(declaim (ftype (function (psx) function) map-memory))
(defun map-memory (psx)
  "Sets functions for easy reading and writing throughout the system."
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
