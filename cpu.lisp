(defpackage #:psx-cpu
  (:nicknames #:cpu)
  (:use :cl)
  (:export #:cpu #:make-cpu
           #:cpu-memory-get #:cpu-memory-set
           #:power-on #:step-cpu))

(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))
(defconstant bios-begin-address-kseg1 #xBFC00000)

(defvar instructions (make-hash-table :test 'equal))

(defstruct instruction
  "PSX instruction"
  (word 0 :type (unsigned-byte 32))
  (address 0 :type (unsigned-byte 32))
  (segment :invalid :type keyword)
  (operation
   (lambda (cpu instruction)
           (declare (ignore cpu instruction))
           (values))
   :type (function (cpu instruction) (values &optional)))
  (mnemonic "" :type string)
  (operation-code 0 :type (unsigned-byte 6))
  ; TODO(Samantha): Consider changing the following two values to denote that
  ; they are indexes
  (source-register 0 :type (unsigned-byte 5))
  (target-register 0 :type (unsigned-byte 5))
  (immediate-value 0 :type (unsigned-byte 16))
  (jump-target 0 :type (unsigned-byte 26))
  (destination-register 0 :type (unsigned-byte 5))
  (shift-amount 0 :type (unsigned-byte 5))
  (secondary-operation-code 0 :type (unsigned-byte 6)))

(defstruct cpu
  "A model PSX cpu"
  (program-counter 0 :type (unsigned-byte 32))
  (registers
   (make-array 32 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (32)))
  (hi 0 :type (unsigned-byte 32))
  (lo 0 :type (unsigned-byte 32))
  (next-instruction (make-instruction) :type instruction)
  (memory-get
   (lambda (address) (declare (ignore address)) 0)
   :type (function ((unsigned-byte 32)) (unsigned-byte 8)))
  (memory-set
   (lambda (address value) (declare (ignore address value)) 0)
   :type (function ((unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8))))

(declaim (ftype (function (cpu) (unsigned-byte 32)) power-on))
(defun power-on (cpu)
  "Sets the cpu to the initial power up state."
  ; TODO(Samantha): Fully implement.
  (setf (cpu-program-counter cpu) bios-begin-address-kseg1)
  (setf (cpu-next-instruction cpu) (decode cpu (fetch cpu)))
  (setf (cpu-program-counter cpu)
        (wrap-word (+ 4 (cpu-program-counter cpu)))))


; TODO(Samantha): Move the following constants and two functions out to a
; separate file so that we don't have multiple copies in the codebase.
(defconstant mirror-size #x20000000)
(defconstant kuseg-base #x00000000)
(defconstant kseg0-base #x80000000)
(defconstant kseg1-base #xA0000000)
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

(declaim (ftype (function (instruction) string) instruction-information))
(defun instruction-information (instruction)
  (format nil "~A (0x~8,'0X) at 0x~8,'0X (segment: ~A)"
          (instruction-mnemonic instruction)
          (instruction-word instruction)
          (instruction-address instruction)
          (instruction-segment instruction)))

(declaim (ftype (function ((unsigned-byte 16)) (unsigned-byte 32))
                sign-extend))
(defun sign-extend (to-be-extended)
  (logior to-be-extended
          (if (ldb-test (byte 1 15) to-be-extended)
            ; Left fill 1s
            #xFFFF0000
            ; Left fill 0s
            #x00000000)))

(declaim (ftype (function ((signed-byte 64)) (unsigned-byte 32))
                wrap-word))
(defun wrap-word (to-be-wrapped)
  "Takes up to a 64 bit unsigned int and returns the truncated 32 bit
   representation"
  (ldb (byte 32 0) to-be-wrapped))

(declaim (ftype (function ((unsigned-byte 16)) (signed-byte 32))
                to-signed-byte-32))
(defun to-signed-byte-32 (to-be-extended)
  ; If the MSB is set, do the inversions.
  (if (ldb-test (byte 1 15) to-be-extended)
    (* (the (signed-byte 32) -1) (wrap-word (1+ (lognot (sign-extend to-be-extended)))))
    to-be-extended))

; TODO(Samantha): These six functions should really be wrapped into the mmu.
(declaim (ftype (function (cpu (unsigned-byte 32)) (unsigned-byte 8))
                read-cpu))
(defun read-cpu (cpu address)
  (funcall (cpu-memory-get cpu) address))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8))
                write-cpu))
(defun write-cpu (cpu address value)
  (funcall (cpu-memory-set cpu) address value))

(declaim (ftype (function (cpu (unsigned-byte 32)) (unsigned-byte 16))
                read-cpu-half-word))
(defun read-cpu-half-word (cpu address)
  (let ((lo (read-cpu cpu address))
        (hi (read-cpu cpu (wrap-word (1+ address)))))
    (logior (ash hi 8) lo)))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 16)) (unsigned-byte 16))
                write-cpu-half-word))
(defun write-cpu-half-word (cpu address value)
  (let ((lo (ldb (byte 8 0) value))
        (hi (ldb (byte 8 8) value)))
    (write-cpu cpu address lo)
    (write-cpu cpu (wrap-word (1+ address)) hi)
    value))

(declaim (ftype (function (cpu (unsigned-byte 32)) (unsigned-byte 32))
                read-cpu-word))
(defun read-cpu-word (cpu address)
  (let ((lo (read-cpu-half-word cpu address))
        (hi (read-cpu-half-word cpu (wrap-word (+ 2 address)))))
    (logior (ash hi 16) lo)))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))
                write-cpu-word))
(defun write-cpu-word (cpu address value)
  (let ((lo (ldb (byte 16 0) value))
        (hi (ldb (byte 16 16) value)))
    (write-cpu-half-word cpu address lo)
    (write-cpu-half-word cpu (wrap-word (+ 2 address)) hi)))

(declaim (ftype (function (cpu) (unsigned-byte 32)) fetch))
(defun fetch (cpu)
  "Retrieves an instruction for execution as a 32 bit word."
  ; TODO(Samantha): Implement.
  (read-cpu-word cpu (cpu-program-counter cpu)))

(declaim (ftype (function (cpu (unsigned-byte 32)) instruction) decode))
(defun decode (cpu instruction-as-word)
  "Transforms a 32 bit word into an executable instruction."
  ; TODO(Samantha): Implement.
  (let ((masked-opcode
         (if (ldb-test (byte 6 26) instruction-as-word)
           (ldb (byte 6 26) instruction-as-word)
           (logior #xFF00 (ldb (byte 6 0) instruction-as-word)))))
    (make-instruction
     :word instruction-as-word
     :segment (determine-segment (cpu-program-counter cpu))
     ; If this instruction isn't in our list, make a dummy.
     :operation (or (cadr (gethash masked-opcode instructions))
                    (lambda (cpu instruction)
                            (declare (ignore cpu instruction))
                            (values)))
     :address (cpu-program-counter cpu)
     ; If this instruction isn't in our list, it's illegal!
     :mnemonic (or (car (gethash masked-opcode instructions)) "Illegal Instruction!")
     :operation-code (ldb (byte 6 26) instruction-as-word)
     :source-register (ldb (byte 5 21) instruction-as-word)
     :target-register (ldb (byte 5 16) instruction-as-word)
     :immediate-value (ldb (byte 16 0) instruction-as-word)
     :jump-target (ldb (byte 26 0) instruction-as-word)
     :destination-register (ldb (byte 5 11) instruction-as-word)
     :shift-amount (ldb (byte 5 6) instruction-as-word)
     :secondary-operation-code (ldb (byte 6 0) instruction-as-word))))

(declaim (ftype (function (cpu instruction) (unsigned-byte 8)) execute))
(defun execute (cpu instruction)
  "Executes a single instruction and returns the number of cycles that this
   took."
  ; TODO(Samantha): Implement.
  (funcall (instruction-operation instruction) cpu instruction)
  (format t "~A~%" (instruction-information instruction))
  (format t "Masked opcode is 0x~8,'0X~%"
          (if (ldb-test (byte 6 26) (instruction-word instruction))
            (ldb (byte 6 26) (instruction-word instruction))
            (logior #xFF00 (ldb (byte 6 0) (instruction-word instruction)))))
  0)

(declaim (ftype (function (cpu) (unsigned-byte 8)) step-cpu))
(defun step-cpu (cpu)
  "Steps the cpu through a single fetch decode execute cycle, returning the
   number of cycles it took."
  ; Always execute the next instruction to appease the branch delay slot
  ; overlords.
  ; TODO(Samantha): The next instruction after a branch should have the same
  ; address as the branch instruction itself. Exceptions add another
  ; complication to this process.
  (let ((instruction (cpu-next-instruction cpu)))
    (setf (cpu-next-instruction cpu) (decode cpu (fetch cpu)))
    (setf (cpu-program-counter cpu)
          (wrap-word (+ 4 (cpu-program-counter cpu))))
    (execute cpu instruction)))
