(defpackage #:psx-cpu
  (:nicknames #:cpu)
  ; TODO(Samantha): Should I be using :psx-coprocessor0 as well?
  (:use :cl :memory)
  (:export #:cpu #:make-cpu
           #:cpu-memory-get-byte #:cpu-memory-set-byte
           #:cpu-memory-get-half-word #:cpu-memory-set-half-word
           #:cpu-memory-get-word #:cpu-memory-set-word
           #:power-on #:step-cpu))

(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defvar instructions (make-hash-table :test 'equal))

(defparameter *debug-cpu* nil)

(defstruct instruction
  "PSX instruction"
  (word 0 :type (unsigned-byte 32))
  (address 0 :type (unsigned-byte 32))
  ; A short representation of an instruction without any specific information.
  ; Used for indexing into the hashtable instead of having a giant case
  ; statement.
  (masked-opcode 0 :type (unsigned-byte 20))
  (segment :invalid :type keyword)
  (operation
   (lambda (cpu instruction)
           (declare (ignore cpu instruction))
           (values))
   :type (function (cpu instruction) (values &optional)))
  (mnemonic "" :type string)
  ; Bits [31:26]
  (operation-code 0 :type (unsigned-byte 6))
  ; TODO(Samantha): Consider changing the following two values to denote that
  ; they are indexes
  ; Bits [25:21]
  (source-register 0 :type (unsigned-byte 5))
  ; Bits [20:16]
  (target-register 0 :type (unsigned-byte 5))
  ; Bits [15:0]
  (immediate-value 0 :type (unsigned-byte 16))
  ; Bits [25:0]
  (jump-target 0 :type (unsigned-byte 26))
  ; Bits [15:11]
  (destination-register 0 :type (unsigned-byte 5))
  ; Bits [10:6]
  (shift-amount 0 :type (unsigned-byte 5))
  ; Bits [5:0]
  (secondary-operation-code 0 :type (unsigned-byte 6)))

(defstruct cpu
  "A model PSX cpu"
  (program-counter 0 :type (unsigned-byte 32))
  ; Used for exceptions exclusively.
  (current-program-counter 0 :type (unsigned-byte 32))
  (next-program-counter 0 :type (unsigned-byte 32))
  (registers
   (make-array 32 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (32)))
  (pending-load-register 0 :type (unsigned-byte 5))
  (pending-load-value 0 :type (unsigned-byte 32))
  (delay-registers
   (make-array 32 :element-type '(unsigned-byte 32))
   :type (simple-array (unsigned-byte 32) (32)))
  (hi 0 :type (unsigned-byte 32))
  (lo 0 :type (unsigned-byte 32))
  (cop0 (cop0:make-cop0) :type cop0:cop0)
  ; TODO(Samantha): These are awful and shouldn't really be necessary.
  (memory-get-byte
   (lambda (address) (declare (ignore address)) 0)
   :type (function ((unsigned-byte 32)) (unsigned-byte 8)))
  (memory-get-half-word
   (lambda (address) (declare (ignore address)) 0)
   :type (function ((unsigned-byte 32)) (unsigned-byte 16)))
  (memory-get-word
   (lambda (address) (declare (ignore address)) 0)
   :type (function ((unsigned-byte 32)) (unsigned-byte 32)))
  (memory-set-byte
   (lambda (address value) (declare (ignore address value)) 0)
   :type (function ((unsigned-byte 32) (unsigned-byte 8)) (unsigned-byte 8)))
  (memory-set-half-word
   (lambda (address value) (declare (ignore address value)) 0)
   :type (function ((unsigned-byte 32) (unsigned-byte 16)) (unsigned-byte 16)))
  (memory-set-word
   (lambda (address value) (declare (ignore address value)) 0)
   :type (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32))))

(declaim (ftype (function (cpu) (unsigned-byte 32)) power-on))
(defun power-on (cpu)
  "Sets the cpu to the initial power up state."
  ; TODO(Samantha): Fully implement.
  (setf (cpu-program-counter cpu) +bios-begin-unmasked-address+)
  (setf
   (cpu-next-program-counter cpu)
   (wrap-word (+ (cpu-program-counter cpu) 4))))

(declaim (ftype (function (instruction) string) instruction-information))
(defun instruction-information (instruction)
  "Returns a pretty formatted representation of any given instruction."
  ; TODO(Samantha): Add a line with disassembly.
  (format nil "~A (0x~8,'0X) at 0x~8,'0X (segment: ~A)~%Masked opcode is 0x~5,'0X~%"
          (instruction-mnemonic instruction)
          (instruction-word instruction)
          (instruction-address instruction)
          (instruction-segment instruction)
          (instruction-masked-opcode instruction)))

(declaim
 (ftype (function
         (cpu (unsigned-byte 5) (unsigned-byte 32)) (unsigned-byte 32))
        set-register))
(defun set-register (cpu index value)
  (setf (aref (cpu-delay-registers cpu) index) value)
  (setf (aref (cpu-delay-registers cpu) 0) 0))

; TODO(Samantha): These six functions should really be wrapped into the mmu.
(declaim (ftype (function (cpu (unsigned-byte 32))
                          (unsigned-byte 8))
                read-cpu-byte))
(defun read-cpu-byte (cpu address)
  (funcall (cpu-memory-get-byte cpu) address))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 8))
                          (unsigned-byte 8))
                write-cpu-byte))
(defun write-cpu-byte (cpu address value)
  (funcall (cpu-memory-set-byte cpu) address value))

(declaim (ftype (function (cpu (unsigned-byte 32))
                          (unsigned-byte 16))
                read-cpu-half-word))
(defun read-cpu-half-word (cpu address)
  (funcall (cpu-memory-get-half-word cpu) address))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 16))
                          (unsigned-byte 16))
                write-cpu-half-word))
(defun write-cpu-half-word (cpu address value)
  (funcall (cpu-memory-set-half-word cpu) address value))

(declaim (ftype (function (cpu (unsigned-byte 32))
                          (unsigned-byte 32))
                read-cpu-word))
(defun read-cpu-word (cpu address)
  (funcall (cpu-memory-get-word cpu) address))

(declaim (ftype (function (cpu (unsigned-byte 32) (unsigned-byte 32))
                          (unsigned-byte 32))
                write-cpu-word))
(defun write-cpu-word (cpu address value)
  (funcall (cpu-memory-set-word cpu) address value))

(declaim (ftype (function (cpu) (unsigned-byte 32)) fetch))
(defun fetch (cpu)
  "Retrieves an instruction for execution as a 32 bit word."
  (read-cpu-word cpu (cpu-program-counter cpu)))

(declaim (ftype (function ((unsigned-byte 32))
                          (unsigned-byte 32))
                get-masked-opcode))
(defun get-masked-opcode (instruction-as-word)
  "Creates a shorter mnemonic for opcodes that's stripped of non-general
   information."
  (if (= (ldb (byte 2 30) instruction-as-word) 1)
    ; Coprocessor op. Mnemonic of #xC0NXX, where C0 means coprocessor, n is the
    ; coprocessor number, and xx is the cop opcode.
    ; TODO(Samantha): RFE is another pain point like b-cond-z. It can have
    ; multiple instructions encoded if we only use our pattern. Thankfully, the
    ; other instructions it can encode are for virtual memory, which the psx
    ; does not have. So, we should be safe to leave this out...?
    (logior
     #x000C0000
     (ash (ldb (byte 2 26) instruction-as-word) 8)
     (ldb (byte 5 21) instruction-as-word))
    (if (ldb-test (byte 6 26) instruction-as-word)
      ; If any bits in [26, 31] of instruction-as-word are set, we can just use
      ; those as our masked opcode, otherwise we have a special opcode and mark
      ; it using a leading #xFF and then use bits [0, 6] of instruction-as-word.
      (ldb (byte 6 26) instruction-as-word)
      (logior #xFF00 (ldb (byte 6 0) instruction-as-word)))))

(declaim (ftype (function (cpu (unsigned-byte 32)) instruction) decode))
(defun decode (cpu instruction-as-word)
  "Transforms a 32 bit word into an executable instruction."
  (let ((masked-opcode (get-masked-opcode instruction-as-word)))
    (make-instruction
     :word instruction-as-word
     :segment (determine-segment (cpu-program-counter cpu))
     ; If this instruction isn't in our list, make a dummy.
     :operation (or
                 (cadr (gethash masked-opcode instructions))
                 (lambda (cpu instruction)
                         (declare (ignore instruction))
                         (trigger-exception cpu :cause :reserved-instruction)
                         (values)))
     :address (cpu-program-counter cpu)
     :masked-opcode masked-opcode
     ; If this instruction isn't in our list, it's illegal!
     ; TODO(Samantha): Consider special casing b-cond-z so we get more useful
     ; information.
     :mnemonic (or
                (car (gethash masked-opcode instructions))
                "Illegal Instruction!")
     :operation-code (ldb (byte 6 26) instruction-as-word)
     :source-register (ldb (byte 5 21) instruction-as-word)
     :target-register (ldb (byte 5 16) instruction-as-word)
     :immediate-value (ldb (byte 16 0) instruction-as-word)
     :jump-target (ldb (byte 26 0) instruction-as-word)
     :destination-register (ldb (byte 5 11) instruction-as-word)
     :shift-amount (ldb (byte 5 6) instruction-as-word)
     :secondary-operation-code (ldb (byte 6 0) instruction-as-word))))

(declaim (ftype (function (cpu &key (:cause keyword))
                          (unsigned-byte 32))
                trigger-exception))
(defun trigger-exception (cpu &key cause)
  ; Exception handler address is determined by the 22nd (BEV) bit of
  ; cop0_12 (status register)
  (setf (cpu-program-counter cpu)
        (if (ldb-test (byte 1 22) (cop0:cop0-status-register (cpu-cop0 cpu)))
          +rom-exception-vector+
          +ram-exception-vector+))
  (setf
   (cpu-next-program-counter cpu)
   (wrap-word (+ (cpu-program-counter cpu) 4)))
  (setf
   (cop0:cop0-epc-register (cpu-cop0 cpu))
   (cpu-current-program-counter cpu))
  (setf (cop0:cop0-cause-register (cpu-cop0 cpu))
        (ash
         (case cause
           (:address-load-error #x4)
           (:address-write-error #x5)
           (:syscall #x8)
           (:breakpoint #x9)
           (:reserved-instruction #xA)
           (:coprocessor-unusable #xB)
           (:arithmetic-overflow #xC)
           (otherwise (error "Unimplemented cause ~A" cause)))
         2))
  ; Only these two causes ever change the bad virtual address register.
  (when (or (eq cause :address-write-error) (eq cause :address-load-error))
    (setf
     (cop0:cop0-bad-virtual-address-register (cpu-cop0 cpu))
     (cpu-current-program-counter cpu)))
  ; TODO(Samantha): Understand this mess better.
  (setf
   (ldb (byte 6 0) (cop0:cop0-status-register (cpu-cop0 cpu)))
   (ldb (byte 6 0) (ash (cop0:cop0-status-register (cpu-cop0 cpu)) 2))))

(declaim (ftype (function (cpu instruction) (unsigned-byte 8)) execute))
(defun execute (cpu instruction)
  "Executes a single instruction and returns the number of cycles that this
   took."
  (set-register cpu (cpu-pending-load-register cpu) (cpu-pending-load-value cpu))
  (setf (cpu-pending-load-register cpu) 0)
  (setf (cpu-pending-load-value cpu) 0)
  (when *debug-cpu*
    (format t "~A~%" (instruction-information instruction)))
  (if (/= 0 (mod (cpu-current-program-counter cpu) 4))
    (trigger-exception cpu :cause :address-load-error)
    (funcall (instruction-operation instruction) cpu instruction))
  ; Move the registers from the delay slots into the regular ones.
  (setf (cpu-registers cpu) (copy-seq (cpu-delay-registers cpu)))
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
  (let ((instruction (decode cpu (fetch cpu))))
    (setf (cpu-current-program-counter cpu) (cpu-program-counter cpu))
    (setf (cpu-program-counter cpu) (cpu-next-program-counter cpu))
    (setf (cpu-next-program-counter cpu)
          (wrap-word (+ 4 (cpu-next-program-counter cpu))))
    (execute cpu instruction)))
