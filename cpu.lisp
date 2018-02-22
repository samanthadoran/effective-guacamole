(defpackage #:psx-cpu
  (:nicknames #:cpu)
  ; TODO(Samantha): Should I be using :psx-coprocessor0 as well?
  (:use :cl :memory)
  (:export #:cpu #:make-cpu
           #:cpu-memory-get-byte #:cpu-memory-set-byte
           #:cpu-memory-get-half-word #:cpu-memory-set-half-word
           #:cpu-memory-get-word #:cpu-memory-set-word
           #:power-on #:step-cpu #:trigger-exception))

(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 0)))

(defvar instructions (make-hash-table :test 'equal))

(declaim (boolean *debug-cpu*))
(defparameter *debug-cpu* nil)

(defstruct instruction
  "PSX instruction"
  (word 0 :type (unsigned-byte 32))
  (address 0 :type (unsigned-byte 32))
  ; A short representation of an instruction without any specific information.
  ; Used for indexing into the hashtable of instructions instead of using a
  ; giant case statement.
  (masked-opcode 0 :type (unsigned-byte 20))
  ; The memory segment in which this instruction was fetched from.
  (segment :invalid :type keyword)
  (operation
   (lambda (cpu instruction)
           (declare (ignore cpu instruction))
           (values))
   :type (function (cpu instruction) (values &optional)))
  ; The r3000a abbreviation for this instruction.
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

(defstruct cache-line
  (tag 0 :type (unsigned-byte 19))
  (instructions-as-words
   (make-array 4 :element-type '(unsigned-byte 32)
               :initial-element #xDEADBEEF)
   :type (simple-array (unsigned-byte 32) (4)))
  (valid
   (make-array 4 :element-type 'boolean
               :initial-element nil)
   :type (simple-array boolean (4))))

(defstruct cpu
  "A model PSX cpu"
  (instruction (make-instruction) :type instruction)
  (cache-lines
   (make-array 256 :element-type 'cache-line
               :initial-contents (loop for i from 0 to 255
                                   collect (make-cache-line)))
   :type (simple-array cache-line (256)))
  (program-counter 0 :type (unsigned-byte 32))
  ; Used for exceptions exclusively.
  (current-program-counter 0 :type (unsigned-byte 32))
  (next-program-counter 0 :type (unsigned-byte 32))
  (registers
   (make-array 32 :element-type '(unsigned-byte 32)
               :initial-element 0)
   :type (simple-array (unsigned-byte 32) (32)))
  (pending-load-register 0 :type (unsigned-byte 5))
  (pending-load-value 0 :type (unsigned-byte 32))
  (delay-registers
   (make-array 32 :element-type '(unsigned-byte 32)
               :initial-element 0)
   :type (simple-array (unsigned-byte 32) (32)))
  (hi 0 :type (unsigned-byte 32))
  (lo 0 :type (unsigned-byte 32))
  (cop0 (cop0:make-cop0) :type cop0:cop0)
  (branch-opcode nil :type boolean)
  (in-branch-delay nil :type boolean)
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

(declaim (ftype (function (cpu (unsigned-byte 5) (unsigned-byte 32))
                          (unsigned-byte 32))
        set-register))
(defun set-register (cpu index value)
  "Sets a register in a load-delay conscious fashion."
  (setf (aref (cpu-delay-registers cpu) index) value)
  (setf (aref (cpu-delay-registers cpu) 0) 0))

(declaim (ftype (function (cpu) (unsigned-byte 32)) fetch-from-cache))
(defun fetch-from-cache (cpu)
  "Fetch an instruction from cache and perform maintenance as necessary."
  (let* ((instruction-address (cpu-program-counter cpu))
        (tag (ldb (byte 19 11) instruction-address))
        (cache-line (aref (cpu-cache-lines cpu)
                          (ldb (byte 8 4) instruction-address)))
        (index (ldb (byte 2 2) instruction-address)))
    (when (or (/= tag (cache-line-tag cache-line))
              (not (aref (cache-line-valid cache-line) index)))
      ; A cache miss is expensive! tick here!
      (loop for i from index to 3
        ; TODO(Samantha): Support something like this because reading from
        ; memory takes time.
        ; do (tick 1)
        do (setf (aref (cache-line-valid cache-line) i) t)
        do (setf (aref (cache-line-instructions-as-words cache-line) i)
                 (funcall (cpu-memory-get-word cpu)
                          (+
                           (cpu-program-counter cpu)
                           (* 4 (ldb (byte 8 4) instruction-address)))))))
    (aref (cache-line-instructions-as-words cache-line) index)))

(declaim (ftype (function (cpu) (unsigned-byte 32)) fetch))
(defun fetch (cpu)
  "Retrieves an instruction for execution as a 32 bit word."
  ; TODO(Samantha): Actually read from cache.
  (if (and nil (is-cacheable (cpu-program-counter cpu)))
    (fetch-from-cache cpu)
    (funcall (cpu-memory-get-word cpu) (cpu-program-counter cpu))))

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
     ; Cop2 opcodes have a different encoding because of course they do.
     (if (= (ldb (byte 2 26) instruction-as-word) 2)
       (ldb (byte 6 0) instruction-as-word)
       (ldb (byte 5 21) instruction-as-word)))
    (if (ldb-test (byte 6 26) instruction-as-word)
      ; If any bits in [26, 31] of instruction-as-word are set, we can just use
      ; those as our masked opcode, otherwise we have a special opcode and mark
      ; it using a leading #xFF and then use bits [0, 6] of instruction-as-word.
      (ldb (byte 6 26) instruction-as-word)
      (logior #xFF00 (ldb (byte 6 0) instruction-as-word)))))

(declaim (list *illegal-instruction*))
(defparameter *illegal-instruction*
  (list "Illegal Instruction!"
        (lambda (cpu instruction)
                (error "Illegal instruction! Word: 0x~8,'0x, ~
                                       masked: 0x~6,'0x~%"
                       (instruction-word instruction)
                       (instruction-masked-opcode instruction))
                (trigger-exception cpu :cause :reserved-instruction)
                (values))))

(declaim (ftype (function (cpu (unsigned-byte 32)) instruction) decode))
(defun decode (cpu instruction-as-word)
  "Transforms a 32 bit word into an executable instruction."
  ; It's much faster to edit the existing instruction than to make a new one
  ; each time, even if it looks messier.
  (let* ((instruction (cpu-instruction cpu))
        (masked-opcode (get-masked-opcode instruction-as-word))
        (hashed-instruction
         (gethash
          masked-opcode instructions
          *illegal-instruction*)))
    (setf (instruction-word instruction)
          instruction-as-word)
    (setf (instruction-segment instruction)
          (determine-segment (cpu-program-counter cpu)))
    ; If this instruction isn't in our list, make a dummy.
    (setf (instruction-operation instruction)
          (cadr hashed-instruction))
    (setf (instruction-address instruction)
          (cpu-program-counter cpu))
    (setf (instruction-masked-opcode instruction)
          masked-opcode)
    ; If this instruction isn't in our list, it's illegal!
    ; TODO(Samantha): Consider special casing b-cond-z so we get more useful
    ; information.
    (setf (instruction-mnemonic instruction)
          (car hashed-instruction))
    (setf (instruction-operation-code instruction)
          (ldb (byte 6 26) instruction-as-word))
    (setf (instruction-source-register instruction)
          (ldb (byte 5 21) instruction-as-word))
    (setf (instruction-target-register instruction)
          (ldb (byte 5 16) instruction-as-word))
    (setf (instruction-immediate-value instruction)
          (ldb (byte 16 0) instruction-as-word))
    (setf (instruction-jump-target instruction)
          (ldb (byte 26 0) instruction-as-word))
    (setf (instruction-destination-register instruction)
          (ldb (byte 5 11) instruction-as-word))
    (setf (instruction-shift-amount instruction)
          (ldb (byte 5 6) instruction-as-word))
    (setf (instruction-secondary-operation-code instruction)
          (ldb (byte 6 0) instruction-as-word))
    instruction))

; TODO(Samantha): This should be triggered by irqs through step-cpu by checking cop0.
(declaim (ftype (function (cpu &key (:cause keyword))
                          (unsigned-byte 32))
                trigger-exception))
(defun trigger-exception (cpu &key cause)
  ; This is basically a pipeline hazard and should stall the cpu, right?
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
        (logior
         (logand #xFFFFFF83 (cop0:cop0-cause-register (cpu-cop0 cpu)))
         (ash
          (case cause
            (:interrupt #x0)
            (:address-load-error #x4)
            (:address-write-error #x5)
            (:syscall #x8)
            (:breakpoint #x9)
            (:reserved-instruction #xA)
            (:coprocessor-unusable #xB)
            (:arithmetic-overflow #xC)
            (otherwise (error "Unimplemented cause ~A" cause)))
          2)
         (ash
          (if (ldb-test (byte 16 0)
                        (logand (funcall (cpu-memory-get-word cpu)
                                         +irq-registers-begin+)
                                (funcall (cpu-memory-get-word cpu)
                                         (+ +irq-registers-begin+ 4))))
            1
            0)
          10)))
  ; Exceptions get a little weird if they are in branch delay slots. Move
  ; back by four to compensate and set the exception in branch delay flag of
  ; the cause register.
  (when (cpu-in-branch-delay cpu)
    (setf (ldb (byte 31 0) (cop0:cop0-cause-register (cpu-cop0 cpu))) 1)
    (setf (cop0:cop0-epc-register (cpu-cop0 cpu))
          (wrap-word (- (cop0:cop0-epc-register (cpu-cop0 cpu)) 4))))
  ; Only these two causes ever change the bad virtual address register.
  (when (or (eq cause :address-write-error) (eq cause :address-load-error))
    (setf
     (cop0:cop0-bad-virtual-address-register (cpu-cop0 cpu))
     (cpu-current-program-counter cpu)))
  ; TODO(Samantha): Understand this mess better.
  (setf
   (ldb (byte 6 0) (cop0:cop0-status-register (cpu-cop0 cpu)))
   (ldb (byte 6 0) (ash (cop0:cop0-status-register (cpu-cop0 cpu)) 2)))
  (cop0:cop0-status-register (cpu-cop0 cpu)))

(declaim (ftype (function (cpu instruction) (unsigned-byte 8)) execute))
(defun execute (cpu instruction)
  "Executes a single instruction and returns the number of cycles that this
   took."
  (set-register cpu (cpu-pending-load-register cpu)
                (cpu-pending-load-value cpu))
  (setf (cpu-pending-load-register cpu) 0)
  (setf (cpu-pending-load-value cpu) 0)
  (if (zerop (mod (cpu-current-program-counter cpu) 4))
    (funcall (instruction-operation instruction) cpu instruction)
    (trigger-exception cpu :cause :address-load-error))
  ; Move the registers from the delay slots into the regular ones.
  (setf (cpu-registers cpu) (copy-seq (cpu-delay-registers cpu)))
  ; TODO(Samantha): We need to finally start working with timing. Many (all?)
  ; of the opcodes take a fixed amount of time with the only variable being
  ; whether or not they are in cache or you hit a pipeline hazard. So, this
  ; means that the fetch decode execute cycle (and it's sub components) each
  ; have a cycle cost. Investigate and implement.
  1)

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
    (setf (cpu-in-branch-delay cpu) nil)
    (when (cpu-branch-opcode cpu)
      (setf (cpu-branch-opcode cpu) nil)
      (setf (cpu-in-branch-delay cpu) t))
    (setf (cpu-current-program-counter cpu) (cpu-program-counter cpu))
    (setf (cpu-program-counter cpu) (cpu-next-program-counter cpu))
    (setf (cpu-next-program-counter cpu)
          (wrap-word (+ 4 (cpu-next-program-counter cpu))))
    (execute cpu instruction)))
