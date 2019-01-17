(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type b-cond-z #x01
  ; If bit 16 is set, we have BGEZ, else BLTZ. Test either way.
  (when (if (ldb-test (byte 1 16) (instruction-word instruction))
          (>= (to-signed-byte-32 source-register-value) 0)
          (< (to-signed-byte-32 source-register-value) 0))
    ; Bit 20 indicates that we either have BGEZAL or BLTZAL
    (when (ldb-test (byte 1 20) (instruction-word instruction))
      (set-register cpu 31 (cpu-next-program-counter cpu)))
    (branch cpu (sign-extend immediate))))

(def-j-type jmp #x02
  (setf (cpu-branch-opcode cpu) t)
  (setf (cpu-next-program-counter cpu)
        (logior
         (logand (cpu-program-counter cpu) #xf0000000)
         (ash jump-target 2))))

; TODO(Samantha): This doesn't use jump-target, should it just be
; def-instruction?
(def-j-type jal #x03
  (set-register cpu 31 (cpu-next-program-counter cpu))
  (jmp cpu instruction))

(def-i-type beq #x04
  (when (= source-register-value
           target-register-value)
    (branch cpu (sign-extend immediate))))

(def-i-type bne #x05
  (when (/= source-register-value
            target-register-value)
    (branch cpu (sign-extend immediate))))

(def-i-type blez #x06
  (when (<= (to-signed-byte-32 source-register-value) 0)
    (branch cpu (sign-extend immediate))))

(def-i-type bgtz #x07
  (when (> (to-signed-byte-32 source-register-value) 0)
    (branch cpu (sign-extend immediate))))

(def-i-type jr #xFF08
  (setf (cpu-branch-opcode cpu) t)
  (setf
   (cpu-next-program-counter cpu)
   source-register-value))

(def-r-type jalr #xFF09
  (set-register
   cpu
   destination-register-index
   (cpu-next-program-counter cpu))
  (jr cpu instruction))

(def-i-type syscall #xFF0C
  (trigger-exception cpu :cause :syscall))

; TODO(Samantha): Fix this shadowing.
(def-i-type break* #xFF0D
  (trigger-exception cpu :cause :breakpoint))

; TODO(Samantha): This isn't really a branch op, rfe just does some custodial
; work on the status register and leaves the rest up to the exception handler.
; Move this somewhere more appropriate?
(def-i-type rfe #xC0010
  ; TODO(Samantha): The way we're dealing with bit 10 from the cause register
  ; is really hacky. A getter and setter for this that handled this magic in
  ; the background would be much better.
  ; Zero out the exception in the cause register from hardware sources until we
  ; enter another.
  ; (setf
  ;  (ldb (byte 1 10) (cop0:cop0-cause-register (cpu-cop0 cpu)))
  ;  0)
  (setf
   (ldb (byte 4 0) (cop0:cop0-status-register (cpu-cop0 cpu)))
   (ldb
    (byte 4 0)
    (ash (ldb (byte 6 0) (cop0:cop0-status-register (cpu-cop0 cpu))) -2))))

(declaim (ftype (function (cpu (unsigned-byte 32)) (unsigned-byte 32))
                branch))
(defun branch (cpu offset)
  (setf (cpu-branch-opcode cpu) t)
  (setf
   (cpu-next-program-counter cpu)
   (wrap-word (+ (cpu-program-counter cpu) (ash offset 2)))))
