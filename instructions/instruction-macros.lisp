(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): Figure out how to change this macro to accept documentation.
(defmacro def-instruction (name opcode &body body)
  `(progn
    (declaim (ftype (function (cpu instruction) (values &optional)) ,name))
    (defun ,name (cpu instruction)
      ,@body
      (values))
    (setf (gethash ,opcode instructions) (list (string ',name) (function ,name)))))

; TODO(Samantha): Almost all of these instructions predictably set a register.
; Consider making it part of the macro and then special casing the outliers.

(defmacro def-i-type (name opcode &body body)
  `(def-instruction
     ,name
     ,opcode
     (let* ((source-register-index (instruction-source-register instruction))
            (source-register-value (aref (cpu-registers cpu)
                                         source-register-index))
            (target-register-index (instruction-target-register instruction))
            (target-register-value (aref (cpu-registers cpu)
                                         target-register-index))
            (immediate (instruction-immediate-value instruction))
            (address
             (wrap-word (+ (sign-extend immediate)
                           source-register-value))))
       (declare (ignorable address)
                ((unsigned-byte 32) source-register-value target-register-value)
                ((unsigned-byte 5) source-register-index target-register-index))
       ; Move load delay along
       (set-register cpu
                          (cpu-pending-load-register cpu)
                          (cpu-pending-load-value cpu))
       (setf (cpu-pending-load-register cpu) 0)
       (setf (cpu-pending-load-value cpu) 0)
       (when *debug-cpu*
         (format t "#x~8,'0x(~A): ~A $~D(#x~8,'0x) $~D(#x~8,'0x) #x~4,'0x~%"
                 (instruction-address instruction)
                 (instruction-segment instruction)
                 (instruction-mnemonic instruction)
                 target-register-index
                 target-register-value
                 source-register-index
                 source-register-value
                 immediate))
       ,@body)))

(defmacro def-j-type (name opcode &body body)
 `(def-instruction
    ,name
    ,opcode
    (let ((jump-target (instruction-jump-target instruction)))
      ; Move load delay along
      (set-register cpu
                         (cpu-pending-load-register cpu)
                         (cpu-pending-load-value cpu))
      (setf (cpu-pending-load-register cpu) 0)
      (setf (cpu-pending-load-value cpu) 0)
      (when *debug-cpu*
        (format t "#x~8,'0x(~A): ~A #x~6,'0x~%"
                (instruction-address instruction)
                (instruction-segment instruction)
                (instruction-mnemonic instruction)
                jump-target))
      ,@body)))

(defmacro def-r-type (name opcode &body body)
  `(def-instruction
     ,name
     ,opcode
     (let* ((source-register-index (instruction-source-register instruction))
            (source-register-value (aref (cpu-registers cpu)
                                         source-register-index))
            (target-register-index (instruction-target-register instruction))
            (target-register-value (aref (cpu-registers cpu)
                                         target-register-index))
            (destination-register-index (instruction-destination-register instruction))
            (destination-register-value (aref (cpu-registers cpu)
                                              destination-register-index))
            (shift-amount (instruction-shift-amount instruction)))
       (declare (ignorable shift-amount)
                ((unsigned-byte 32) source-register-value
                                    target-register-value
                                    destination-register-value)
                ((unsigned-byte 5) source-register-index
                                   target-register-index
                                   destination-register-index))
       ; Move load delay along
       (set-register cpu
                          (cpu-pending-load-register cpu)
                          (cpu-pending-load-value cpu))
       (setf (cpu-pending-load-register cpu) 0)
       (setf (cpu-pending-load-value cpu) 0)
       (when *debug-cpu*
         (format t "#x~8,'0x(~A): ~A $~D(#x~8,'0x) $~D(#x~8,'0x) $~D(#x~8,'0x)~%"
                 (instruction-address instruction)
                 (instruction-segment instruction)
                 (instruction-mnemonic instruction)
                 destination-register-index
                 destination-register-value
                 source-register-index
                 source-register-value
                 target-register-index
                 target-register-value))
       ,@body)))
