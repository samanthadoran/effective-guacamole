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
     (let ((source-register (instruction-source-register instruction))
           (target-register (instruction-target-register instruction))
           (immediate (instruction-immediate-value instruction)))
       (declare (ignorable source-register target-register immediate))
       (when *debug-cpu*
         (format t "#x~8,'0x(~A): ~A $~D $~D #x~4,'0x~%"
                 (instruction-address instruction)
                 (instruction-segment instruction)
                 (instruction-mnemonic instruction)
                 target-register
                 source-register
                 immediate))
       ,@body)))

(defmacro def-j-type (name opcode &body body)
 `(def-instruction
    ,name
    ,opcode
    (let ((jump-target (instruction-jump-target instruction)))
      (declare (ignorable jump-target))
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
     (let ((source-register (instruction-source-register instruction))
           (target-register (instruction-target-register instruction))
           (destination-register (instruction-destination-register instruction))
           (shift-amount (instruction-shift-amount instruction)))
       (declare (ignorable source-register target-register
                           destination-register shift-amount))
       (when *debug-cpu*
         (format t "#x~8,'0x(~A): ~A $~D $~D $~D~%"
                 (instruction-address instruction)
                 (instruction-segment instruction)
                 (instruction-mnemonic instruction)
                 destination-register
                 source-register
                 target-register))
       ,@body)))
