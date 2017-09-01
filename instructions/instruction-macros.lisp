(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): This insane repetition could probably be made into four
; separate macros: `def-instruction` `def-i-type` `def-r-type` `def-j-type`,
; where the last three would call the first.

(defmacro def-instruction (name opcode &body body)
  `(progn
    (declaim (ftype (function (cpu instruction) (values &optional)) ,name))
    (defun ,name (cpu instruction)
      ,@body
      (values))
    (setf (gethash ,opcode instructions) (list (string ',name) (function ,name)))))

(defmacro def-i-type (name opcode &body body)
  `(def-instruction
     ,name
     ,opcode
     (let ((source-register (instruction-source-register instruction))
           (target-register (instruction-target-register instruction))
           (immediate (instruction-immediate-value instruction)))
       (declare (ignorable source-register target-register immediate))
       ,@body)))

(defmacro def-j-type (name opcode &body body)
 `(def-instruction
    ,name
    ,opcode
    (let ((jump-target (instruction-jump-target instruction)))
      (declare (ignorable jump-target))
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
       ,@body)))
