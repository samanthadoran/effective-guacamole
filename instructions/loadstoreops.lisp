(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(declaim (ftype (function (cpu instruction) (values &optional)) lui))
(defun lui (cpu instruction)
  (setf
   (aref (cpu-registers cpu) (instruction-target-register instruction))
   (ash (instruction-immediate-value instruction) 16))
  (values))
