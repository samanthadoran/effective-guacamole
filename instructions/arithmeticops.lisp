(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type ori #x0D
  (setf
   (aref (cpu-registers cpu) target-register)
   (logior immediate (aref (cpu-registers cpu) source-register))))
