(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type lui #x0F
  (setf
   (aref (cpu-registers cpu) target-register)
   (ash immediate 16)))
