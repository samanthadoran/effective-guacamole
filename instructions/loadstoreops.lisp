(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type lui #x0F
  (setf
   (aref (cpu-registers cpu) target-register)
   (ash immediate 16)))

(def-i-type sw #x2B
  (write-cpu-word
   cpu
   (wrap-word
    (+
     (to-signed-byte-32 immediate)
     (aref (cpu-registers cpu) source-register)))
   (aref (cpu-registers cpu) target-register)))
