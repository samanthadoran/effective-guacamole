(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type lui #x0F
  (setf
   (aref (cpu-registers cpu) target-register)
   (ash immediate 16)))

(def-r-type mfc0 #xC0000
  (setf
   (aref (cpu-registers cpu) target-register)
   (case destination-register
     (otherwise (progn (format t "Unknown read to cop0$~d~%" destination-register) 0)))))

(def-r-type mtc0 #xC0004
  (case destination-register
    (12
     (setf
      (cpu-status-register cpu)
      (aref (cpu-registers cpu) target-register)))
    (otherwise
     (progn
      (format t "Unknown write of 0x~8,'0X to cop0$~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)
      0))))

(def-i-type sw #x2B
  (write-cpu-word
   cpu
   (wrap-word
    (+
     (sign-extend immediate)
     (aref (cpu-registers cpu) source-register)))
   (aref (cpu-registers cpu) target-register)))
