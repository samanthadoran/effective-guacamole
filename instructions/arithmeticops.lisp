(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type addiu #x09
  (setf
   (aref (cpu-registers cpu) target-register)
   (ldb (byte 32 0)
        (+ (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))

(def-i-type ori #x0D
  (setf
   (aref (cpu-registers cpu) target-register)
   (logior immediate (aref (cpu-registers cpu) source-register))))

; TODO(Samantha): Fix this shadowing.
(def-r-type or* #xFF25
  (setf
   (aref (cpu-registers cpu) destination-register)
   (logior
    (aref (cpu-registers cpu) source-register)
    (aref (cpu-registers cpu) target-register))))

(def-r-type sll #xFF00
  (setf
   (aref (cpu-registers cpu) destination-register)
   (wrap-word (ash (aref (cpu-registers cpu) target-register) shift-amount))))
