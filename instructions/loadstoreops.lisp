(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defun is-cache-isolated (cpu)
  (ldb-test (byte 1 16) (cpu-status-register cpu)))

(def-i-type lui #x0F
  (setf
   (aref (cpu-registers cpu) target-register)
   (ash immediate 16)))

; TODO(Samantha): Consider making this io a bit more generic, it's
; frustrating to repeat myself.
(def-r-type mfc0 #xC0000
  (set-register cpu target-register
    (case destination-register
      (12 (cpu-status-register cpu))
      (otherwise
       (format t "Unknown read to cop0$~d~%" destination-register)
       0))))

(def-r-type mtc0 #xC0004
  (case destination-register
    (12
     (setf
      (cpu-status-register cpu)
      (aref (cpu-registers cpu) target-register)))
    (otherwise
     (format t "Unknown write of 0x~8,'0X to cop0$~d~%"
             (aref (cpu-registers cpu) target-register)
             destination-register)
     0)))

(def-i-type lw #x23
  (when (not (is-cache-isolated cpu))
    (set-register
     cpu target-register
     (read-cpu-word
      cpu
      (wrap-word
       (+
        (sign-extend immediate)
        (aref (cpu-registers cpu) source-register)))))))

(def-i-type sw #x2B
  (when (not (is-cache-isolated cpu))
    (write-cpu-word
     cpu
     (wrap-word
      (+
       (sign-extend immediate)
       (aref (cpu-registers cpu) source-register)))
     (aref (cpu-registers cpu) target-register))))

(def-i-type sb #x28
  (when (not (is-cache-isolated cpu))
    (write-cpu
     cpu
     (wrap-word
      (+
       (sign-extend immediate)
       (aref (cpu-registers cpu) source-register)))
     (ldb (byte 8 0) (aref (cpu-registers cpu) target-register)))))

(def-i-type sh #x29
  (when (not (is-cache-isolated cpu))
    (write-cpu-half-word
     cpu
     (wrap-word
      (+
       (sign-extend immediate)
       (aref (cpu-registers cpu) source-register)))
     (ldb (byte 16 0) (aref (cpu-registers cpu) target-register)))))
