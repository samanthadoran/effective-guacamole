(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-j-type jmp #x02
  (setf (cpu-program-counter cpu)
        (logior
         (logand (cpu-program-counter cpu) #xf0000000)
         (ash jump-target 2))))

; TODO(Samantha): This doesn't use jump-target, should it just be
; def-instruction?
(def-j-type jr #xFF08
  (setf
   (aref (cpu-registers cpu) 31)
   (cpu-program-counter cpu))
  (jmp cpu instruction))
