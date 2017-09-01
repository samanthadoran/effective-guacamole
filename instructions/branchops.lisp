(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): There's nothing accounting for the branch delay slot. Fix this
(def-j-type jmp #x02
  (setf (cpu-program-counter cpu)
        (logior
         (logand (cpu-program-counter cpu) #xf0000000)
         (ash jump-target 2))))
