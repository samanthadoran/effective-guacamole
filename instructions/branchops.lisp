(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-j-type jmp #x02
  (setf (cpu-program-counter cpu)
        (logior
         (logand (cpu-program-counter cpu) #xf0000000)
         (ash jump-target 2))))

; TODO(Samantha): This doesn't use jump-target, should it just be
; def-instruction?
(def-j-type jal #x03
  (setf
   (aref (cpu-registers cpu) 31)
   (cpu-program-counter cpu))
  (jmp cpu instruction))

(def-i-type jr #xFF08
  (setf
   (cpu-program-counter cpu)
   (aref (cpu-registers cpu) source-register)))

(def-r-type jalr #xFF09
  (setf
   (aref (cpu-registers cpu) destination-register)
   (cpu-program-counter cpu))
  (jr cpu instruction))

(declaim (ftype (function (cpu (unsigned-byte 32)) (unsigned-byte 32))
                branch))
(defun branch (cpu offset)
  (setf
   (cpu-program-counter cpu)
   (wrap-word
    ; Move the program counter by the specified offset and then back one to
    ; account for the already incremented program counter.
    (+ (cpu-program-counter cpu) (ash offset 2) -4))))

(def-i-type beq #x04
  (when (= (aref (cpu-registers cpu) source-register)
           (aref (cpu-registers cpu) target-register))
    (branch cpu (sign-extend immediate))))

(def-i-type bne #x05
  (when (/= (aref (cpu-registers cpu) source-register)
            (aref (cpu-registers cpu) target-register))
    (branch cpu (sign-extend immediate))))

(def-i-type blez #x06
  (when (<= (aref (cpu-registers cpu) source-register) 0)
    (branch cpu (sign-extend immediate))))

(def-i-type bgtz #x07
  (when (> (aref (cpu-registers cpu) source-register) 0)
    (branch cpu (sign-extend immediate))))
