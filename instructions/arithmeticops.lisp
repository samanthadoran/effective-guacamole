(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type addi #x08
  (format t "Adding 0x~8,'0X to 0x~8,'0X Result is 0x~8,'0X~%"
          (sign-extend immediate)
          (aref (cpu-registers cpu) source-register)
          (+ (sign-extend immediate)
                          (aref (cpu-registers cpu) source-register)))
  (let ((value (+ (sign-extend immediate)
                  (aref (cpu-registers cpu) source-register))))
    ; TODO(Samantha): Consider declaring the type of value so this is
    ; guaranteed to work.
    (when (> value #xFFFFFFFF)
      (format t "Overflow behavior unimplemented. =(~%"))
    (set-register
     cpu target-register
     (wrap-word value))))

(def-i-type addiu #x09
  (set-register
   cpu target-register
   (wrap-word
        (+ (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))

(def-r-type addu #xFF21
  (set-register
   cpu destination-register
   (wrap-word
    (+
     (aref (cpu-registers cpu) source-register)
     (aref (cpu-registers cpu) target-register)))))

(def-i-type andi #x0C
  (set-register
   cpu target-register
   (logand immediate (aref (cpu-registers cpu) source-register))))

(def-i-type ori #x0D
  (set-register
   cpu target-register
   (logior immediate (aref (cpu-registers cpu) source-register))))

(def-i-type xori #x0E
  (set-register
   cpu target-register
   (logxor immediate (aref (cpu-registers cpu) source-register))))

; TODO(Samantha): Fix this shadowing.
(def-r-type and* #xFF24
  (set-register
   cpu destination-register
   (logand
    (aref (cpu-registers cpu) source-register)
    (aref (cpu-registers cpu) target-register))))

(def-r-type or* #xFF25
  (set-register
   cpu destination-register
   (logior
    (aref (cpu-registers cpu) source-register)
    (aref (cpu-registers cpu) target-register))))

(def-r-type xor* #xFF26
  (set-register
   cpu destination-register
   (logxor
    (aref (cpu-registers cpu) source-register)
    (aref (cpu-registers cpu) target-register))))

(def-r-type nor* #xFF27
  (set-register
   cpu destination-register
   (wrap-word
    (lognot
     (logior
      (aref (cpu-registers cpu) source-register)
      (aref (cpu-registers cpu) target-register))))))

(def-r-type slt #xFF2A
  (set-register
   cpu destination-register
   (if (< (to-signed-byte-32 (aref (cpu-registers cpu) source-register))
          (to-signed-byte-32 (aref (cpu-registers cpu) target-register)))
     1
     0)))

(def-r-type sltu #xFF2B
  (set-register
   cpu destination-register
   (if (< (aref (cpu-registers cpu) source-register)
          (aref (cpu-registers cpu) target-register))
     1
     0)))

(def-r-type sll #xFF00
  (set-register
   cpu destination-register
   (wrap-word (ash (aref (cpu-registers cpu) target-register) shift-amount))))
