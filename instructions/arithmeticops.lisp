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
    (when (> value #XFFFFFFFF)
      (format t "Overflow behavior unimplemented. =(~%"))
    (set-register
     cpu target-register
     (ldb (byte 0 32) value))))

(def-i-type addiu #x09
  (set-register
   cpu target-register
   (ldb (byte 0 32)
        (+ (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))

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

(def-r-type sll #xFF00
  (set-register
   cpu destination-register
   (wrap-word (ash (aref (cpu-registers cpu) target-register) shift-amount))))
