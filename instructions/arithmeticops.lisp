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
    ; This check works because we are treating them as unsigned anyway,
    ; otherwise we would need to check a different value.
    (when (> value #xFFFFFFFF)
      (error "Overflow behavior unimplemented. =(~%"))
    (set-register cpu target-register (wrap-word value))))

(def-i-type addiu #x09
  (set-register
   cpu target-register
   (wrap-word
        (+ (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))

(def-i-type slti #x0A
  (set-register
   cpu target-register
   (if (< (to-signed-byte-32 (aref (cpu-registers cpu) source-register))
          (to-signed-byte-32 (sign-extend immediate)))
     1
     0)))

(def-i-type sltiu #x0B
  (set-register
   cpu target-register
   (if (< (aref (cpu-registers cpu) source-register) immediate)
     1
     0)))

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

(def-r-type sll #xFF00
  (set-register
   cpu destination-register
   (wrap-word (ash (aref (cpu-registers cpu) target-register) shift-amount))))

(def-r-type srl #xFF02
  (set-register
   cpu destination-register
   (ash (aref (cpu-registers cpu) target-register) (* -1 shift-amount))))

(def-r-type sra #xFF03
  (set-register
   cpu destination-register
   (wrap-word
    (ash
      ; ASH is a bit of a misnomer here, it maintains sign, but for it to do
      ; that, we need to cast to a signed byte.
     (to-signed-byte-32 (aref (cpu-registers cpu) target-register))
     (* -1 shift-amount)))))

; TODO(Samantha): Find out a way to quash the warning about signed conversion
; that SBCL generates for this and SLL.
(def-r-type sllv #xFF04
  (set-register
   cpu destination-register
   (wrap-word
    (ash
     (aref (cpu-registers cpu) target-register)
     ; Shift from variable is masked by #x1F, or the lowest 5 bits.
     (ldb (byte 5 0) (aref (cpu-registers cpu) source-register))))))

(def-r-type srlv #xFF06
  (set-register
   cpu destination-register
   (ash
    (aref (cpu-registers cpu) target-register)
    ; Shift from variable is masked by #x1F, or the lowest 5 bits.
    (* -1 (ldb (byte 5 0) (aref (cpu-registers cpu) source-register))))))

(def-r-type srav #xFF07
  (set-register
   cpu destination-register
   (wrap-word
    (ash
     ; Make sure to cast so ash retains the sign.
     (to-signed-byte-32 (aref (cpu-registers cpu) target-register))
     ; Shift from variable is masked by #x1F, or the lowest 5 bits.
     (* -1 (ldb (byte 5 0) (aref (cpu-registers cpu) source-register)))))))

; TODO(Samantha): Are those `to-signed-byte-32`s going to work?
(def-r-type mult #xFF18
  (let ((result
         (* (to-signed-byte-32 (aref (cpu-registers cpu) source-register))
            (to-signed-byte-32 (aref (cpu-registers cpu) target-register)))))
    (setf (cpu-lo cpu) (ldb (byte 32 0) result))
    (setf (cpu-hi cpu) (ldb (byte 32 32) result))))

(def-r-type multu #xFF19
  (let ((result
         (* (aref (cpu-registers cpu) source-register)
            (aref (cpu-registers cpu) target-register))))
    (setf (cpu-lo cpu) (ldb (byte 32 0) result))
    (setf (cpu-hi cpu) (ldb (byte 32 32) result))))

(def-r-type add #xFF20
  (let ((value (+
                (aref (cpu-registers cpu) source-register)
                (aref (cpu-registers cpu) target-register))))
    (when (> value #xFFFFFFFF)
      (error "Overflow behavior unimplemented. =(~%"))
    (set-register cpu destination-register value)))

(def-r-type addu #xFF21
  (set-register
   cpu destination-register
   (wrap-word
    (+
     (aref (cpu-registers cpu) source-register)
     (aref (cpu-registers cpu) target-register)))))

(def-r-type sub #xFF22
  (let ((result
         (- (to-signed-byte-32 (aref (cpu-registers cpu) source-register))
            (to-signed-byte-32 (aref (cpu-registers cpu) target-register)))))
    ; TODO(Samantha): Is this really doing what I think it's doing?
    ; TODO(Samantha): Figure out a testing framework for common lisp.
    (when (< result #x-80000000)
      (error "Signed overflow unimplemented. =(~%"))
    (set-register cpu destination-register result)))

(def-r-type subu #xFF23
  (set-register
   cpu destination-register
   (wrap-word (- (aref (cpu-registers cpu) source-register)
                 (aref (cpu-registers cpu) target-register)))))

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
