(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type addi #x08
  (let ((value (+
                ; We need to change these to (signed-byte-32), otherwise the
                ; overflow detection will just be wrong.
                (to-signed-byte-32 (sign-extend immediate))
                (to-signed-byte-32 (aref (cpu-registers cpu) source-register)))))
    (when (> value #x7FFFFFFF)
      (trigger-exception cpu :cause :arithmetic-overflow))
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

; ASH is a bit of a misnomer in the following instructions, it maintains sign,
; but for it to do that, we need to cast to a signed byte.
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
     (to-signed-byte-32 (aref (cpu-registers cpu) target-register))
     (* -1 shift-amount)))))

(def-r-type sllv #xFF04
  (set-register
   cpu destination-register
   (wrap-word
    ; TODO(Samantha): SBCL _really_ doesn't like this being sent out to
    ; wrap-word as a potentially 63 bit signed integer. Even just doing the
    ; logand manually quashes a warning. Ditto for SLL.
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

(def-r-type div #xFF1A
  (if (zerop (aref (cpu-registers cpu) target-register))
    ; Division by zero is not an exception, it's just the max/min value
    (progn
     (setf (cpu-hi cpu)
           (wrap-word
            (to-signed-byte-32 (aref (cpu-registers cpu) source-register))))
     ; The sign of the result changes depending on the sign of the dividend
     (setf (cpu-lo cpu)
           (wrap-word
            (if (>= (aref (cpu-registers cpu) source-register) 0)
              #x7FFFFFFF
              #x-80000000))))
    (if (and
         (= (to-signed-byte-32 (aref (cpu-registers cpu) source-register)) -1)
         (= (aref (cpu-registers cpu) target-register) #x80000000))
      ; The minimum value we can store in (signed-byte 32) is #x-7FFFFFFF. Once
      ; again, this isn't an exception, it just puts specific values in hi and lo.
      (progn
       (setf (cpu-hi cpu) 0)
       (setf (cpu-lo cpu) #x80000000))
      (progn
       (setf (cpu-hi cpu)
             (wrap-word
              (mod
               (aref (cpu-registers cpu) source-register)
               (aref (cpu-registers cpu) target-register))))
       (setf (cpu-lo cpu)
             (floor
              (aref (cpu-registers cpu) source-register)
              (aref (cpu-registers cpu) target-register)))))))

(def-r-type divu #xFF1B
  (if (zerop (aref (cpu-registers cpu) target-register))
    ; Division by zero is not an exception, it's just the max value
    (progn
     (setf (cpu-hi cpu) (aref (cpu-registers cpu) source-register))
     (setf (cpu-lo cpu) #xFFFFFFFF))
    (progn
     (setf (cpu-hi cpu)
           (mod
            (aref (cpu-registers cpu) source-register)
            (aref (cpu-registers cpu) target-register)))
     (setf (cpu-lo cpu)
           (floor
            (aref (cpu-registers cpu) source-register)
            (aref (cpu-registers cpu) target-register))))))

(def-r-type add #xFF20
  (let ((value (+
                (to-signed-byte-32 (aref (cpu-registers cpu) source-register))
                (to-signed-byte-32 (aref (cpu-registers cpu) target-register)))))
    (when (> value #x7FFFFFFF)
      (trigger-exception cpu :cause :arithmetic-overflow))
    (set-register cpu destination-register (wrap-word value))))

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
      (trigger-exception cpu :cause :arithmetic-overflow))
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
