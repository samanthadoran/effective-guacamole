(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(def-i-type addi #x08
  (let ((value (+
                ; We need to change these to (signed-byte-32), otherwise the
                ; overflow detection will just be wrong.
                (to-signed-byte-32 (sign-extend immediate))
                (to-signed-byte-32 source-register-value))))
    (if (or (< value #x-80000000) (> value #x7FFFFFFF))
      (trigger-exception cpu :cause :arithmetic-overflow)
      (set-register cpu target-register-index (wrap-word value)))))

(def-i-type addiu #x09
  (set-register
   cpu target-register-index
   (wrap-word
    (+ (sign-extend immediate)
       source-register-value))))

(def-i-type slti #x0A
  (set-register
   cpu target-register-index
   (if (< (to-signed-byte-32 source-register-value)
          (to-signed-byte-32 (sign-extend immediate)))
     1
     0)))

(def-i-type sltiu #x0B
  (set-register
   cpu target-register-index
   (if (< source-register-value
          (sign-extend immediate))
     1
     0)))

(def-i-type andi #x0C
  (set-register
   cpu target-register-index
   (logand immediate source-register-value)))

(def-i-type ori #x0D
  (set-register
   cpu target-register-index
   (logior immediate source-register-value)))

(def-i-type xori #x0E
  (set-register
   cpu target-register-index
   (logxor immediate source-register-value)))

; ASH is a bit of a misnomer in the following instructions, it maintains sign,
; but for it to do that, we need to cast to a signed byte.
(def-r-type sll #xFF00
  (set-register
   cpu destination-register-index
   (wrap-word (ash target-register-value shift-amount))))

(def-r-type srl #xFF02
  (set-register
   cpu destination-register-index
   (ash target-register-value (* -1 shift-amount))))

(def-r-type sra #xFF03
  (set-register
   cpu destination-register-index
   (wrap-word
    (ash
     (to-signed-byte-32 target-register-value)
     (* -1 shift-amount)))))

(def-r-type sllv #xFF04
  (set-register
   cpu destination-register-index
   (wrap-word
    ; TODO(Samantha): SBCL _really_ doesn't like this being sent out to
    ; wrap-word as a potentially 63 bit signed integer. Even just doing the
    ; logand manually quashes a warning. Ditto for SLL.
    (ash
     target-register-value
     ; Shift from variable is masked by #x1F, or the lowest 5 bits.
     (ldb (byte 5 0) source-register-value)))))

(def-r-type srlv #xFF06
  (set-register
   cpu destination-register-index
   (ash
    target-register-value
    ; Shift from variable is masked by #x1F, or the lowest 5 bits.
    (* -1 (ldb (byte 5 0) source-register-value)))))

(def-r-type srav #xFF07
  (set-register
   cpu destination-register-index
   (wrap-word
    (ash
     ; Make sure to cast so ash retains the sign.
     (to-signed-byte-32 target-register-value)
     ; Shift from variable is masked by #x1F, or the lowest 5 bits.
     (* -1 (ldb (byte 5 0) source-register-value))))))

; TODO(Samantha): Are those `to-signed-byte-32`s going to work?
(def-r-type mult #xFF18
  (let ((result
         (* (to-signed-byte-32 source-register-value)
            (to-signed-byte-32 target-register-value))))
    (setf (cpu-lo cpu) (ldb (byte 32 0) result))
    (setf (cpu-hi cpu) (ldb (byte 32 32) result))))

(def-r-type multu #xFF19
  (let ((result
         (* source-register-value
            target-register-value)))
    (setf (cpu-lo cpu) (ldb (byte 32 0) result))
    (setf (cpu-hi cpu) (ldb (byte 32 32) result))))

(def-r-type div #xFF1A
  (let ((numerator (to-signed-byte-32 source-register-value))
        (denominator (to-signed-byte-32 target-register-value)))
    (if (zerop denominator)
      ; Division by zero is not an exception, it's just the max/min value
      (progn
       (setf (cpu-hi cpu) (wrap-word numerator))
       (setf (cpu-lo cpu)
             (if (>= numerator 0)
               #xFFFFFFFF
               #x1)))
      (if (and (= denominator -1) (= (wrap-word numerator) #x80000000))
        ; This would overflow and causes a bogus value.
        (progn
         (setf (cpu-hi cpu) 0)
         (setf (cpu-lo cpu) #x80000000))
        (progn
         ; TODO(Samantha): I'm not entirely certain the mod is working how I'm
         ; expecting it to. Study how psx handles mod with combinations of
         ; negative numerators and denominators. For now, just use rem.
         (setf (cpu-hi cpu) (wrap-word (rem numerator denominator)))
         (setf (cpu-lo cpu) (wrap-word (truncate numerator denominator))))))))

(def-r-type divu #xFF1B
  (let ((numerator source-register-value)
        (denominator target-register-value))
    (if (zerop denominator)
      (progn
       (setf (cpu-hi cpu) numerator)
       (setf (cpu-lo cpu) #xFFFFFFFF))
      (progn
       (setf (cpu-hi cpu) (mod numerator denominator))
       (setf (cpu-lo cpu) (floor numerator denominator))))))

(def-r-type add #xFF20
  (let ((value (+
                (to-signed-byte-32 source-register-value)
                (to-signed-byte-32 target-register-value))))
    (if (or (< value #x-80000000) (> value #x7FFFFFFF))
      (trigger-exception cpu :cause :arithmetic-overflow)
      (set-register cpu destination-register-index (wrap-word value)))))

(def-r-type addu #xFF21
  (set-register
   cpu destination-register-index
   (wrap-word
    (+
     source-register-value
     target-register-value))))

(def-r-type sub #xFF22
  (let ((result
         (- (to-signed-byte-32 source-register-value)
            (to-signed-byte-32 target-register-value))))
    ; TODO(Samantha): Is this really doing what I think it's doing?
    ; TODO(Samantha): Figure out a testing framework for common lisp.
    (if (or (< result #x-80000000) (>= result #x80000000))
      (trigger-exception cpu :cause :arithmetic-overflow)
      (set-register cpu destination-register-index (wrap-word result)))))

(def-r-type subu #xFF23
  (set-register
   cpu destination-register-index
   (wrap-word (- source-register-value
                 target-register-value))))

; TODO(Samantha): Fix this shadowing.
(def-r-type and* #xFF24
  (set-register
   cpu destination-register-index
   (logand
    source-register-value
    target-register-value)))

(def-r-type or* #xFF25
  (set-register
   cpu destination-register-index
   (logior
    source-register-value
    target-register-value)))

(def-r-type xor* #xFF26
  (set-register
   cpu destination-register-index
   (logxor
    source-register-value
    target-register-value)))

(def-r-type nor* #xFF27
  (set-register
   cpu destination-register-index
   (wrap-word
    (lognot
     (logior
      source-register-value
      target-register-value)))))

(def-r-type slt #xFF2A
  (set-register
   cpu destination-register-index
   (if (< (to-signed-byte-32 source-register-value)
          (to-signed-byte-32 target-register-value))
     1
     0)))

(def-r-type sltu #xFF2B
  (set-register
   cpu destination-register-index
   (if (< source-register-value
          target-register-value)
     1
     0)))
