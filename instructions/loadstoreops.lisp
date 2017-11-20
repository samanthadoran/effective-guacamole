(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defun is-cache-isolated (cpu)
  (ldb-test (byte 1 16) (cpu-status-register cpu)))

(def-i-type lui #x0F
  (setf
   (aref (cpu-registers cpu) target-register)
   (ash immediate 16)))

; TODO(Samantha): Consider making a macro for cache sensitivity.
(def-i-type lb #x20
  (when (not (is-cache-isolated cpu))
    (set-register
     cpu target-register
     (sign-extend-byte
      (read-cpu-byte
       cpu
       (wrap-word
        (+
         (sign-extend immediate)
         (aref (cpu-registers cpu) source-register))))))))

(def-i-type lh #x21
  (when (not (is-cache-isolated cpu))
    (set-register
     cpu target-register
     (sign-extend
      (read-cpu-half-word
       cpu
       (wrap-word
        (+
         (sign-extend immediate)
         (aref (cpu-registers cpu) source-register))))))))

(def-i-type lwl #x22
  (let* ((address (wrap-word (+ (aref (cpu-registers cpu) source-register) (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (read-cpu-word cpu aligned-address))
         (current-value (aref (cpu-registers cpu) target-register)))
    (set-register cpu target-register
                  (case (ldb (byte 2 0) address)
                    (0 (logior (logand current-value #x00FFFFFF) (wrap-word (ash aligned-word 24))))
                    (1 (logior (logand current-value #x0000FFFF) (wrap-word (ash aligned-word 16))))
                    (2 (logior (logand current-value #x000000FF) (wrap-word (ash aligned-word 8))))
                    (3 (logior (logand current-value #x00000000) (wrap-word (ash aligned-word 0))))
                    (otherwise (error "Unreachable.~%"))))))

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

(def-i-type lbu #x24
  (when (not (is-cache-isolated cpu))
    (set-register
     cpu target-register
     (read-cpu-byte
      cpu
      (wrap-word
       (+
        (sign-extend immediate)
        (aref (cpu-registers cpu) source-register)))))))

(def-i-type lhu #x25
  (when (not (is-cache-isolated cpu))
    (set-register
     cpu target-register
     (read-cpu-half-word
      cpu
      (wrap-word
       (+
        (sign-extend immediate)
        (aref (cpu-registers cpu) source-register)))))))

(def-i-type lwr #x26
  (let* ((address (wrap-word (+ (aref (cpu-registers cpu) source-register) (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (read-cpu-word cpu aligned-address))
         (current-value (aref (cpu-registers cpu) target-register)))
    (set-register cpu target-register
                  (case (ldb (byte 2 0) address)
                    (0 (logior (logand current-value #x00000000) (wrap-word (ash aligned-word 0))))
                    (1 (logior (logand current-value #xFF000000) (wrap-word (ash aligned-word -8))))
                    (2 (logior (logand current-value #xFFFF0000) (wrap-word (ash aligned-word -16))))
                    (3 (logior (logand current-value #xFFFFFF00) (wrap-word (ash aligned-word -24))))
                    (otherwise (error "Unreachable.~%"))))))

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

(def-i-type sw #x2B
  (when (not (is-cache-isolated cpu))
    (write-cpu-word
     cpu
     (wrap-word
      (+
       (sign-extend immediate)
       (aref (cpu-registers cpu) source-register)))
     (aref (cpu-registers cpu) target-register))))



; TODO(Samantha): Consider making this io a bit more generic, it's
; frustrating to repeat myself.
(def-r-type mfc0 #xC0000
  (set-register cpu target-register
                (case destination-register
                  (12 (cpu-status-register cpu))
                  (otherwise
                   (error "Unknown read to cop0$~d~%" destination-register)
                   0))))

(def-r-type mtc0 #xC0004
  (case destination-register
    ; TODO(Samantha): Handle anything other than $cop0_12.
    ; BPC
    (3
     (when (/= (aref (cpu-registers cpu) target-register) 0)
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)))
    ; BDA
    (5
     (when (/= (aref (cpu-registers cpu) target-register) 0)
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)))
    ; JUMPDEST
    (6
      (when (/= (aref (cpu-registers cpu) target-register) 0)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; DCIC
    (7
      (when (/= (aref (cpu-registers cpu) target-register) 0)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; BDAM
    (9
      (when (/= (aref (cpu-registers cpu) target-register) 0)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; BPCM
    (11
      (when (/= (aref (cpu-registers cpu) target-register) 0)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    (12
     (setf
      (cpu-status-register cpu)
      (aref (cpu-registers cpu) target-register)))
    ; CAUSE
    (13
     (when (/= (aref (cpu-registers cpu) target-register) 0)
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)))
    (otherwise
     (error "Unknown write of 0x~8,'0X to $cop0_~d~%"
             (aref (cpu-registers cpu) target-register)
             destination-register)
     0)))

(def-r-type mfhi #xFF10
  (set-register
   cpu destination-register
   (cpu-hi cpu)))

(def-r-type mthi #xFF11
  (setf (cpu-hi cpu) (aref (cpu-registers cpu) source-register)))

(def-r-type mflo #xFF12
  (set-register
   cpu destination-register
   (cpu-lo cpu)))

(def-r-type mtlo #xFF13
  (setf (cpu-lo cpu) (aref (cpu-registers cpu) source-register)))
