(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defun is-cache-isolated (cpu)
  (ldb-test (byte 1 16) (cop0:cop0-status-register (cpu-cop0 cpu))))

(def-i-type lui #x0F
  (set-register cpu target-register (ash immediate 16)))

; TODO(Samantha): Consider making a macro for cache sensitivity.
(def-i-type lb #x20
  (setf (cpu-pending-load-register cpu) target-register)
  (setf
   (cpu-pending-load-value cpu)
   (sign-extend-byte
    (funcall (cpu-memory-get-byte cpu)
             (wrap-word
              (+
               (sign-extend immediate)
               (aref (cpu-registers cpu) source-register)))))))

(def-i-type lh #x21
  (let ((address
         (wrap-word
          (+
           (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))
    (if (/= 0 (mod address 2))
      (trigger-exception cpu :cause :address-load-error)
      (progn
       (setf (cpu-pending-load-register cpu) target-register)
       (setf
        (cpu-pending-load-value cpu)
        (sign-extend (funcall (cpu-memory-get-half-word cpu) address)))))))

; TODO(Samantha): Shouldn't this and lwr be cache conscious?
; LWL and LWR are both meant to be executed in sequence, it wouldn't make sense
; for them to have a load delay.
(def-i-type lwl #x22
  (let* ((address
          (wrap-word
           (+
            (aref (cpu-registers cpu) source-register)
            (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (current-value (aref (cpu-registers cpu) target-register)))
    (set-register cpu target-register
                  (case (ldb (byte 2 0) address)
                    (0 (logior
                        (logand current-value #x00FFFFFF)
                        (wrap-word (ash aligned-word 24))))
                    (1 (logior
                        (logand current-value #x0000FFFF)
                        (wrap-word (ash aligned-word 16))))
                    (2 (logior
                        (logand current-value #x000000FF)
                        (wrap-word (ash aligned-word 8))))
                    (3 (logior
                        (logand current-value #x00000000)
                        (wrap-word (ash aligned-word 0))))
                    (otherwise (error "Unreachable.~%"))))))

(def-i-type lw #x23
  (let ((address
         (wrap-word
          (+
           (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))
    (if (/= 0 (mod address 4))
      (trigger-exception cpu :cause :address-load-error)
      (progn
       (setf (cpu-pending-load-register cpu) target-register)
       (setf
        (cpu-pending-load-value cpu)
        (funcall (cpu-memory-get-word cpu) address))))))

(def-i-type lbu #x24
  (setf (cpu-pending-load-register cpu) target-register)
  (setf (cpu-pending-load-value cpu)
        (funcall (cpu-memory-get-byte cpu)
                 (wrap-word
                  (+
                   (sign-extend immediate)
                   (aref (cpu-registers cpu) source-register))))))

(def-i-type lhu #x25
  (let ((address
         (wrap-word
          (+
           (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))
    (if (/= 0 (mod address 2))
      (trigger-exception cpu :cause :load-address-error)
      (progn
       (setf (cpu-pending-load-register cpu) target-register)
       (setf
        (cpu-pending-load-value cpu)
        (funcall (cpu-memory-get-half-word cpu) address))))))

(def-i-type lwr #x26
  (let* ((address
          (wrap-word
           (+
            (aref (cpu-registers cpu) source-register)
            (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (current-value (aref (cpu-registers cpu) target-register)))
    (set-register cpu target-register
                  (case (ldb (byte 2 0) address)
                    (0 (logior
                        (logand current-value #x00000000)
                        (wrap-word (ash aligned-word 0))))
                    (1 (logior
                        (logand current-value #xFF000000)
                        (wrap-word (ash aligned-word -8))))
                    (2 (logior
                        (logand current-value #xFFFF0000)
                        (wrap-word (ash aligned-word -16))))
                    (3 (logior
                        (logand current-value #xFFFFFF00)
                        (wrap-word (ash aligned-word -24))))
                    (otherwise (error "Unreachable.~%"))))))

(def-i-type sb #x28
  (unless (is-cache-isolated cpu)
    (funcall (cpu-memory-set-byte cpu)
             (wrap-word
              (+
               (sign-extend immediate)
               (aref (cpu-registers cpu) source-register)))
             (ldb (byte 8 0) (aref (cpu-registers cpu) target-register)))))

(def-i-type sh #x29
  (let ((address
         (wrap-word
          (+
           (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))
    (if (/= 0 (mod address 2))
      (trigger-exception cpu :cause :address-write-error)
      (unless (is-cache-isolated cpu)
        (funcall (cpu-memory-set-half-word cpu)
                 address
                 (ldb (byte 16 0) (aref (cpu-registers cpu) target-register)))))))

(def-i-type swl #x2A
  (let* ((address
          (wrap-word
           (+
            (aref (cpu-registers cpu) source-register)
            (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (value (aref (cpu-registers cpu) target-register)))
    (funcall (cpu-memory-set-word cpu)
             aligned-address
             (case (ldb (byte 2 0) address)
               (0 (logior
                   (logand aligned-word #xFFFFFF00)
                   (wrap-word (ash value 24))))
               (1 (logior
                   (logand aligned-word #xFFFF0000)
                   (wrap-word (ash value 16))))
               (2 (logior
                   (logand aligned-word #xFF000000)
                   (wrap-word (ash value 8))))
               (3 (logior
                   (logand aligned-word #x00000000)
                   (wrap-word (ash value 0))))
               (otherwise (error "Unreachable.~%"))))))

(def-i-type sw #x2B
  (let ((address
         (wrap-word
          (+
           (sign-extend immediate)
           (aref (cpu-registers cpu) source-register)))))
    (if (/= 0 (mod address 4))
      (trigger-exception cpu :cause :address-write-error)
      (unless (is-cache-isolated cpu)
        (funcall (cpu-memory-set-word cpu)
                 address
                 (aref (cpu-registers cpu) target-register))))))

(def-i-type swr #x2E
  (let* ((address
          (wrap-word
           (+
            (aref (cpu-registers cpu) source-register)
            (sign-extend immediate))))
         (aligned-address (logand address #xFFFFFF00))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (value (aref (cpu-registers cpu) target-register)))
    (funcall (cpu-memory-set-word cpu)
             aligned-address
             (case (ldb (byte 2 0) address)
               (0 (logior
                   (logand aligned-word #x00000000)
                   (wrap-word (ash value 0))))
               (1 (logior
                   (logand aligned-word #x000000FF)
                   (wrap-word (ash value 8))))
               (2 (logior
                   (logand aligned-word #x0000FFFF)
                   (wrap-word (ash value 16))))
               (3 (logior
                   (logand aligned-word #x00FFFFFF)
                   (wrap-word (ash value 24))))
               (otherwise (error "Unreachable.~%"))))))

; TODO(Samantha): CPU in lwc2 and swc2 is only referenced to quash a warning,
; implement them so we can remove this ugly hack.
(def-i-type lwc0 #x30
  (trigger-exception cpu :cause :coprocessor-unusable))

(def-i-type lwc1 #x31
  (trigger-exception cpu :cause :coprocessor-unusable))

(def-i-type lwc2 #x32
  cpu
  (error "lwc2 not yet implemented!~%"))

(def-i-type lwc3 #x33
  (trigger-exception cpu :cause :coprocessor-unusable))

(def-i-type swc0 #x38
  (trigger-exception cpu :cause :coprocessor-unusable))

(def-i-type swc1 #x39
  (trigger-exception cpu :cause :coprocessor-unusable))

(def-i-type swc2 #x3A
  cpu
  (error "swc2 not yet implemented!~%"))

(def-i-type swc3 #x3B
  (trigger-exception cpu :cause :coprocessor-unusable))

; TODO(Samantha): Consider making this io a bit more generic, it's
; frustrating to repeat myself.
(def-r-type mfc0 #xC0000
  (setf (cpu-pending-load-register cpu) target-register)
  (setf
   (cpu-pending-load-value cpu)
   (case destination-register
     (12 (cop0:cop0-status-register (cpu-cop0 cpu)))
     (13 (cop0:cop0-cause-register (cpu-cop0 cpu)))
     (14 (cop0:cop0-epc-register (cpu-cop0 cpu)))
     (otherwise
      (error "Unknown read to cop0$~d~%" destination-register)
      0))))

(def-r-type mtc0 #xC0004
  (case destination-register
    ; TODO(Samantha): Handle anything other than $cop0_12.
    ; BPC
    (3
     (unless (zerop (aref (cpu-registers cpu) target-register))
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)))
    ; BDA
    (5
     (unless (zerop (aref (cpu-registers cpu) target-register))
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              (aref (cpu-registers cpu) target-register)
              destination-register)))
    ; JUMPDEST
    (6
      (unless (zerop (aref (cpu-registers cpu) target-register))
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; DCIC
    (7
      (unless (zerop (aref (cpu-registers cpu) target-register))
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; BDAM
    (9
      (unless (zerop (aref (cpu-registers cpu) target-register))
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    ; BPCM
    (11
      (unless (zerop (aref (cpu-registers cpu) target-register))
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               (aref (cpu-registers cpu) target-register)
               destination-register)))
    (12
     (setf
      (cop0:cop0-status-register (cpu-cop0 cpu))
      (aref (cpu-registers cpu) target-register)))
    ; CAUSE
    (13
     (unless (zerop (aref (cpu-registers cpu) target-register))
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
