(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

(defun is-cache-isolated (cpu)
  (ldb-test (byte 1 16) (cop0:cop0-status-register (cpu-cop0 cpu))))

(def-i-type lui #x0F
  (set-register cpu target-register-index (ash immediate 16)))

; TODO(Samantha): Consider making a macro for cache sensitivity.
(def-i-type lb #x20
  (setf (cpu-pending-load-register cpu) target-register-index)
  (setf
   (cpu-pending-load-value cpu)
   (sign-extend-byte (funcall (cpu-memory-get-byte cpu) address))))

(def-i-type lh #x21
  (if (/= 0 (mod address 2))
    (trigger-exception cpu :cause :address-load-error)
    (progn
     (setf (cpu-pending-load-register cpu) target-register-index)
     (setf
      (cpu-pending-load-value cpu)
      (sign-extend (funcall (cpu-memory-get-half-word cpu) address))))))

; TODO(Samantha): Shouldn't this and lwr be cache conscious?
; LWL and LWR are both meant to be executed in sequence, it wouldn't make sense
; for them to have a load delay.
(def-i-type lwl #x22
  (let* ((aligned-address (logand address #xFFFFFFFC))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (current-value target-register-value))
    (setf (cpu-pending-load-register cpu) target-register-index)
    (setf (cpu-pending-load-value cpu)
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
  (if (/= 0 (mod address 4))
    (trigger-exception cpu :cause :address-load-error)
    (progn
     (setf (cpu-pending-load-register cpu) target-register-index)
     (setf
      (cpu-pending-load-value cpu)
      (funcall (cpu-memory-get-word cpu) address)))))

(def-i-type lbu #x24
  (setf (cpu-pending-load-register cpu) target-register-index)
  (setf (cpu-pending-load-value cpu)
        (funcall (cpu-memory-get-byte cpu) address)))

(def-i-type lhu #x25
  (if (/= 0 (mod address 2))
    (trigger-exception cpu :cause :load-address-error)
    (progn
     (setf (cpu-pending-load-register cpu) target-register-index)
     (setf
      (cpu-pending-load-value cpu)
      (funcall (cpu-memory-get-half-word cpu) address)))))

(def-i-type lwr #x26
  (let* ((aligned-address (logand address #xFFFFFFFC))
         (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
         (current-value target-register-value))
    (setf (cpu-pending-load-register cpu) target-register-index)
    (setf (cpu-pending-load-value cpu)
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
  (if (is-cache-isolated cpu)
    (invalidate-cache cpu)
    (funcall (cpu-memory-set-byte cpu)
             address
             (ldb (byte 8 0) target-register-value))))

(def-i-type sh #x29
  (if (/= 0 (mod address 2))
    (trigger-exception cpu :cause :address-write-error)
    (if (is-cache-isolated cpu)
      (invalidate-cache cpu)
      (funcall (cpu-memory-set-half-word cpu)
               address
               (ldb (byte 16 0) target-register-value)))))

(def-i-type swl #x2A
  (if (is-cache-isolated cpu)
    (invalidate-cache cpu)
    (let* ((aligned-address (logand address #xFFFFFFFC))
           (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
           (value target-register-value))
      (funcall (cpu-memory-set-word cpu)
               aligned-address
               (case (ldb (byte 2 0) address)
                 (0 (logior
                     (logand aligned-word #xFFFFFF00)
                     (wrap-word (ash value -24))))
                 (1 (logior
                     (logand aligned-word #xFFFF0000)
                     (wrap-word (ash value -16))))
                 (2 (logior
                     (logand aligned-word #xFF000000)
                     (wrap-word (ash value -8))))
                 (3 (logior
                     (logand aligned-word #x00000000)
                     (wrap-word (ash value 0))))
                 (otherwise (error "Unreachable.~%")))))))

(def-i-type sw #x2B
  (if (/= 0 (mod address 4))
    (trigger-exception cpu :cause :address-write-error)
    (if (is-cache-isolated cpu)
      (invalidate-cache cpu)
      (funcall (cpu-memory-set-word cpu)
               address
               target-register-value))))

(def-i-type swr #x2E
  (if (is-cache-isolated cpu)
    (invalidate-cache cpu)
    (let* ((aligned-address (logand address #xFFFFFFFC))
           (aligned-word (funcall (cpu-memory-get-word cpu) aligned-address))
           (value target-register-value))
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
                 (otherwise (error "Unreachable.~%")))))))

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

(def-i-type gte-nop #xC0200
  (log:debug "gte nop?~%"))

; TODO(Samantha): Consider making this io a bit more generic, it's
; frustrating to repeat myself.
(def-r-type mfc0 #xC0000
  (setf (cpu-pending-load-register cpu) target-register-index)
  (setf
   (cpu-pending-load-value cpu)
   (case destination-register-index
     (12 (cop0:cop0-status-register (cpu-cop0 cpu)))
     (13 (cop0:cop0-cause-register (cpu-cop0 cpu)))
     (14 (cop0:cop0-epc-register (cpu-cop0 cpu)))
     (otherwise
      (error "Unknown read to cop0$~d~%" destination-register-index)
      0))))

(def-r-type mtc0 #xC0004
  (case destination-register-index
    ; TODO(Samantha): Handle anything other than $cop0_12.
    ; BPC
    (3
     (unless (zerop target-register-value)
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              target-register-value
              destination-register-index)))
    ; BDA
    (5
     (unless (zerop target-register-value)
       (error "Tried to write 0x~8,'0x to $cop0_~d~%"
              target-register-value
              destination-register-index)))
    ; JUMPDEST
    (6
      (unless (zerop target-register-value)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               target-register-value
               destination-register-index)))
    ; DCIC
    (7
      (unless (zerop target-register-value)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               target-register-value
               destination-register-index)))
    ; BDAM
    (9
      (unless (zerop target-register-value)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               target-register-value
               destination-register-index)))
    ; BPCM
    (11
      (unless (zerop target-register-value)
        (error "Tried to write 0x~8,'0x to $cop0_~d~%"
               target-register-value
               destination-register-index)))
    (12
     (setf
      (cop0:cop0-status-register (cpu-cop0 cpu))
      target-register-value))
    ; CAUSE
    (13
     (unless (zerop target-register-value)
       (error "Are we actually storing to the cause?~%"))
      (setf
       (cop0:cop0-cause-register (cpu-cop0 cpu))
       target-register-value))
    (otherwise
     (error "Unknown write of 0x~8,'0X to $cop0_~d~%"
             target-register-value
             destination-register-index)
     0)))

(def-r-type mfhi #xFF10
  (set-register
   cpu destination-register-index
   (cpu-hi cpu)))

(def-r-type mthi #xFF11
  (setf (cpu-hi cpu) source-register-value))

(def-r-type mflo #xFF12
  (set-register
   cpu destination-register-index
   (cpu-lo cpu)))

(def-r-type mtlo #xFF13
  (setf (cpu-lo cpu) source-register-value))
