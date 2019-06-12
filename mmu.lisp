(in-package :psx-console)

(declaim (optimize (speed 3) (safety 1)))

(declaim (ftype (function (psx (unsigned-byte 32))
                          (unsigned-byte 8))
                load-byte*))
(defun load-byte* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (let ((address (mask-address address)))
    (cond
      ((in-range +joypad-registers-begin+ +joypad-registers-size+ address)
       (ldb (byte 8 0) (psx-joypads:read-joypads
                        (psx-joypads psx)
                        (mod address +joypad-registers-begin+))))
      ((in-range +cdrom-registers-begin+ +cdrom-registers-size+ address)
       (psx-cdrom:read-cdrom-registers (psx-cdrom psx)
                                       (mod address +cdrom-registers-begin+)))
      ((in-range +bios-begin-address+
                 (array-dimension (psx-bios-rom psx) 0)
                 address)
       (aref (psx-bios-rom psx) (- address +bios-begin-address+)))
      ((in-range +ram-begin+ +ram-size+ address)
       (aref (psx-ram psx) (mod address +ram-size-non-mirrored+)))
      ((in-range +expansion-1-begin+
                 +expansion-1-size+
                 address)
       ; We don't care about the expansion port just yet, return a dummy value.
       #xFF)
      ; Unimplemented.
      (t (error "Byte reads to 0x~8,'0X are unimplemented~%" address)))))

(declaim (ftype (function (psx (unsigned-byte 32))
                          (unsigned-byte 16))
                load-half-word*))
(defun load-half-word* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (let ((address (mask-address address)))
    (cond
      ((in-range +joypad-registers-begin+ +joypad-registers-size+ address)
       (ldb (byte 16 0) (psx-joypads:read-joypads
                         (psx-joypads psx)
                         (mod address +joypad-registers-begin+))))
      ((in-range +spu-registers-begin+ +spu-registers-size+ address)
       (psx-spu:read-spu-half-word (psx-spu psx)
                                   (mod address +spu-registers-begin+)))
      ((in-range +ram-begin+ +ram-size+ address)
       (read-half-word-from-byte-array (psx-ram psx)
                                       (mod address +ram-size-non-mirrored+)))
      ((in-range +irq-registers-begin+ +irq-registers-size+ address)
       (psx-irq:read-irq (psx-irq psx) (mod address +irq-registers-begin+)))
      ((in-range +timers-begin+ +timers-size+ address)
       (psx-timers:read-timers (psx-timers psx) (mod address +timers-begin+)))
      ; Unimplemented.
      (t (error "Half-word reads to 0x~8,'0X are unimplemented~%" address)))))

(declaim (ftype (function (psx (unsigned-byte 32))
                          (unsigned-byte 32))
                load-word*))
(defun load-word* (psx address)
  ; TODO(Samantha): Implement more places, simplify the cond.
  (let ((address (mask-address address)))
    (cond
      ; BIOS
      ((in-range +bios-begin-address+
                 (array-dimension (psx-bios-rom psx) 0)
                 address)
       (read-word-from-byte-array
        (psx-bios-rom psx) (- address +bios-begin-address+)))
      ; RAM
      ((in-range +ram-begin+ +ram-size+ address)
       (read-word-from-byte-array (psx-ram psx)
                                  (mod address +ram-size-non-mirrored+)))
      ((in-range +irq-registers-begin+ +irq-registers-size+ address)
       (psx-irq:read-irq (psx-irq psx) (mod address +irq-registers-begin+)))
      ((in-range +timers-begin+ +timers-size+ address)
       (psx-timers:read-timers (psx-timers psx) (mod address +timers-begin+)))
      ((in-range +dma-registers-begin+ +dma-registers-size+ address)
       (psx-dma:get-register (psx-dma psx) (mod address +dma-registers-begin+)))
      ((in-range +gpu-registers-begin+ +gpu-registers-size+ address)
       (psx-gpu:sync (psx-gpu psx) (psx-clock psx))
       (psx-gpu:read-gpu (psx-gpu psx) (mod address +gpu-registers-begin+)))
      ; Unimplemented.
      (t (error "Word reads to 0x~8,'0X are unimplemented~%"
                address)))))

; TODO(Samantha): Figure out a way to fix this shadowing.
(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 8))
                  (unsigned-byte 8))
        write-byte*))
(defun write-byte* (psx address value)
  (let ((address (mask-address address)))
    (cond
      ((in-range +joypad-registers-begin+ +joypad-registers-size+ address)
       (ldb (byte 8 0) (psx-joypads:write-joypads
                        (psx-joypads psx)
                        (mod address +joypad-registers-begin+)
                        value)))
      ((in-range +cdrom-registers-begin+ +cdrom-registers-size+ address)
       (psx-cdrom:write-cdrom-registers (psx-cdrom psx)
                                        (mod address +cdrom-registers-begin+)
                                        value))
      ((in-range +expansion-2-begin+ +expansion-2-size+ address)
       (log:debug "Wrote 0x~2,'0x to expansion2 @ 0x~8,'0x!~%" value address)
       value)
      ((in-range +ram-begin+ +ram-size+ address)
       (setf
        (aref (psx-ram psx) (mod address +ram-size-non-mirrored+))
        value))
      ; Unimplemented.
      (t (error "Byte writes to 0x~8,'0X are unimplemented!~%" address)))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 16))
                  (unsigned-byte 16))
        write-half-word*))
(defun write-half-word* (psx address value)
  (let ((address (mask-address address)))
    (cond
      ((in-range +joypad-registers-begin+ +joypad-registers-size+ address)
       (psx-joypads:write-joypads (psx-joypads psx)
                                  (mod address +joypad-registers-begin+)
                                  value))
      ((in-range +spu-registers-begin+ +spu-registers-size+ address)
       (psx-spu:write-spu-half-word (psx-spu psx)
                                    (mod address +spu-registers-begin+)
                                    value))
      ((in-range +timers-begin+ +timers-size+ address)
       (psx-timers:write-timers (psx-timers psx)
                                (mod address +timers-begin+)
                                value))
      ((in-range +ram-begin+ +ram-size+ address)
       (write-half-word-to-byte-array (psx-ram psx)
                                      (mod address +ram-size-non-mirrored+)
                                      value))
      ((in-range +irq-registers-begin+ +irq-registers-size+ address)
       (psx-irq:write-irq (psx-irq psx)
                          (mod address +irq-registers-begin+)
                          value))
      ; Unimplemented.
      (t (error "Half-word writes to 0x~8,'0X are unimplemented!~%" address)))))

(declaim
 (ftype (function (psx (unsigned-byte 32) (unsigned-byte 32))
                  (unsigned-byte 32))
        write-word*))
(defun write-word* (psx address value)
  (let ((address (mask-address address)))
    (cond
      ((in-range +memory-control-begin+ +memory-control-size+ address)
       (cond
         ; Expansion base 1 register
         ((= address +memory-control-begin+)
          (when (/= value +expansion-1-begin+)
            (error "Unexpected value for expansion-1-begin, got 0x~8,'0x, ~
                    expected 0x~8,'0x!~%"
                   value +expansion-1-begin+))
          value)
         ((= address (+ +memory-control-begin+ 4))
          (when (/= value +expansion-2-begin+)
            (error "Unexpected value for expansion-2-begin, got 0x~8,'0x, ~
                    expected 0x~8,'0x!~%"
                   value +expansion-2-begin+))
          value)
         ((= address (+ +memory-control-begin+ 8))
          (log:debug "Wrote 0x~8,'0x to expansion 1 delay/size!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #xC))
          (log:debug "Wrote 0x~8,'0x to expansion 3 delay/size!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #x10))
          (log:debug "Wrote 0x~8,'0x to bios delay/size!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #x14))
          (log:debug "Wrote 0x~8,'0x to spu_delay!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #x18))
          (log:debug "Wrote 0x~8,'0x to cdrom_delay!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #x1C))
          (log:debug "Wrote 0x~8,'0x to expansion 2 delay/size!~%" value)
          value)
         ((= address (+ +memory-control-begin+ #x20))
          (log:debug "Wrote 0x~8,'0x to common delay!~%" value)
          value)
         (t
          (error "Unexpected write of 0x~8,'0x! to Memory Control ~
                  at 0x~8,'0x!~%"
                 value address))))
      ((in-range +irq-registers-begin+ +irq-registers-size+ address)
       (psx-irq:write-irq (psx-irq psx)
                          (mod address +irq-registers-begin+)
                          value))
      ((= address +ram-size-begin+)
       (log:debug "Wrote 0x~8,'0x to ram size!~%" value)
       value)
      ((= address +cache-control+)
       (psx-cache-control:write-cache-control
        (psx-cpu:cpu-cache-control (psx-cpu psx))
        value)
       value)
      ((in-range +gpu-registers-begin+ +gpu-registers-size+ address)
       (psx-gpu:sync (psx-gpu psx) (psx-clock psx))
       (psx-gpu::write-gpu (psx-gpu psx)
                           (mod address +gpu-registers-begin+)
                           value))
      ((in-range +timers-begin+ +timers-size+ address)
       (psx-timers:write-timers (psx-timers psx)
                                (mod address +timers-begin+)
                                value))
      ; RAM
      ((in-range +ram-begin+ +ram-size+ address)
       (write-word-to-byte-array (psx-ram psx)
                                 (mod address +ram-size-non-mirrored+)
                                 value))
      ((in-range +dma-registers-begin+ +dma-registers-size+ address)
       (psx-dma:set-register (psx-dma psx)
                             (mod address +dma-registers-begin+)
                             value))
      ; Unimplemented.
      (t (error "Word writes to 0x~8,'0X are unimplemented!~%" address)))))

; TODO(Samantha): This function is getting a bit out of hand, maybe a rename to
; something like map-callbacks is in order?
(declaim (ftype (function (psx) function) map-memory))
(defun map-memory (psx)
  "Sets functions for easy reading and writing throughout the system."
  (setf
   (psx-irq:irq-exception-callback (psx-irq psx))
   (lambda ()
           (when (ldb-test
                  (byte 1 0)
                  (cop0:cop0-status-register (psx-cpu::cpu-cop0 (psx-cpu psx))))
             (psx-cpu:trigger-exception (psx-cpu psx) :cause :interrupt))
           0))
  (setf
   (psx-cdrom:cdrom-exception-callback (psx-cdrom psx))
   (lambda ()
           (psx-irq::raise-interrupt (psx-irq psx) :cdrom)
           0))
  (setf
   (psx-gpu:gpu-sync-callback (psx-gpu psx))
   (lambda (event clock) (register-sync-event psx event clock)))
  (setf
   (psx-gpu:gpu-exception-callback (psx-gpu psx))
   (lambda ()
           (psx-irq::raise-interrupt (psx-irq psx) :vblank)
           0))
  (setf (psx-gpu::gpu-render-callback (psx-gpu psx))
        (lambda () (psx-renderer:draw (psx-gpu psx))))
  (setf
   (psx-joypads:joypads-exception-callback (psx-joypads psx))
   (lambda ()
           (psx-irq::raise-interrupt (psx-irq psx) :joypad)
           0))
  ; TODO(Samantha): Why can't sbcl figure the type of these
  ; arrays without hints?
  (setf
   (psx-joypads:controller-buttons-callback (aref (the (simple-array psx-joypads::controller)(psx-joypads:joypads-controllers (psx-joypads psx))) 0))
   (lambda ()
           (psx-input:controller-callback 0)))
  (setf
   (psx-joypads:controller-buttons-callback (aref (the (simple-array psx-joypads::controller)(psx-joypads:joypads-controllers (psx-joypads psx))) 1))
   (lambda ()
           (psx-input:controller-callback 1)))
  (setf
   (psx-timers:timers-exception-callback (psx-timers psx))
   (lambda (keyword)
           (psx-irq::raise-interrupt (psx-irq psx) keyword)
           0))
  (setf
   (psx-dma:dma-tick (psx-dma psx))
   (lambda (ticks) (psx-cpu:tick (psx-cpu psx) ticks)))
  (setf
   (psx-dma:dma-read (psx-dma psx))
   (lambda (address) (load-word* psx address)))
  (setf
   (psx-dma:dma-write (psx-dma psx))
   (lambda (address value) (write-word* psx address value)))
  (setf
   (psx-cpu:cpu-memory-get-byte (psx-cpu psx))
   (lambda (address) (load-byte* psx address)))
  (setf
   (psx-cpu:cpu-memory-get-half-word (psx-cpu psx))
   (lambda (address) (load-half-word* psx address)))
  (setf
   (psx-cpu:cpu-memory-get-word (psx-cpu psx))
   (lambda (address) (load-word* psx address)))
  (setf
   (psx-cpu:cpu-memory-set-byte (psx-cpu psx))
   (lambda (address value) (write-byte* psx address value)))
  (setf
   (psx-cpu:cpu-memory-set-half-word (psx-cpu psx))
   (lambda (address value) (write-half-word* psx address value)))
  (setf
   (psx-cpu:cpu-memory-set-word (psx-cpu psx))
   (lambda (address value) (write-word* psx address value)))
  (setf
   (psx-cpu:cpu-has-pending-irq (psx-cpu psx))
   (lambda () (psx-irq:has-pending-irq (psx-irq psx)))))
