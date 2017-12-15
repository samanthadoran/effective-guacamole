(defpackage #:psx-spu
  (:nicknames #:spu)
  (:use :cl :memory)
  (:export #:spu #:make-spu #:read-spu-half-word #:write-spu-half-word))

(in-package :psx-spu)

(declaim (optimize (speed 3) (safety 1)))

(defparameter *debug-spu* nil)

(defstruct voice
  (volume-left 0 :type (unsigned-byte 16))
  (volume-right 0 :type (unsigned-byte 16))
  (adpcm-sample-rate 0 :type (unsigned-byte 16))
  (adpcm-start-address 0 :type (unsigned-byte 16))
  (adsr 0 :type (unsigned-byte 32))
  (adsr-volume 0 :type (unsigned-byte 16))
  (adpcm-repeat-address 0 :type (unsigned-byte 16)))

(defstruct spu
  (main-volume-left 0 :type (unsigned-byte 16))
  (main-volume-right 0 :type (unsigned-byte 16))
  (reverb-output-volume-left 0 :type (unsigned-byte 16))
  (reverb-output-volume-right 0 :type (unsigned-byte 16))
  ; These two registers are write only.
  (key-on 0 :type (unsigned-byte 24))
  (key-off 0 :type (unsigned-byte 24))
  (pitch-modulation-enable 0 :type (unsigned-byte 24))
  (noise-mode 0 :type (unsigned-byte 24))
  (reverb-mode 0 :type (unsigned-byte 24))
  ; Channel status is read only.
  (channel-status 0 :type (unsigned-byte 24))
  (sound-ram-reverb-work-area-start-address 0 :type (unsigned-byte 16))
  (sound-ram-irq-address 0 :type (unsigned-byte 16))
  (sound-ram-data-transfer-address 0 :type (unsigned-byte 16))
  (sound-ram-data-transfer-fifo 0 :type (unsigned-byte 16))
  (control 0 :type (unsigned-byte 16))
  (sound-ram-data-transfer-control 0 :type (unsigned-byte 16))
  ; Spu status is read only.
  (status 0 :type (unsigned-byte 16))
  (cd-volume-left 0 :type (unsigned-byte 16))
  (cd-volume-right 0 :type (unsigned-byte 16))
  (external-volume-left 0 :type (unsigned-byte 16))
  (external-volume-right 0 :type (unsigned-byte 16))
  (current-main-volume-left 0 :type (unsigned-byte 16))
  (current-main-volume-right 0 :type (unsigned-byte 16))
  (ram (make-array #x80000 :element-type '(unsigned-byte 8) :initial-element 0)
       :type (simple-array (unsigned-byte 8) (#x80000)))
  (voices
    (make-array 24
                :element-type 'voice
                ; It would be nice to use :initial-element, but the argument to
                ; it is only evaluated once.
                :initial-contents `(,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)
                                    ,(make-voice) ,(make-voice) ,(make-voice)))
    :type (simple-array voice (24))))

; TODO(Samantha): Some of these registers are read or write only, but can still
; accept reads or writes and exhibit special behavior. Implement.
(declaim (ftype (function (spu (unsigned-byte 32))
                          (unsigned-byte 16))
                read-spu-half-word))
(defun read-spu-half-word (spu offset)
  (cond
    ((in-range
      (- +spu-voice-registers-begin+ +spu-registers-begin+)
      +spu-voice-registers-size+ offset)
     (let ((voice (aref (spu-voices spu) (ldb (byte 8 4) offset))))
       (case (ldb (byte 4 0) offset)
         (#x0 (voice-volume-left voice))
         (#x2 (voice-volume-right voice))
         (#x4 (voice-adpcm-sample-rate voice))
         (#x6 (voice-adpcm-start-address voice))
         ; #x8 and #xA are two different pieces of the adsr register.
         (#x8 (ldb (byte 16 0) (voice-adsr voice)))
         (#xA (ldb (byte 16 16) (voice-adsr voice)))
         (#xC (voice-adsr-volume voice))
         (#xE (voice-adpcm-repeat-address voice))
         (otherwise
          (error "Invalid voice register: 0x~2,'0x" (ldb (byte 4 0) offset))))))
    ((in-range
      (- +spu-control-registers-begin+ +spu-registers-begin+)
      +spu-control-registers-size+ offset)
     (case offset
       (#x180 (spu-main-volume-left spu))
       (#x182 (spu-main-volume-right spu))
       (#x184 (spu-reverb-output-volume-left spu))
       (#x186 (spu-reverb-output-volume-right spu))
       (#x188 (ldb (byte 16 0) (spu-key-on spu)))
       (#x18A (ldb (byte 8 16) (spu-key-on spu)))
       (#x18C (ldb (byte 16 0) (spu-key-off spu)))
       (#x18E (ldb (byte 8 16) (spu-key-off spu)))
       (#x190 (ldb (byte 16 0) (spu-pitch-modulation-enable spu)))
       (#x192 (ldb (byte 8 16) (spu-pitch-modulation-enable spu)))
       (#x194 (ldb (byte 16 0) (spu-noise-mode spu)))
       (#x196 (ldb (byte 8 16) (spu-noise-mode spu)))
       (#x198 (ldb (byte 16 0) (spu-reverb-mode spu)))
       (#x19A (ldb (byte 8 16) (spu-reverb-mode spu)))
       (#x19C (ldb (byte 16 0) (spu-channel-status spu)))
       (#x19E (ldb (byte 8 16) (spu-channel-status spu)))
       (#x1A0
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (#x1A2 (spu-sound-ram-reverb-work-area-start-address spu))
       (#x1A4 (spu-sound-ram-irq-address spu))
       (#x1A6 (spu-sound-ram-data-transfer-address spu))
       (#x1A8 (spu-sound-ram-data-transfer-fifo spu))
       (#x1AA (spu-control spu))
       (#x1AC (spu-sound-ram-data-transfer-control spu))
       (#x1AE (spu-status spu))
       (#x1B0 (spu-cd-volume-left spu))
       (#x1B2 (spu-cd-volume-right spu))
       (#x1B4 (spu-external-volume-left spu))
       (#x1B6 (spu-external-volume-right spu))
       (#x1B8 (spu-current-main-volume-left spu))
       (#x1BA (spu-current-main-volume-right spu))
       (#x1BC
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (#x1BE
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (otherwise
        (error "Unrecognized spu address 0x~8,'0x!~%"
               (+ offset +spu-registers-begin+)))))
    (t
     (when *debug-spu*
       (format t "Internal registers and reverb configuration ~
                  registers are unimplemented!~%"))
     0)))

(declaim (ftype (function (spu (unsigned-byte 32) (unsigned-byte 16))
                          (unsigned-byte 16))
                write-spu-half-word))
(defun write-spu-half-word (spu offset value)
  (cond
    ((in-range
      (- +spu-voice-registers-begin+ +spu-registers-begin+)
      +spu-voice-registers-size+ offset)
     (let ((voice (aref (spu-voices spu) (ldb (byte 8 4) offset))))
       (case (ldb (byte 4 0) offset)
         (#x0 (setf (voice-volume-left voice) value))
         (#x2 (setf (voice-volume-right voice) value))
         (#x4 (setf (voice-adpcm-sample-rate voice) value))
         (#x6 (setf (voice-adpcm-start-address voice) value))
         ; #x8 and #xA are two different pieces of the adsr register.
         (#x8 (setf (ldb (byte 16 0) (voice-adsr voice)) value))
         (#xA (setf (ldb (byte 16 16) (voice-adsr voice)) value))
         (#xC (setf (voice-adsr-volume voice) value))
         (#xE (setf (voice-adpcm-repeat-address voice) value))
         (otherwise
          (error "Invalid voice register: 0x~2,'0x" (ldb (byte 4 0) offset))))))
    ((in-range
      (- +spu-control-registers-begin+ +spu-registers-begin+)
      +spu-control-registers-size+ offset)
     (case offset
       (#x180 (setf (spu-main-volume-left spu) value))
       (#x182 (setf (spu-main-volume-right spu) value))
       (#x184 (setf (spu-reverb-output-volume-left spu) value))
       (#x186 (setf (spu-reverb-output-volume-right spu) value))
       ; Supposedly, only bits [0, 23] are used of the following six registers,
       ; but bios likes to write to all of them, anyway. Ignore the top 8 bits
       ; so we don't overflow the slot.
       (#x188 (setf (ldb (byte 16 0) (spu-key-on spu)) value))
       (#x18A (setf (ldb (byte 8 16) (spu-key-on spu)) value))
       (#x18C (setf (ldb (byte 16 0) (spu-key-off spu)) value))
       (#x18E (setf (ldb (byte 8 16) (spu-key-off spu)) value))
       (#x190 (setf (ldb (byte 16 0) (spu-pitch-modulation-enable spu)) value))
       (#x192 (setf (ldb (byte 8 16) (spu-pitch-modulation-enable spu)) value))
       (#x194 (setf (ldb (byte 16 0) (spu-noise-mode spu)) value))
       (#x196 (setf (ldb (byte 8 16) (spu-noise-mode spu)) value))
       (#x198 (setf (ldb (byte 16 0) (spu-reverb-mode spu)) value))
       (#x19A (setf (ldb (byte 8 16) (spu-reverb-mode spu)) value))
       (#x19C (setf (ldb (byte 16 0) (spu-channel-status spu)) value))
       (#x19E (setf (ldb (byte 8 16) (spu-channel-status spu)) value))
       (#x1A0
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (#x1A2 (setf (spu-sound-ram-reverb-work-area-start-address spu) value))
       (#x1A4 (setf (spu-sound-ram-irq-address spu) value))
       (#x1A6 (setf (spu-sound-ram-data-transfer-address spu) value))
       (#x1A8 (setf (spu-sound-ram-data-transfer-fifo spu) value))
       (#x1AA (setf (spu-control spu) value))
       (#x1AC (setf (spu-sound-ram-data-transfer-control spu) value))
       (#x1AE (setf (spu-status spu) value))
       (#x1B0 (setf (spu-cd-volume-left spu) value))
       (#x1B2 (setf (spu-cd-volume-right spu) value))
       (#x1B4 (setf (spu-external-volume-left spu) value))
       (#x1B6 (setf (spu-external-volume-right spu) value))
       (#x1B8 (setf (spu-current-main-volume-left spu) value))
       (#x1BA (setf (spu-current-main-volume-right spu) value))
       (#x1BC
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (#x1BE
         (when *debug-spu*
           (format t "Spu register at location 0x~8,'0x ~
                      has an unknown purpose and is unimplemented!~%"
                   (+ offset +spu-registers-begin+)))
         0)
       (otherwise
        (error "Unrecognized spu address 0x~8,'0x!~%"
               (+ offset +spu-registers-begin+)))))
    (t
     (when *debug-spu*
       (format t "Internal registers and reverb configuration ~
                  registers are unimplemented!~%"))
     0)))
