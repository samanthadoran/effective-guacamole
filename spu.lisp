(defpackage #:psx-spu
  (:nicknames #:spu)
  (:use :cl :memory)
  (:export #:spu #:make-spu #:read-spu-half-word #:write-spu-half-word))

(in-package :psx-spu)

(declaim (optimize (speed 3) (safety 1)))

(defstruct adsr
  "A representation of an spu voice's ADSR (attack decay sustain release)
   envelope."
  (sustain-level #x800 :type (integer #x800 #x8000))
  (decay-step -8  :type (signed-byte 5) :read-only t)
  (decay-shift 0 :type (unsigned-byte 4))
  (decay-direction :decrease :type keyword :read-only t)
  (decay-mode :exponential :type keyword :read-only t)
  (attack-step 7 :type (integer 4 7))
  (attack-shift 0 :type (unsigned-byte 5))
  (attack-direction :increase :type keyword :read-only t)
  (attack-mode :linear :type keyword)
  (release-step -8  :type (signed-byte 5) :read-only t)
  (release-shift 0 :type (unsigned-byte 5))
  (release-direction :decrease :type keyword :read-only t)
  (release-mode :linear :type keyword)
  ; TODO(Samantha): The sustain values here talk about increasing or decreasing.
  ; I don't understand, at all.
  ; This should be interpreted as negative depending on sustain direction
  (sustain-step 7 :type (integer 4 7))
  (sustain-shift 0 :type (unsigned-byte 5))
  (sustain-direction :increase :type keyword)
  (sustain-mode :linear :type keyword))

(declaim (ftype (function (adsr) (unsigned-byte 32)) adsr-to-word))
(defun adsr-to-word (adsr)
  (logior
   (ash (ldb (byte 4 0) (1- (floor (adsr-sustain-level adsr) #x800))) 0)
   (ash (adsr-decay-shift adsr) 4)
   (ash (ldb (byte 2 0) (- (adsr-attack-step adsr) 4)) 8)
   (ash (adsr-attack-shift adsr) 10)
   (ash (if (eql (adsr-attack-mode adsr) :linear) 0 1) 15)
   (ash (adsr-release-shift adsr) 16)
   (ash (if (eql (adsr-release-mode adsr) :linear) 0 1) 21)
   (ash (ldb (byte 2 0) (- (adsr-sustain-step adsr) 4)) 22)
   (ash (adsr-sustain-shift adsr) 24)
   (ash 0 29)
   (ash (if (eql (adsr-sustain-direction adsr) :increase) 0 1) 30)
   (ash (if (eql (adsr-sustain-mode adsr) :linear) 0 1) 31)))

(declaim (ftype (function ((unsigned-byte 32)) adsr) word-to-adsr))
(defun word-to-adsr (word)
  (make-adsr
   :sustain-level (* #x800 (1+ (ldb (byte 4 0) word)))
   :decay-shift (ldb (byte 4 4) word)
   :attack-step (+ 4 (ldb (byte 2 8) word))
   :attack-shift (ldb (byte 5 10) word)
   :attack-mode (if (ldb-test (byte 1 15) word) :exponential :linear)
   :release-shift (ldb (byte 5 16) word)
   :release-mode (if (ldb-test (byte 1 21) word) :exponential :linear)
   :sustain-step (+ 4 (ldb (byte 2 22) word))
   :sustain-shift (ldb (byte 5 24) word)
   :sustain-direction (if (ldb-test (byte 1 30) word) :decrease :increase)
   :sustain-mode (if (ldb-test (byte 1 31) word) :exponential :linear)))

(defstruct voice
  (volume-left 0 :type (unsigned-byte 16))
  (volume-right 0 :type (unsigned-byte 16))
  (adpcm-sample-rate 0 :type (unsigned-byte 16))
  (adpcm-start-address 0 :type (unsigned-byte 16))
  (adsr (make-adsr) :type adsr)
  (adsr-volume 0 :type (unsigned-byte 16))
  (adpcm-repeat-address 0 :type (unsigned-byte 16)))

(defstruct spu-status
  (current-spu-mode 0 :type (unsigned-byte 6))
  (irq9 nil :type boolean)
  (data-transfer-dma-read-write-request nil :type boolean)
  (data-transfer-dma-write-request nil :type boolean)
  (data-transfer-dma-read-request nil :type boolean)
  (data-transfer-busy nil :type boolean)
  (writing-to-capture-buffer :first :type keyword))

(declaim (ftype (function (spu-status)
                          (unsigned-byte 16))
                spu-status-to-word))
(defun spu-status-to-word (spu-status)
  (logior
   (ash (spu-status-current-spu-mode spu-status) 0)
   (ash (if (spu-status-irq9 spu-status) 1 0) 6)
   (ash (if (spu-status-data-transfer-dma-read-write-request spu-status) 1 0) 7)
   (ash (if (spu-status-data-transfer-dma-write-request spu-status) 1 0) 8)
   (ash (if (spu-status-data-transfer-dma-read-request spu-status) 1 0) 9)
   (ash (if (spu-status-data-transfer-busy spu-status) 1 0) 10)
   (ash (ecase (spu-status-writing-to-capture-buffer spu-status)
               (:first 0)
               (:second 1))
        11)))

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
  (status (make-spu-status) :type spu-status)
  (cd-volume-left 0 :type (unsigned-byte 16))
  (cd-volume-right 0 :type (unsigned-byte 16))
  (external-volume-left 0 :type (unsigned-byte 16))
  (external-volume-right 0 :type (unsigned-byte 16))
  (current-main-volume-left 0 :type (unsigned-byte 16))
  (current-main-volume-right 0 :type (unsigned-byte 16))
  (transfer-address 0 :type (integer 0 #x80000))
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
         (#x8 (ldb (byte 16 0) (adsr-to-word (voice-adsr voice))))
         (#xA (ldb (byte 16 16) (adsr-to-word (voice-adsr voice))))
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
         (log:debug "Spu register at location 0x~8,'0x ~
                    has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+))
         0)
       (#x1A2 (spu-sound-ram-reverb-work-area-start-address spu))
       (#x1A4 (spu-sound-ram-irq-address spu))
       (#x1A6 (spu-sound-ram-data-transfer-address spu))
       (#x1A8 (spu-sound-ram-data-transfer-fifo spu))
       (#x1AA (spu-control spu))
       (#x1AC (spu-sound-ram-data-transfer-control spu))
       (#x1AE (spu-status-to-word (spu-status spu)))
       (#x1B0 (spu-cd-volume-left spu))
       (#x1B2 (spu-cd-volume-right spu))
       (#x1B4 (spu-external-volume-left spu))
       (#x1B6 (spu-external-volume-right spu))
       (#x1B8 (spu-current-main-volume-left spu))
       (#x1BA (spu-current-main-volume-right spu))
       (#x1BC
         (log:debug "Spu register at location 0x~8,'0x ~
                    has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+))
         0)
       (#x1BE
         (log:debug "Spu register at location 0x~8,'0x ~
                    has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+))
         0)
       (otherwise
        (error "Unrecognized spu address 0x~8,'0x!~%"
               (+ offset +spu-registers-begin+)))))
    (t
      (log:debug "Internal registers and reverb configuration ~
                 registers are unimplemented!~%")
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
         (#x8
           (setf (voice-adsr voice)
                 (word-to-adsr
                  (logior value
                          (logand #xFFFF0000
                                  (adsr-to-word (voice-adsr voice)))))))
         (#xA
           (setf (voice-adsr voice)
                 (word-to-adsr
                  (logior (ash value 16)
                          (logand #x0000FFFF
                                  (adsr-to-word (voice-adsr voice)))))))
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
         (log:debug "Spu register at location 0x~8,'0x ~
                     has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+)))
       (#x1A2 (setf (spu-sound-ram-reverb-work-area-start-address spu) value))
       (#x1A4 (setf (spu-sound-ram-irq-address spu) value))
       ; TODO(Samantha): This value is used as a base address for any transfers.
       (#x1A6 (progn
               (setf (spu-transfer-address spu) (mod (* value 8) #x80000))
               (setf (spu-sound-ram-data-transfer-address spu) value)))
       (#x1A8 (progn
               ; TODO(Samantha): Actually make a fifo.
               (setf (aref (spu-ram spu) (spu-transfer-address spu))
                     (ldb (byte 8 0) value))
               (setf  (spu-transfer-address spu)
                      (mod (1+ (spu-transfer-address spu)) #x80000))
               (setf (aref (spu-ram spu) (spu-transfer-address spu))
                     (ldb (byte 8 8) value))
               (setf  (spu-transfer-address spu)
                      (mod (1+ (spu-transfer-address spu)) #x80000))
               (setf (spu-sound-ram-data-transfer-fifo spu) value)))
       (#x1AA (setf (spu-control spu) value))
       (#x1AC (setf (spu-sound-ram-data-transfer-control spu) value))
       (#x1AE
         (progn
          (log:debug "Attempting to write 0x~8,'0x to spu-status ~
                      which is read only.~%"
                      value)
          value))
       (#x1B0 (setf (spu-cd-volume-left spu) value))
       (#x1B2 (setf (spu-cd-volume-right spu) value))
       (#x1B4 (setf (spu-external-volume-left spu) value))
       (#x1B6 (setf (spu-external-volume-right spu) value))
       (#x1B8 (setf (spu-current-main-volume-left spu) value))
       (#x1BA (setf (spu-current-main-volume-right spu) value))
       (#x1BC
         (log:debug "Spu register at location 0x~8,'0x ~
                    has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+)))
       (#x1BE
         (log:debug "Spu register at location 0x~8,'0x ~
                    has an unknown purpose and is unimplemented!~%"
                    (+ offset +spu-registers-begin+)))
       (otherwise
        (error "Unrecognized spu address 0x~8,'0x!~%"
               (+ offset +spu-registers-begin+)))))
    (t
      (log:debug "Internal registers and reverb configuration ~
                 registers are unimplemented!~%")))
  value)
