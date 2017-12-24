(defpackage #:psx-dma
  (:nicknames #:dma)
  (:use :cl :memory)
  (:export #:dma #:make-dma #:dma-control-register #:dma-interrupt-register
           #:dma-channels #:get-register #:set-register #:dma-write #:dma-read))

(in-package :psx-dma)
(declaim (optimize (speed 3) (safety 1)))

(defstruct channel-control
  ; TODO(Samantha): Preserve unknown bits at the end?
  (direction :to-ram :type keyword)
  (step 4 :type (signed-byte 4))
  (chopping nil :type boolean)
  (sync-mode :manual :type keyword)
  (dma-chop-size 0 :type (unsigned-byte 8))
  (cpu-chop-size 0 :type (unsigned-byte 8))
  (enable nil :type boolean)
  (trigger nil :type boolean))

; TODO(Samantha): Seriously, these two functions are the most error-prone code
; I've ever written. Find a way to make this safer. Maybe give it a spec
; that roughly matches the format from nocash?
(declaim (ftype (function ((unsigned-byte 32))
                          channel-control)
                word-to-channel-control))
(defun word-to-channel-control (word)
  (make-channel-control
   :direction (if (ldb-test (byte 1 0) word) :from-ram :to-ram)
   :step (if (ldb-test (byte 1 1) word) -4 4)
   ; Bits [7:2] are unused, according to nocash.
   :chopping (ldb-test (byte 1 8) word)
   :sync-mode
   (case (ldb (byte 2 9) word)
     (0 :manual)
     (1 :request)
     (2 :linked-list)
     (otherwise (error "Reserved DMA sync-mode!~%")))
   ; Bits [15:11] are unused.
   :dma-chop-size (expt 2 (ldb (byte 3 16) word))
   ; Bit 19 is unused.
   :cpu-chop-size (expt 2 (ldb (byte 3 20) word))
   ; Bit 23 is unused.
   :enable (ldb-test (byte 1 24) word)
   ; Bits [27:25] are unused.
   :trigger (ldb-test (byte 1 28) word))) ; Bits [31:29] are unused.

(declaim (ftype (function (channel-control)
                          (unsigned-byte 32))
                channel-control-to-word))
(defun channel-control-to-word (channel-control)
  (logior
   (if (eql (channel-control-direction channel-control) :from-ram) 1 0)
   (ash (if (= (channel-control-step channel-control) -4) 1 0) 1)
   (ash (if (channel-control-chopping channel-control) 1 0) 8)
   (ash (case (channel-control-sync-mode channel-control)
          (:manual 0)
          (:request 1)
          (:linked-list 2)
          (otherwise (error "Reserved DMA sync-mode!~%")))
        9)
   (ash (channel-control-dma-chop-size channel-control) 16)
   (ash (channel-control-cpu-chop-size channel-control) 20)
   (ash (if (channel-control-enable channel-control) 1 0) 24)
   (ash (if (channel-control-trigger channel-control) 1 0) 28)))

(defstruct channel
  (port :otc :type keyword)
  (base 0 :type (unsigned-byte 24))
  (block-size 0 :type (unsigned-byte 16))
  (block-count 0 :type (unsigned-byte 16))
  (channel-control (make-channel-control) :type channel-control))

(declaim (ftype (function (channel) boolean)
                active))
(defun active (channel)
  (and
   (channel-control-enable (channel-channel-control channel))
   (case (channel-control-sync-mode (channel-channel-control channel))
     (:manual (channel-control-trigger (channel-channel-control channel)))
     (otherwise t))))

(declaim (ftype (function (channel))
                complete))
(defun complete (channel)
  (setf (channel-control-enable (channel-channel-control channel)) nil)
  (setf (channel-control-trigger (channel-channel-control channel)) nil)
  (values))

(declaim (ftype (function (channel) (unsigned-byte 32))
                get-channel-block-control))
(defun get-channel-block-control (channel)
  (logior (channel-block-size channel) (ash (channel-block-count channel) 16)))

(declaim (ftype (function (channel) (unsigned-byte 32)) channel-block-control))
(defun set-channel-block-control (channel word)
  (setf (channel-block-size channel) (ldb (byte 16 0) word))
  (setf (channel-block-count channel) (ldb (byte 16 16) word))
  word)

(defstruct interrupt-register
  (unknown 0 :type (unsigned-byte 6))
  ; Bits [14:6] are unused.
  (force-irq nil :type boolean)
  (irq-channel-enable 0 :type (unsigned-byte 7))
  (irq-enable nil :type boolean)
  (irq-channel-flags 0 :type (unsigned-byte 7)))

(declaim (ftype (function (interrupt-register) boolean)
                interrupt-register-bit-31))
(defun interrupt-register-bit-31 (interrupt-register)
  (or
   (interrupt-register-force-irq interrupt-register)
   (and
    (interrupt-register-irq-enable interrupt-register)
    (ldb-test
     (byte 7 0)
     (logand
      (interrupt-register-irq-channel-flags interrupt-register)
      (interrupt-register-irq-channel-enable interrupt-register))))))

(declaim (ftype (function (interrupt-register) (unsigned-byte 32))
                interrupt-register-to-word))
(defun interrupt-register-to-word (interrupt-register)
  (logior
   (interrupt-register-unknown interrupt-register)
   (ash (if (interrupt-register-force-irq interrupt-register) 1 0) 15)
   (ash (interrupt-register-irq-channel-enable interrupt-register) 16)
   (ash (if (interrupt-register-irq-enable interrupt-register) 1 0) 23)
   (ash (interrupt-register-irq-channel-flags interrupt-register) 24)
   (ash (if (interrupt-register-bit-31 interrupt-register) 1 0) 31)))

(defun word-to-interrupt-register (interrupt-register word)
  (make-interrupt-register
   :unknown (ldb (byte 6 0) word)
   :force-irq (ldb-test (byte 1 15) word)
   :irq-channel-enable (ldb (byte 7 16) word)
   :irq-enable (ldb-test (byte 1 23) word)
   ; The flags reset if written to.
   :irq-channel-flags
   (ldb (byte 7 0)
        (logand
         (lognot (ldb (byte 7 24) word))
         (interrupt-register-irq-channel-flags interrupt-register)))))

(defstruct dma
  (write
   (lambda (address value) (declare (ignore address)) value)
   :type (function ((unsigned-byte 32) (unsigned-byte 32)) (unsigned-byte 32)))
  (read
   (lambda (address) (declare (ignore address)) 0)
   :type (function ((unsigned-byte 32)) (unsigned-byte 32)))
  (control-register #x07654321 :type (unsigned-byte 32))
  (interrupt-register (make-interrupt-register) :type interrupt-register)
  (channels
   (make-array 7
               :element-type 'channel
               ; It would be nice to use :initial-element, but the argument to
               ; it is only evaluated once.
               :initial-contents `(,(make-channel :port :mdec-in) ,(make-channel :port :mdec-out)
                                   ,(make-channel :port :gpu) ,(make-channel :port :cdrom)
                                   ,(make-channel :port :spu) ,(make-channel :port :pio)
                                   ,(make-channel :port :otc)))
   :type (simple-array channel (7))))

(declaim (ftype (function (dma (unsigned-byte 8) (unsigned-byte 32))
                          (unsigned-byte 32))
                set-register))
(defun set-register (dma offset value)
  ; Are we setting one of the channel registers?
  (if (< -1 offset #x70)
    ; Grab the top digit of offset, it happens to be our index into
    ; dma-channels while it's less than #x70.
    (let ((channel (aref (dma-channels dma) (ldb (byte 4 4) offset))))
      (case (ldb (byte 4 0) offset)
        (0 (setf (channel-base channel) (ldb (byte 24 0) value)))
        (4 (set-channel-block-control channel value) value)
        (8 (setf (channel-channel-control channel)
                 (word-to-channel-control value))
           value)
        (otherwise
         (error "Unhandle dma write of 0x~8,'0x at offset 0x~8,'0x~%"
                value offset)))
      (when (active channel)
        (run-dma dma channel))
      value)
    ; We're setting one of the general purpose registers
    (case offset
      (#x70 (setf (dma-control-register dma) value))
      (#x74 (setf
             (dma-interrupt-register dma)
             (word-to-interrupt-register (dma-interrupt-register dma) value))
        value)
      (otherwise
       (error "Unhandle dma write of 0x~8,'0x at offset 0x~8,'0x~%"
              value offset)))))

(declaim (ftype (function (dma (unsigned-byte 8)) (unsigned-byte 32))
                get-register))
(defun get-register (dma offset)
  (if (< -1 offset #x70)
    ; We're setting one of the channel registers
    ; Grab the top digit of offset, it happens to be our index into
    ; dma-channels while it's less than #x70.
    (let ((channel (aref (dma-channels dma) (ldb (byte 4 4) offset))))
      (case (ldb (byte 4 0) offset)
        (0 (channel-base channel))
        (4 (get-channel-block-control channel))
        (8 (channel-control-to-word (channel-channel-control channel)))
        (otherwise (error "Unhandle dma read at offset 0x~8,'0x~%" offset))))
    ; We're setting one of the general purpose registers
    (case offset
      (#x70 (dma-control-register dma))
      (#x74 (interrupt-register-to-word (dma-interrupt-register dma)))
      (otherwise (error "Unhandle dma read at offset 0x~8,'0x~%" offset)))))

(declaim (ftype (function (dma channel)) run-dma))
(defun run-dma (dma channel)
  (case (channel-control-sync-mode (channel-channel-control channel))
    (:manual (run-dma-block dma channel))
    (:request (run-dma-block dma channel))
    (:linked-list (run-dma-linked-list dma channel))
    (otherwise (error "Reserved DMA sync-mode!~%")))
  (values))

(declaim (ftype (function (channel) (unsigned-byte 32)) transfer-size))
(defun transfer-size (channel)
  (case (channel-control-sync-mode (channel-channel-control channel))
    (:manual (channel-block-size channel))
    (:request (* (channel-block-size channel) (channel-block-count channel)))
    (:linked-list 0)
    (otherwise (error "Reserved DMA sync-mode!~%"))))

(declaim (ftype (function (dma channel)) run-dma-block))
(defun run-dma-block (dma channel)
  (let ((remaining (transfer-size channel))
        (channel-control (channel-channel-control channel))
        (base (channel-base channel)))
    (loop for i from remaining downto 1
      do (case (channel-control-direction channel-control)
           (:from-ram
            (case (channel-port channel)
              (:gpu
               (funcall (dma-write dma)
                        +gpu-registers-begin+
                        (funcall (dma-read dma) (logand base #x1FFFFC))))
              (otherwise
               (error "Unhandled DMA channel ~A~%" (channel-port channel)))))
           (:to-ram
            (case (channel-port channel)
              (:otc
               (funcall (dma-write dma) (logand base #x1FFFFC)
                        (if (= i 1)
                          ; End of Table
                          #xFFFFFF
                          ; Previous link
                          (ldb (byte 21 0) (wrap-word (- base 4))))))
              (otherwise
               (error "Unhandled DMA channel ~A~%" (channel-port channel)))))
           (otherwise
            (error "Invalid DMA direction ~A~%"
                   (channel-control-direction channel-control))))
      do (setf base
               (wrap-word (+ base (channel-control-step channel-control))))))
  (complete channel))

(declaim (ftype (function (dma channel)) run-dma-linked-list))
(defun run-dma-linked-list (dma channel)
  (let ((channel-control (channel-channel-control channel))
        (base (logand #x1FFFFC (channel-base channel))))
    (unless (eql :gpu (channel-port channel))
      (error "Linked list dma not implemented for anything other than GPU!~%"))
    (when (eql :to-ram (channel-control-direction channel-control))
      (error "To ram dma linked list transfers are invalid!~%"))
    (loop do
      (let ((header (funcall (dma-read dma) base)))
        (loop for i from (ldb (byte 8 24) header) downto 1 do
          (progn
           (setf base (logand #x1FFFFC (+ 4 base)))
           (funcall (dma-write dma) +gpu-registers-begin+
                    (funcall (dma-read dma) base))))
        ; TODO(Samantha): Some sources say that the psx only reads the high bit
        ; to test for end of the list.
        (when (or (ldb-test (byte 32 0) (logand header #x800000)) nil)
          (loop-finish))
        (setf base (logand header #x1FFFFC)))))
  (complete channel))
