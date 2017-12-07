(defpackage #:psx-gpu
  (:nicknames #:gpu)
  (:use :cl)
  (:export #:gpu #:make-gpu #:gpu-gpu-stat #:gpu-stat-to-word
           #:read-gpu #:write-gpu))

(in-package :psx-gpu)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): Convert (unsigned-byte 1) to boolean when it makes sense.
(defstruct gpu-stat
  (texture-page-x-base 0 :type (unsigned-byte 4))
  (texture-page-y-base 0 :type (unsigned-byte 1))
  (semi-transparency 0 :type (unsigned-byte 2))
  (texture-page-colors 0 :type (unsigned-byte 2))
  (dither-24-to-15-bit 0 :type (unsigned-byte 1))
  (draw-to-display-area 0 :type (unsigned-byte 1))
  (set-mask-bit 0 :type (unsigned-byte 1))
  (draw-pixels 0 :type (unsigned-byte 1))
  (interlace-field 0 :type (unsigned-byte 1))
  ; According to nocash, this bit just causes strange effects on the
  ; display on real hardware; ignore?
  (reverse-flag 0 :type (unsigned-byte 1))
  (texture-disable 0 :type (unsigned-byte 1))
  (horizontal-resolution-2 0 :type (unsigned-byte 1))
  (horizontal-resolution-1 0 :type (unsigned-byte 2))
  (vertical-resolution 0 :type (unsigned-byte 1))
  (video-mode 0 :type (unsigned-byte 1))
  (display-area-color-depth 0 :type (unsigned-byte 1))
  (vertical-interlace 0 :type (unsigned-byte 1))
  (display-disabled 0 :type (unsigned-byte 1))
  (irq1 0 :type (unsigned-byte 1))
  (dma 0 :type (unsigned-byte 1))
  ; Set to avoid a hang in the bios
  (ready-to-receive-command 1 :type (unsigned-byte 1))
  (ready-to-send-vram-to-cpu 1 :type (unsigned-byte 1))
  (ready-to-receive-dma-block 1 :type (unsigned-byte 1))
  (dma-direction 0 :type (unsigned-byte 2))
  (even-odd-line 0 :type (unsigned-byte 1)))

(declaim (ftype (function (gpu-stat) (unsigned-byte 32)) gpu-stat-to-word))
(defun gpu-stat-to-word (gpu-stat)
  (logior
   (gpu-stat-texture-page-x-base gpu-stat)
   (ash (gpu-stat-texture-page-y-base gpu-stat) 4)
   (ash (gpu-stat-semi-transparency gpu-stat) 5)
   (ash (gpu-stat-texture-page-colors gpu-stat) 7)
   (ash (gpu-stat-dither-24-to-15-bit gpu-stat) 9)
   (ash (gpu-stat-draw-to-display-area gpu-stat) 10)
   (ash (gpu-stat-set-mask-bit gpu-stat) 11)
   (ash (gpu-stat-draw-pixels gpu-stat) 12)
   (ash (gpu-stat-interlace-field gpu-stat) 13)
   (ash (gpu-stat-reverse-flag gpu-stat) 14)
   (ash (gpu-stat-texture-disable gpu-stat) 15)
   (ash (gpu-stat-horizontal-resolution-2 gpu-stat) 16)
   (ash (gpu-stat-horizontal-resolution-1 gpu-stat) 17)
   (ash (gpu-stat-vertical-resolution gpu-stat) 19)
   (ash (gpu-stat-video-mode gpu-stat) 20)
   (ash (gpu-stat-display-area-color-depth gpu-stat) 21)
   (ash (gpu-stat-vertical-interlace gpu-stat) 22)
   (ash (gpu-stat-display-disabled gpu-stat) 23)
   (ash (gpu-stat-irq1 gpu-stat) 24)
   (ash (gpu-stat-dma gpu-stat) 25)
   (ash (gpu-stat-ready-to-receive-command gpu-stat) 26)
   (ash (gpu-stat-ready-to-send-vram-to-cpu gpu-stat) 27)
   (ash (gpu-stat-ready-to-receive-dma-block gpu-stat) 28)
   (ash (gpu-stat-dma-direction gpu-stat) 29)
   (ash (gpu-stat-even-odd-line gpu-stat) 31)))

; TODO(Samantha): word-to-gpu-stat function.

(defstruct gpu
  "A model psx gpu"
  (gpu-stat (make-gpu-stat) :type gpu-stat)
  (textured-rectangle-x-flip nil :type boolean)
  (textured-rectangle-y-flip nil :type boolean))

(declaim (ftype (function (gpu) (unsigned-byte 32)) read-gpu-read))
(defun read-gpu-read (gpu)
  (declare (ignore gpu))
  (format t "Reading from gpu-read is unimplemented! Returning 0.~%")
  0)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                draw-mode-settings))
(defun draw-mode-settings (gpu value)
  (let ((gpu-stat (gpu-gpu-stat gpu)))
    (setf (gpu-stat-texture-page-x-base gpu-stat) (ldb (byte 4 0) value))
    (setf (gpu-stat-texture-page-y-base gpu-stat) (ldb (byte 1 4) value))
    (setf (gpu-stat-semi-transparency gpu-stat) (ldb (byte 2 5) value))
    (setf (gpu-stat-texture-page-colors gpu-stat) (ldb (byte 2 7) value))
    (setf (gpu-stat-dither-24-to-15-bit gpu-stat) (ldb (byte 1 9) value))
    (setf (gpu-stat-draw-to-display-area gpu-stat) (ldb (byte 1 10) value))
    (setf (gpu-stat-texture-disable gpu-stat) (ldb (byte 1 11) value)))
  (setf (gpu-textured-rectangle-x-flip gpu) (ldb-test (byte 1 12) value))
  (setf (gpu-textured-rectangle-y-flip gpu) (ldb-test (byte 1 13) value)))

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                set-drawing-area-top-left))
(defun set-drawing-area-top-left (gpu value)
  0)

(declaim (ftype (function (gpu (unsigned-byte 32)) (unsigned-byte 32)) write-gp0))
(defun write-gp0 (gpu value)
  (format t "Wrote: 0x~8,'0x to GP0~%" value)
  (case (ldb (byte 8 24) value)
    (#xE1 (draw-mode-settings gpu value))
    (#xE3 (set-drawing-area-top-left gpu value))
    ; This is a noop?
    (#x00 0)
    (otherwise
     (error "Unrecognized GP0 opcode 0x~2,'0x. Full word: 0x~8,'0x"
            (ldb (byte 8 24) value)
            value)))
  0)

(declaim (ftype (function (gpu (unsigned-byte 32))
                          (unsigned-byte 32))
                write-gp1))
(defun write-gp1 (gpu value)
  (declare (ignore gpu))
  (format t "Wrote: 0x~8,'0x to GP1~%" value)
  0)

(declaim (ftype (function (gpu (unsigned-byte 4) (unsigned-byte 32))
                          (unsigned-byte 32))
                write-gpu))
(defun write-gpu (gpu offset value)
  (case offset
    (0 (write-gp0 gpu value))
    (4 (write-gp1 gpu value))
    (otherwise (error "Invalid GPU write offset 0x~8,'0x~%" offset))))

(declaim (ftype (function (gpu (unsigned-byte 4))
                          (unsigned-byte 32))
                read-gpu))
(defun read-gpu (gpu offset)
  (case offset
    (0 (read-gpu-read gpu))
    (4 (gpu-stat-to-word (gpu-gpu-stat gpu)))
    (otherwise (error "Invalid GPU read offset 0x~8,'0x~%" offset))))
