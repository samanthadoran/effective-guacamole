(defpackage #:psx-console
  (:nicknames #:psx)
  (:use :cl :psx-cpu :memory)
  (:export #:make-psx #:load-rom-from-file #:console-on #:make-console
           #:setup-and-run))

(in-package :psx-console)
(declaim (optimize (speed 3) (safety 1)))

(defstruct psx
  "A model psx"
  (scheduler (psx-scheduler:make-scheduler) :type psx-scheduler:scheduler)
  (cpu (make-cpu) :type cpu)
  (irq (psx-irq:make-irq) :type psx-irq:irq)
  (timers (psx-timers:make-timers) :type psx-timers:timers)
  (cdrom (psx-cdrom:make-cdrom) :type psx-cdrom:cdrom)
  (joypads (psx-joypads:make-joypads) :type psx-joypads:joypads)
  (gpu (psx-gpu:make-gpu) :type psx-gpu:gpu)
  (spu (psx-spu:make-spu) :type psx-spu:spu)
  ; TODO(Samantha): I'm not convinced this is going to work out cleanly.
  (dma (psx-dma:make-dma) :type psx-dma:dma)
  (bios-rom
   (make-array #x80000 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (#x80000)))
  ; The #x200000s here are indicative of the non-mirrored ram size of the psx.
  ; We already have a constant for this size, but using it here makes the
  ; macroexpander cry.
  (ram
   (make-array #x200000 :element-type '(unsigned-byte 8) :initial-element 0)
   :type (simple-array (unsigned-byte 8) (#x200000))))

(defun load-rom-from-file (filepath)
  (with-open-file (stream filepath :element-type '(unsigned-byte 8))
    (let ((rom (make-array (file-length stream)
                           :element-type '(unsigned-byte 8))))
      (read-sequence rom stream)
      rom)))

(declaim (ftype (function (psx pathname &optional pathname)) console-on))
(defun console-on (psx bios-rom-path &optional iso-path)
  (when iso-path
    (setf (psx-cdrom:cdrom-image (psx-cdrom psx))
          (load-rom-from-file iso-path)))
  (setf (psx-bios-rom psx) (load-rom-from-file bios-rom-path))
  (map-memory psx)
  (psx-cpu:power-on (psx-cpu psx))
  (psx-gpu:power-on (psx-gpu psx))
  (setf (psx-scheduler:component-sync-callback
         (aref (psx-scheduler:scheduler-components (psx-scheduler psx)) 0))
        (lambda (epoch) (psx-gpu:sync (psx-gpu psx) epoch)))
  (setf (psx-timers::timers-system-clock-callback (psx-timers psx))
        (lambda () (psx-scheduler:scheduler-master-clock (psx-scheduler psx))))
  (psx-timers:init-timers (psx-timers psx))
  (values))

; TODO(Samantha): Rename this to something more descriptive
(declaim (ftype (function (pathname) psx) make-console))
(defun make-console (bios-rom-path)
  (let ((my-psx (make-psx)))
    (console-on my-psx bios-rom-path)
    my-psx))

(declaim (ftype (function (pathname) (unsigned-byte 32)) setup-and-run))
(defun setup-and-run (bios-rom-path)
  (psx-renderer:initialize)
  (psx-input:init-pads)
  (let ((psx (make-console bios-rom-path)))
    (loop for cpu-clocks = (step-cpu (psx-cpu psx))

      ; TODO(Samantha): This isn't even kind of right. It should be tied to
      ; various clocks, not just each instruction.
      ; TODO(Samantha): Timers is dog slow, optimize it.
      do (psx-timers:sync-timers (psx-timers psx))
      do (psx-scheduler:sync-components (psx-scheduler psx) cpu-clocks)
      do (psx-joypads:tick-joypads (psx-joypads psx) cpu-clocks)))
  0)
