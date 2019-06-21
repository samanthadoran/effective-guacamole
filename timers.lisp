(defpackage #:psx-timers
  (:nicknames #:timers)
  (:use :cl)
  (:export #:timers #:make-timers #:read-timers #:write-timers
           #:advance-timers #:timers-exception-callback
           #:sync-timers #:init-timers #:timers-sync-callback))

(in-package :psx-timers)
(declaim (optimize (speed 3) (safety 1)))

(defstruct mode
  (synchronization-enabled nil :type boolean)
  (synchronization-mode 0 :type (unsigned-byte 2))
  (zero-counter-condition :at-max :type keyword)
  (irq-at-target-value nil :type boolean)
  (irq-at-max-value nil :type boolean)
  (irq-frequency :once :type keyword)
  (irq-style :pulse :type keyword)
  ; Each different timer has a different meaning for this selection...
  (clock-source 0 :type (integer 0 3))
  (clock-source-callback
   (lambda () 0)
   :type (function () (unsigned-byte 63)))
  (cycles-till-value
   (lambda (value) value)
   :type (function ((unsigned-byte 63))
                   (unsigned-byte 63)))
  (reached-max-value nil :type boolean)
  (reached-target-value nil :type boolean)
  (fired-irq nil :type boolean))

(defstruct timer
  (identifier :timer0 :type keyword)
  (source-sync-epoch 0 :type (unsigned-byte 63))
  (current-value 0 :type (unsigned-byte 16))
  (target-value 0 :type (unsigned-byte 16))
  (mode (make-mode) :type mode)
  (synchronization-modes 0 :type (integer 0 3)))

(declaim (ftype (function (mode)
                          (unsigned-byte 16))
                mode-to-word))
(defun mode-to-word (mode)
  (logior
   (ash (if (mode-synchronization-enabled mode) 1 0) 0)
   (ash (mode-synchronization-mode mode) 1)
   (ash (if (eql (mode-zero-counter-condition mode) :at-target) 1 0) 3)
   (ash (if (mode-irq-at-target-value mode) 1 0) 4)
   (ash (if (mode-irq-at-max-value mode) 1 0) 5)
   (ash (if (eql (mode-irq-frequency mode) :repeat) 1 0) 6)
   (ash (if (eql (mode-irq-style mode) :toggle) 1 0) 7)
   (ash (mode-clock-source mode) 8)
   (ash (if (has-irq mode) 0 1) 10)
   (ash (if (mode-reached-target-value mode) 1 0) 11)
   (ash (if (mode-reached-max-value mode) 1 0) 12)))

(declaim (ftype (function ((unsigned-byte 16))
                          mode)
                  word-to-mode))
(defun word-to-mode (word)
  (make-mode
   :synchronization-enabled (ldb-test (byte 1 0) word)
   :synchronization-mode (ldb (byte 2 1) word)
   :zero-counter-condition (if (ldb-test (byte 1 3) word) :at-target :at-max)
   :irq-at-target-value (ldb-test (byte 1 4) word)
   :irq-at-max-value (ldb-test (byte 1 5) word)
   :irq-frequency (if (ldb-test (byte 1 6) word) :repeat :once)
   :irq-style (if (ldb-test (byte 1 7) word) :toggle :pulse)))

(declaim (ftype (function (mode) boolean) has-irq))
(defun has-irq (mode)
  (or (and (mode-reached-target-value mode)
           (mode-irq-at-target-value mode))
      (and (mode-reached-max-value mode)
           (mode-irq-at-max-value mode))))

(defstruct timers
  ; TODO(Samantha): Figure out why this being 64 bit causes SBCL optimization
  ; notes to complain.
  (clock 0 :type (unsigned-byte 63))
  ; TODO(Samantha): Add dotclock and hblank callbacks.
  (sync-callback
   (lambda (clock) (declare (ignore clock)))
   :type (function ((unsigned-byte 63))))
  (system-clock-callback
   (lambda () 0)
   :type (function () (unsigned-byte 63)))
  (exception-callback
   (lambda (keyword) (declare (ignore keyword)) 0)
   :type (function (keyword) (unsigned-byte 9)))
  (timers
   (make-array 3
               :element-type 'timer
               :initial-contents (vector
                                  (make-timer :identifier :timer0)
                                  (make-timer :identifier :timer1)
                                  (make-timer :identifier :timer2)))
   :type (simple-array timer (3))))

(declaim (ftype (function (timers timer (integer 0 3)))
                set-clocksource))
(defun set-clocksource (timers timer source)
  (let* ((mode (timer-mode timer))
         (cycles-till-value
          (lambda (value)
                  (- value
                     (funcall
                      (mode-clock-source-callback mode))))))

    (ecase (timer-identifier timer)
           (:timer0
            (ecase source
                   ; System clock
                   (0 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; Dotclock
                   ; TODO(Samantha): Change this to the proper source
                   (1 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; System clock
                   (2 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; Dot clock
                   ; TODO(Samantha): Change this to the proper source
                   (3 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))))
           (:timer1
            (ecase source
                   ; System clock
                   (0 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; Hblank
                   ; TODO(Samantha): Change this to the proper source
                   (1 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; System clock
                   (2 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; Hblank
                   ; TODO(Samantha): Change this to the proper source
                   (3 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))))
           (:timer2
            (ecase source
                   ; System clock
                   (0 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; System clock
                   (1 (setf (mode-clock-source-callback mode)
                            (timers-system-clock-callback timers))
                      (setf (mode-cycles-till-value mode)
                            cycles-till-value))
                   ; System clock div 8
                   (2 (setf (mode-clock-source-callback mode)
                            (lambda ()
                                    (truncate (/ (funcall
                                                  (timers-system-clock-callback timers))
                                                 8.0))))
                      (setf (mode-cycles-till-value mode)
                            (lambda (value)
                                    (* 8 (funcall cycles-till-value value)))))
                   ; System clock div 8
                   (3 (setf (mode-clock-source-callback mode)
                            (lambda ()
                                    (truncate (/ (funcall
                                                  (timers-system-clock-callback timers))
                                                 8.0))))
                      (setf (mode-cycles-till-value mode)
                            (lambda (value)
                                    (* 8 (funcall cycles-till-value value))))))))
    (setf (timer-source-sync-epoch timer)
          (funcall (mode-clock-source-callback mode)))
    (setf (mode-clock-source mode)
          source))
  (values))

(declaim (ftype (function (timer)
                          boolean)
                has-repeat-irq))
(defun has-repeat-irq (timer)
  (or
   (and (= (timer-current-value timer) (timer-target-value timer))
        (mode-irq-at-target-value (timer-mode timer)))
   (and (= (timer-current-value timer) #xFFFF)
        (mode-irq-at-max-value (timer-mode timer)))))

(declaim (ftype (function (timers timer)
                          (unsigned-byte 8))
                generate-irq))
(defun generate-irq (timers timer)
  (if (eql (mode-irq-frequency (timer-mode timer)) :repeat)
    (when (has-repeat-irq timer)
      (funcall (timers-exception-callback timers) (timer-identifier timer)))

    (when (and (has-irq (timer-mode timer))
               (not (mode-fired-irq (timer-mode timer))))
      (funcall (timers-exception-callback timers) (timer-identifier timer))
      (setf (mode-fired-irq (timer-mode timer)) t)))
  0)

(declaim (ftype (function (timers timer))
                sync-timer))
(defun sync-timer (timers timer)
  (let ((cycles-since-sync (- (funcall (mode-clock-source-callback
                                        (timer-mode timer)))
                              (timer-source-sync-epoch timer)))
        (old-value (timer-current-value timer))
        (mode (timer-mode timer)))

    ; TODO(Samantha): Sync modes?

    ; Move the timer's last sync forwards
    (setf (timer-source-sync-epoch timer)
          (funcall (mode-clock-source-callback
                    mode)))

    (setf (timer-current-value timer)
          (ldb (byte 16 0) (+ cycles-since-sync old-value)))

    (when (<= (1+ old-value)
              #xFFFF
              (+ old-value
                 cycles-since-sync))

      (setf (mode-reached-max-value mode)
            t)

      (when (mode-irq-at-max-value mode)
      ; TODO(Samantha): Pass the type of irq?
        (generate-irq timers timer)))

    (when (<= (1+ old-value)
              (timer-target-value timer)
              (+ old-value
                 cycles-since-sync))

      (setf (mode-reached-target-value mode)
            t)

      (when (eql (mode-zero-counter-condition mode) :at-target)
        (setf (timer-current-value timer)
              (mod (+ (timer-current-value timer)
                      cycles-since-sync)
                   (timer-target-value timer))))

      (when (mode-irq-at-max-value mode)
      ; TODO(Samantha): Pass the type of irq?
        (generate-irq timers timer)))
    ; TODO(Samantha): take the min of cycles until target value and cycles until
    ; maximum value and register a sync.
    (values)))

(declaim (ftype (function (timer)
                          (unsigned-byte 63))
                ticks-until-nece))
(defun ticks-until-necessary-sync (timer)
  (funcall
   (mode-cycles-till-value (timer-mode timer))
   (+
    (timer-source-sync-epoch timer)
    (min (if (< (- (timer-target-value timer)
                   (timer-current-value timer))
                0)
           #x1FFFF
           (- (timer-target-value timer)
            (timer-current-value timer)))
         (- #xFFFF
            (timer-current-value timer))))))

(declaim (ftype (function (timers))
                init-timers))
(defun init-timers (timers)
  (loop for timer across (timers-timers timers)
    do (set-clocksource timers timer 0))
  (values))

(declaim (ftype (function (timers))
                sync-timers))
(defun sync-timers (timers)
  (loop for timer across (timers-timers timers)
    do (sync-timer timers timer))
  (funcall (timers-sync-callback timers)
           (min (ticks-until-necessary-sync (aref (timers-timers timers) 0))
                (ticks-until-necessary-sync (aref (timers-timers timers) 1))
                (ticks-until-necessary-sync (aref (timers-timers timers) 2))))
  (values))

(declaim (ftype (function (timers (unsigned-byte 8))
                          (unsigned-byte 16))
                read-timers))
(defun read-timers (timers offset)
  (sync-timers timers)
  (let ((timer (aref (timers-timers timers) (ldb (byte 2 4) offset))))
    (case (ldb (byte 4 0) offset)
      (0 (timer-current-value timer))
      (4 (let ((mode (mode-to-word (timer-mode timer))))
           (setf (mode-reached-max-value (timer-mode timer)) nil)
           (setf (mode-reached-target-value (timer-mode timer)) nil)
           (setf (mode-fired-irq (timer-mode timer)) nil)
           mode))
      (8 (timer-target-value timer))
      (otherwise
       (error "Invalid timer register index: #x~1,'0x with timer ~
               number: #x~1,'0x"
              (ldb (byte 4 0) offset) (ldb (byte 2 4) offset))))))

(declaim (ftype (function (timers (unsigned-byte 8) (unsigned-byte 32))
                          (unsigned-byte 16))
                write-timers))
(defun write-timers (timers offset value)
  (let ((timer (aref (timers-timers timers) (ldb (byte 2 4) offset)))
        (value (ldb (byte 16 0) value)))
    (case (ldb (byte 4 0) offset)
      (0 (setf (timer-current-value timer) value))
      ; Writing to a timer's mode register causes the current value to
      ; be reset to 0.
      (4 (setf (timer-current-value timer) #x0000)
         (setf (timer-mode timer) (word-to-mode value))
         (set-clocksource timers timer (ldb (byte 2 8) value)))
      (8 (setf (timer-target-value timer) value))
      (otherwise
       (error "Invalid timer register index: #x~1,'0x with timer ~
                             number: #x~1,'0x"
              (ldb (byte 4 0) offset) (ldb (byte 2 4) offset))))
    (sync-timers timers)
    value))
