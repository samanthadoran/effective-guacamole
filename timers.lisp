(defpackage #:psx-timers
  (:nicknames #:timers)
  (:use :cl)
  (:export #:timers #:make-timers #:read-timers #:write-timers
           #:advance-timers #:timers-exception-callback))

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
  (clock-source 0 :type (unsigned-byte 2))
  (reached-max-value nil :type boolean)
  (reached-target-value nil :type boolean)
  (fired-irq nil :type boolean))

(defstruct timer
  (identifier :timer0 :type keyword)
  (clock-divider 0 :type (unsigned-byte 8))
  (current-value 0 :type (unsigned-byte 16))
  (mode (make-mode) :type mode)
  (target-value 0 :type (unsigned-byte 16))
  ; TODO(Samantha): Make a set of rules for this. Each counter has a different
  ; set of ways it syncs.
  (synchronization-modes nil :type boolean))

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
   :irq-style (if (ldb-test (byte 1 7) word) :toggle :pulse)
   :clock-source (ldb (byte 2 8) word)))

(declaim (ftype (function (mode) boolean) has-irq))
(defun has-irq (mode)
  (or (and (mode-reached-target-value mode)
           (mode-irq-at-target-value mode))
      (and (mode-reached-max-value mode)
           (mode-irq-at-max-value mode))))

(defstruct timers
  (clock 0 :type (unsigned-byte 64))
  (exception-callback
   (lambda (keyword) (declare (ignore keyword)) 0)
   :type (function (keyword) (unsigned-byte 9)))
  (timers
   (make-array 3
               :element-type 'timer
               :initial-contents `(,(make-timer :identifier :timer0
                                                :clock-divider 1)
                                   ,(make-timer :identifier :timer1
                                                :clock-divider 8)
                                   ,(make-timer :identifier :timer2
                                                :clock-divider 30)))
   :type (simple-array timer (3))))

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
    ; (progn
    ;  (setf (mode-fired-irq (timer-mode timer))
    ;        (not (mode-fired-irq (timer-mode timer))))
    ;  (unless (mode-fired-irq (timer-mode timer))
    ;    (when (has-repeat-irq timer)
    ;      (funcall (timers-exception-callback timers) (timer-identifier timer)))))
    (when (and (has-irq (timer-mode timer))
               (not (mode-fired-irq (timer-mode timer))))
      (funcall (timers-exception-callback timers) (timer-identifier timer))
      (setf (mode-fired-irq (timer-mode timer)) t)))
  0)

(declaim (ftype (function (timers timer)
                          (unsigned-byte 8))
                advance-timer))
(defun advance-timer (timers timer)
  (when (= (timer-current-value timer) #xFFFF)
    (setf (timer-current-value timer) #x0000))
  (when (and
         (eql (mode-zero-counter-condition (timer-mode timer)) :at-target)
         (= (timer-current-value timer) (timer-target-value timer)))
    (setf (timer-current-value timer) #x0000))
  (when (zerop (mod (timers-clock timers) (timer-clock-divider timer)))
    (incf (timer-current-value timer)))
  (unless (mode-reached-max-value (timer-mode timer))
    (setf (mode-reached-max-value (timer-mode timer))
          (= (timer-current-value timer) #xFFFF)))
  (unless (mode-reached-target-value (timer-mode timer))
    (setf (mode-reached-target-value (timer-mode timer))
          (= (timer-current-value timer) (timer-target-value timer))))
  (generate-irq timers timer)
  0)

(declaim (ftype (function (timers)) advance-timers))
(defun advance-timers (timers)
  ; TODO(Samantha): This only takes into account the timers being tied to one
  ; clock source.
  (setf (timers-clock timers)
        (logand #xFFFFFFFFFFFFFFFF (1+ (timers-clock timers))))
  (loop for timer being the elements of (timers-timers timers)
    do (advance-timer timers timer))
  (values))

(declaim (ftype (function (timers (unsigned-byte 8))
                          (unsigned-byte 16))
                read-timers))
(defun read-timers (timers offset)
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
         (setf (timer-mode timer) (word-to-mode value)))
      (8 (setf (timer-target-value timer) value))
      (otherwise
       (error "Invalid timer register index: #x~1,'0x with timer ~
                             number: #x~1,'0x"
              (ldb (byte 4 0) offset) (ldb (byte 2 4) offset))))
    value))
