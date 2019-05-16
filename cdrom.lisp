(defpackage #:psx-cdrom
  (:nicknames #:cdrom)
  (:use :cl)
  (:import-from
   :fset
   :empty-seq :wb-seq :empty? :size :seq :less-first :with-last)
  (:export #:cdrom #:make-cdrom #:read-cdrom-registers #:write-cdrom-registers
           #:cdrom-exception-callback #:cdrom-image))

(in-package :psx-cdrom)

(declaim (optimize (speed 3) (safety 1)))

(defstruct cdrom
  "Encompasses all of the internal state for the cdrom controller, disc, and
  any other information."
  (image
   (make-array 0 :fill-pointer 0 :adjustable t
               :initial-element 0 :element-type '(unsigned-byte 8))
   :type (array (unsigned-byte 8) *))

  (exception-callback
   (lambda () 0)
   :type (function () (unsigned-byte 8)))
  (interrupt-enable #xFF :type (unsigned-byte 8))
  (interrupt-status #x00 :type (unsigned-byte 8))
  (index 0 :type (unsigned-byte 2))
  (command-busy nil :type boolean)

  ;; TODO(Samantha): Specialize on wb-seq?
  (remaining-interrupts (empty-seq) :type wb-seq)
  (data-fifo (empty-seq) :type wb-seq)
  (xa-adpcm-fifo (empty-seq) :type wb-seq)
  (response-fifo (empty-seq) :type wb-seq)
  (parameter-fifo (empty-seq) :type wb-seq))

(declaim (ftype (function (cdrom)
                          (unsigned-byte 8))
                status-register))
(defun status-register (cdrom)
  "Make a psx status register as an eight-bit value from the cdrom."
  ; TODO(Samantha): This is painfully error prone, fixme?
  (logior
   (ash (cdrom-index cdrom) 0)
   (ash (if (empty? (cdrom-xa-adpcm-fifo cdrom)) 0 1) 2)
   (ash (if (empty? (cdrom-parameter-fifo cdrom)) 1 0) 3)
   (ash (if (= 16 (size (cdrom-parameter-fifo cdrom))) 0 1) 4)
   (ash (if (empty? (cdrom-response-fifo cdrom)) 0 1) 5)
   (ash (if (empty? (cdrom-data-fifo cdrom)) 0 1) 6)
   (ash (if (cdrom-command-busy cdrom) 1 0) 7)))

 (declaim (ftype (function (cdrom (unsigned-byte 8)))
                 write-status-register))
 (defun write-status-register (cdrom value)
   "Handle writes to the status register of the CDROM. All fields in this
   register are immuatable, other than the index field. As such, only
   bits [0, 2] matter as input to this function."
   (setf (cdrom-index cdrom)
         (ldb (byte 2 0) value))
   (values))

(declaim (ftype (function (cdrom)
                          (unsigned-byte 8))
                read-response-fifo))
(defun read-response-fifo (cdrom)
  "Reads the first value in the response fifo and then progresses it to
  the next. If the response fifo is empty, returns 0."
  (let ((response (or (fset:first (cdrom-response-fifo cdrom))
                      0)))
    (setf (cdrom-response-fifo cdrom)
          (less-first (cdrom-response-fifo cdrom)))
    response))

(declaim (ftype (function (cdrom (unsigned-byte 8)))
                write-parameter-fifo))
(defun write-parameter-fifo (cdrom value)
  "Handles adding items to the parameter fifo. If there are 16 items in
  the fifo, it throws the given value away."
  ; TODO(Samantha): Verify behaviour past 16 values?
  (unless (= 16 (size (cdrom-parameter-fifo cdrom)))
    (setf (cdrom-parameter-fifo cdrom)
          (with-last (cdrom-parameter-fifo cdrom) value))))

(declaim (ftype (function (cdrom)
                          (unsigned-byte 8))
                get-stat))
(defun get-stat (cdrom)
  (declare (ignore cdrom))
  ; TODO(Samantha): Actually implement this.
  #x10)

(declaim (ftype (function (cdrom (unsigned-byte 8)))
                write-interrupt-flag))
(defun write-interrupt-flag (cdrom value)
  "Handles the various bookkeeping of writing to the interrupt flag register."
  (when (ldb-test (byte 5 0) value)
    ; The behaviour over not clearing all response bits is a bit odd, ignore
    ; for now?
    (unless (= (ldb (byte 3 0) value) 7)
      (error "We don't handle not clearing all of the irq bits."))
    ; TODO(Samantha): Handle interrupts past int 5.
    (setf (cdrom-remaining-interrupts cdrom)
          (less-first (cdrom-remaining-interrupts cdrom))))

  (when (ldb-test (byte 1 6) value)
    (setf (cdrom-parameter-fifo cdrom)
          (empty-seq)))

  (values))

(declaim (ftype (function (cdrom (unsigned-byte 8)))
                raise-cdrom-interrupt))
(defun raise-cdrom-interrupt (cdrom interrupt)
  "Conditionally adds a pending interrupt to the queue depending on the mask."
  ; TODO(Samantha): We can only mask interrupts [1, 5]? Verify.
  (when (or (ldb-test (byte 1 (1- interrupt)) (cdrom-interrupt-enable cdrom))
            (> interrupt 5))
    ; TODO(Samantha): This needs to move to a cdrom-tick.
    (funcall (cdrom-exception-callback cdrom))
    (setf (cdrom-remaining-interrupts cdrom)
          (with-last (cdrom-remaining-interrupts cdrom) interrupt)))
  (values))

(declaim (ftype (function (cdrom (unsigned-byte 8)))
                write-command))
(defun write-command (cdrom command)
  ; TODO(Samantha): Find a place for delay timing of the parameter fifo
  ; transferring to the controller. The IRQ should _not_ fire immediately.
  (when (cdrom-command-busy cdrom)
    (error "Trying to send a command while the busy flag is up is not good!"))

  (unless (empty? (cdrom-remaining-interrupts cdrom))
    (error "Remaining interrupts should be false because command isn't busy."))

  (ecase command
         ;; GetStat
         (#x1
           (setf (cdrom-response-fifo cdrom)
                 (with-last (cdrom-response-fifo cdrom) #x10))

           (raise-cdrom-interrupt cdrom #x3))
         ;; Tests with subfunctions
         (#x19
           (ecase (fset:first (cdrom-parameter-fifo cdrom))
                  ;; cdrom bios version
                  (#x20
                    (setf (cdrom-response-fifo cdrom)
                          (seq #x97 #x01 #x10 #xC2))

                    (raise-cdrom-interrupt cdrom #x3)))))

  ; TODO(Samantha): This isn't really how this works. Transmission takes time.
  (setf (cdrom-parameter-fifo cdrom)
        (empty-seq))
  (values))

(declaim (ftype (function (cdrom (unsigned-byte 2))
                          (unsigned-byte 8))
                read-cdrom-registers))
(defun read-cdrom-registers (cdrom offset)
  (ecase offset
         (0 (status-register cdrom))
         (1 (ecase (cdrom-index cdrom)
                   (1
                    (let ((response
                           (or (fset:first (cdrom-response-fifo cdrom))
                               0)))
                      (setf (cdrom-response-fifo cdrom)
                            (less-first (cdrom-response-fifo cdrom)))
                      response))))
         (3 (ecase (cdrom-index cdrom)
                   (0 (cdrom-interrupt-enable cdrom))
                   (1 (fset:first (cdrom-remaining-interrupts cdrom)))
                   (2 (cdrom-interrupt-enable cdrom))
                   (3 (fset:first (cdrom-remaining-interrupts cdrom)))))))

(declaim (ftype (function (cdrom (unsigned-byte 2) (unsigned-byte 8))
                          (unsigned-byte 8))
                write-cdrom-registers))
(defun write-cdrom-registers (cdrom offset value)
  (ecase offset
         (0 (write-status-register cdrom value))
         (1 (ecase (cdrom-index cdrom)
                   (0 (write-command cdrom value))))
         (2 (ecase (cdrom-index cdrom)
                   (0 (write-parameter-fifo cdrom value))
                   (1
                    (setf (cdrom-interrupt-enable cdrom)
                          value))))
         (3 (ecase (cdrom-index cdrom)
                   (1 (write-interrupt-flag cdrom value)))))
  value)
