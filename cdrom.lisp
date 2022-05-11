(defpackage #:psx-cdrom
  (:nicknames #:cdrom)
  (:use :cl)
  (:import-from
   :fset
   :empty-seq :concat :wb-seq :empty? :size :seq :less-first :with-last :do-seq)
  (:export #:cdrom #:make-cdrom #:read-cdrom-registers #:write-cdrom-registers
           #:cdrom-exception-callback #:cdrom-image #:cdrom-sync-callback #:sync
           #:cdrom-system-clock-callback))

(in-package :psx-cdrom)

(declaim (optimize (speed 3) (safety 1)))

(defstruct cdrom-position
  "Holds a position on cd specified as minutes:seconds:sectors"
  (minutes 0 :type (integer 0 74))
  (seconds 0 :type (integer 0 60))
  (sector 0 :type (integer 0 75)))

(defstruct cdrom-interrupt
  "Holds a queueable cdrom interrupt code and the associated action."
  (code 0 :type (integer 0 16))
  (action
   (lambda () (values))
   :type (function ())))

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
  (seek nil :type boolean)
  (read nil :type boolean)
  (position (make-cdrom-position) :type cdrom-position)

  ;; TODO(Samantha): Specialize on wb-seq?
  (system-clock-callback
   (lambda () 0)
   :type (function () (unsigned-byte 62)))
  (sync-callback
   (lambda (clock) (declare (ignore clock)))
   :type (function ((unsigned-byte 62))))
  (remaining-interrupts (empty-seq) :type wb-seq)
  (current-interrupt 0 :type (integer 0 16))
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
   (ash (if (= 16 (the fixnum (size (cdrom-parameter-fifo cdrom)))) 0 1) 4)
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
  (unless (= 16 (the fixnum (size (cdrom-parameter-fifo cdrom))))
    (setf (cdrom-parameter-fifo cdrom)
          (with-last (cdrom-parameter-fifo cdrom) value))))

(declaim (ftype (function (cdrom)
                          (unsigned-byte 8))
                get-stat))
(defun get-stat (cdrom)
  ; TODO(Samantha): Actually implement this.
  ; Shell open?
  (logior
   (ash
     (if (zerop (car (array-dimensions (cdrom-image cdrom))))
       1
       0)
     4)
   (ash 0 1)
   (ash (if (cdrom-read cdrom) 1 0) 5)
   (ash (if (cdrom-seek cdrom) 1 0) 6)))

(declaim (ftype (function (cdrom-position)
                          (unsigned-byte 64))
                position-to-offset))
(defun position-to-offset (position)
  "Converts a position into an LBA offset."
  (+ (* (+ (* 60 (cdrom-position-minutes position))
           (cdrom-position-seconds position)
         75))
     (cdrom-position-sector position)
     -150))

(declaim (ftype (function (cdrom))
                readN))
(defun readN (cdrom)
  (setf (cdrom-data-fifo cdrom)
        (fset:convert 'seq
                      (subseq (cdrom-image cdrom
                                           (position-to-offset (cdrom-position cdrom))
                                           (+ (position-to-offset (cdrom-position cdrom))
                                              ; TODO(Samantha): Move this to a variable called sector size.
                                              #x930)))))
  (values))

(declaim (ftype (function (cdrom cdrom-interrupt))
                process-interrupt))
(defun process-interrupt (cdrom interrupt)
  "Actually run the interrupt's associated actions and do some bookkeeping.
   Conditionally inform the system about the interupt."
  (when (or (ldb-test (byte 1 (1- (cdrom-interrupt-code interrupt))) (cdrom-interrupt-enable cdrom))
            (> (cdrom-interrupt-code interrupt) 5))
    (funcall (cdrom-exception-callback cdrom)))
  ; We still need to report an irq, it's just that we don't bother the cpu with
  ; it. I think? This seems to work.
  (setf (cdrom-current-interrupt cdrom)
        (cdrom-interrupt-code interrupt))
  (funcall (cdrom-interrupt-action interrupt))
  (values))

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
    (setf (cdrom-current-interrupt cdrom) 0))

  (when (ldb-test (byte 1 6) value)
    (setf (cdrom-parameter-fifo cdrom)
          (empty-seq)))

  (values))

(declaim (ftype (function (cdrom cdrom-interrupt))
                queue-cdrom-interrupt))
(defun queue-cdrom-interrupt (cdrom interrupt)
  "Add a new interrupt code and action to the current pending interrupts"
  (setf (cdrom-remaining-interrupts cdrom)
        (with-last (cdrom-remaining-interrupts cdrom) interrupt))
  (values))

; TODO(Samantha): Separate handling of command. Writing the command should
; technically take some amount of time.
(declaim (ftype (function (cdrom (unsigned-byte 8)))
                write-command))
(defun write-command (cdrom command)
  (when (cdrom-command-busy cdrom)
    (error "Trying to send a command while the busy flag is up is not good!"))

  ; TODO(Samantha): This assumption isn't correct. The busy flag only has to
  ; deal with command and parameter transmission. (I think.)
  ; (unless (empty? (cdrom-remaining-interrupts cdrom))
  ;   (error "Remaining interrupts should be false because command isn't busy."))
  ; Capture the current time to aid in scheduling interrupts.
  (let ((epoch (funcall (cdrom-system-clock-callback cdrom))))
    (ecase command
           ; GetStat
           (#x1
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "Getstat action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; Setloc
           (#x2
             (setf (cdrom-position cdrom)
                   (make-cdrom-position :minutes (fset:@ (cdrom-parameter-fifo cdrom) 0)
                                        :seconds (fset:@ (cdrom-parameter-fifo cdrom) 1)
                                        :sector (fset:@ (cdrom-parameter-fifo cdrom) 2)))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "Setloc action to: ~A~%" (cdrom-position cdrom))
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; ReadN
           (#x6
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "ReadN action~%")
                               (setf (cdrom-read cdrom) t)
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 4000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x1
               :action (lambda ()
                               (log:debug "ReadN action 2~%")
                               ; (setf (cdrom-read cdrom) nil)
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; Stop
           (#x8
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "Stop action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 4000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "Stop action 2~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; Init
           (#xA
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "Init action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 6000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x2
               :action (lambda ()
                               (log:debug "Init action 2~%")
                               (setf (cdrom-remaining-interrupts cdrom)
                                     (empty-seq))
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; SetMode
           (#xE
             ; TODO(Samantha): Actually perform this action.
             ; (log:warn "SetMode is currently not implemented, it's only a stub.")
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "SetMode action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 4000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x2
               :action (lambda ()
                               (log:debug "SetMode action 2~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; SeekL
           (#x15
             ; (log:debug "SeekL is currently not implemented, it's only a stub.")
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "SeekL action~%")
                               (setf (cdrom-seek cdrom) t)
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 6000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x2
               :action (lambda ()
                               (setf (cdrom-seek cdrom) nil)
                               (log:debug "SeekL action 2~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; GetID
           (#x1A
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "GetID action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch (* 5000 2))
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x2
               :action (lambda ()
                               (log:debug "GetID action 2~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (concat (cdrom-response-fifo cdrom)
                                             (seq #x02 #x00 #x20 #x00 #x53 #x43 #x45 #x41)))))))
           ; ReadTOC
           (#x1E
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x3
               :action (lambda ()
                               (log:debug "ReadTOC action~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))
                               ; Sync the cdrom for the next interrupt once
                               ; enough time has passed.
                               (let ((current-clock (funcall (cdrom-system-clock-callback cdrom))))
                                 (funcall (cdrom-sync-callback cdrom)
                                          (+ current-clock
                                             (- (+ epoch 4000 400000)
                                                current-clock)))))))
             (queue-cdrom-interrupt
              cdrom
              (make-cdrom-interrupt
               :code #x2
               :action (lambda ()
                               (log:debug "ReadTOC action 2~%")
                               (setf (cdrom-response-fifo cdrom)
                                     (with-last (cdrom-response-fifo cdrom)
                                       (get-stat cdrom)))))))
           ; Subfunctions
           (#x19
             (ecase (fset:first (cdrom-parameter-fifo cdrom))
                    ;; cdrom bios version
                    (#x20
                      (queue-cdrom-interrupt
                       cdrom
                       (make-cdrom-interrupt
                        :code #x3
                        :action (lambda ()
                                        (log:debug "test subfunction-action~%")
                                        (setf (cdrom-response-fifo cdrom)
                                              (seq #x97 #x01 #x10 #xC2))))))))))

  (setf (cdrom-parameter-fifo cdrom) (empty-seq))
  (funcall (cdrom-sync-callback cdrom)
           (+ (funcall (cdrom-system-clock-callback cdrom))
              4000))
  (values))

(declaim (ftype (function (cdrom (unsigned-byte 2))
                          (unsigned-byte 8))
                read-cdrom-registers))
(defun read-cdrom-registers (cdrom offset)
  (ecase offset
         (0 (status-register cdrom))
         (1 (ecase (cdrom-index cdrom)
                   (1 (read-response-fifo cdrom))))
         (3 (ecase (cdrom-index cdrom)
                   (0 (cdrom-interrupt-enable cdrom))
                   (1 (cdrom-current-interrupt cdrom))
                   (2 (cdrom-interrupt-enable cdrom))
                   (3 (cdrom-current-interrupt cdrom))))))

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

(declaim (ftype (function (cdrom (unsigned-byte 62)))
                sync))
(defun sync (cdrom clock)
  (declare (ignore clock))
  (unless (empty? (cdrom-remaining-interrupts cdrom))
    (process-interrupt cdrom (fset:first (cdrom-remaining-interrupts cdrom))))
  (setf (cdrom-remaining-interrupts cdrom)
        (less-first (cdrom-remaining-interrupts cdrom)))
  (funcall (cdrom-sync-callback cdrom)
           (+ (funcall (cdrom-system-clock-callback cdrom))
              4000))

  (values))
