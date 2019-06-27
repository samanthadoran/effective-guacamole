(defpackage #:psx-scheduler
  (:nicknames #:scheduler)
  (:use :cl)
  (:export #:scheduler #:scheduler-components #:make-scheduler
           #:register-sync-event #:scheduler-master-clock
           #:component-epoch-of-next-sync
           #:sync-components #:component-sync-callback))

(in-package :psx-scheduler)

(declaim (optimize (speed 3) (safety 1)))

(defstruct component
  "Represents a syncable component."
  ; TODO(Samantha): It might finally be time to make this a class and have the
  ; syncable components inherit from it.
  (epoch-of-next-sync 0 :type (unsigned-byte 62))
  (sync-callback
   (lambda (clock) (declare (ignore clock)))
   :type (function ((unsigned-byte 62)))))

(defstruct scheduler
  "Records all of the various syncable components and the system's master clock.
   Responsible for handling sync event registration and component syncing
   itself."
  (master-clock 0 :type (unsigned-byte 62))
  (components
   (make-array '(3)
               :element-type 'component
               :initial-contents (vector (make-component)
                                         (make-component)
                                         (make-component)))
   :type (simple-array component (3))))

(declaim (ftype (function (scheduler keyword (unsigned-byte 62)))))
(defun register-sync-event (scheduler component epoch-of-next-sync)
  "Sets a definitive time for when a component knows it will _need_ to sync.
   Pleast note that this is not the only way a component can sync, as is the
   case that happens when the cpu attempts to read or write to a syncable
   component."
  (ecase component
         (:gpu (setf (component-epoch-of-next-sync
                      (aref (scheduler-components scheduler)
                            0))
                     epoch-of-next-sync))
         (:timers (setf (component-epoch-of-next-sync
                         (aref (scheduler-components scheduler)
                               1))
                        epoch-of-next-sync))
         (:joypads (setf (component-epoch-of-next-sync
                          (aref (scheduler-components scheduler)
                                2))
                         epoch-of-next-sync))))

(declaim (ftype (function (scheduler (unsigned-byte 62)))
                sync-components))
(defun sync-components (scheduler clocks)
  "Loops through all registered syncable components and updates those that have
   registered a predicted sync."
  (let ((previous-clock (scheduler-master-clock scheduler)))
    (setf (scheduler-master-clock scheduler)
          (ldb (byte 62 0) (+ (scheduler-master-clock scheduler)
                              clocks)))
    (loop for component across (scheduler-components scheduler)
      do (when (<= (1+ previous-clock)
                   (component-epoch-of-next-sync component)
                   (scheduler-master-clock scheduler))
           (funcall (component-sync-callback component)
                    (scheduler-master-clock scheduler))))
    (values)))
