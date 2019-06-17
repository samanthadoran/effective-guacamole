(defpackage #:psx-scheduler-tests
  (:nicknames #:scheduler-test)
  (:use :cl
        :rove
        :psx-scheduler))

(in-package :psx-scheduler-tests)

(deftest sync-components-test
  (let* ((scheduler (make-scheduler))
         (test-component (aref (scheduler-components scheduler) 0))
         (sync-flag nil))

    ; The sync function will toggle a flag on fire.
    ; TODO(Samantha): I had tried using `(outputs)` here, but it didn't seem
    ; to catch any output from the funcalled lambda function within
    ; sync-components. Investigate?
    (setf (component-sync-callback test-component)
          (lambda (clock) (declare (ignore clock))(setf sync-flag t)))
    (setf (component-epoch-of-next-sync test-component)
          0)

    (testing "Stale sync"
      (setf (scheduler-master-clock scheduler) 1)
      (sync-components scheduler 1)
      (ng sync-flag))
    (testing "not yet sync"
      (setf sync-flag nil)
      (setf (scheduler-master-clock scheduler) 5)
      (setf (component-epoch-of-next-sync test-component)
            10)
      (sync-components scheduler 1)
      (ng sync-flag))
    (testing "sync"
      (setf sync-flag nil)
      (setf (scheduler-master-clock scheduler) 5)
      (setf (component-epoch-of-next-sync test-component)
            6)
      (sync-components scheduler 5)
      (ok sync-flag))))
