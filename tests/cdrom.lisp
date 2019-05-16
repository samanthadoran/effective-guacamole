(defpackage #:psx-cdrom-tests
  (:nicknames #:cdrom-test)
  (:use :cl
        :rove
        :psx-cdrom))

(in-package :psx-cdrom-tests)

(deftest test-stuff
  (testing "asdf"
    (ok t)))
