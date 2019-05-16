(defpackage #:psx-memory-tests
  (:nicknames #:memory-test)
  (:use :cl
        :rove
        :memory-constants))

(in-package :psx-memory-tests)

(deftest test-in-range
  (testing "In range"
    (ok (in-range 1 5 3)))
  (testing "Above range"
    (ng (in-range 1 5 10)))
  (testing "Below range"
    (ng (in-range 1 5 0)))
  (testing "Bottom edge of range"
    (ok (in-range 1 5 1)))
  (testing "Top edge of range"
    (ok (in-range 1 5 5))))

(deftest test-byte-array-io
  (let ((array (make-array 4 :element-type '(unsigned-byte 8)
                           :initial-contents '(1 2 3 4))))
    (testing "half-word<->byte-array io"
      (write-half-word-to-byte-array array 2 #xDEAD)
      (ok (= (read-half-word-from-byte-array array 2) #xDEAD))
      (ok (= (aref array 2) #xAD))
      (ok (= (aref array 3) #xDE)))
    (testing "word<->byte-array io"
      (write-word-to-byte-array array 0 #xCAFEBABE)
      (ok (= (read-word-from-byte-array array 0) #xCAFEBABE))
      (ok (= (aref array 0) #xBE))
      (ok (= (aref array 1) #xBA))
      (ok (= (aref array 2) #xFE))
      (ok (= (aref array 3) #xCA))
      (ok (= (read-half-word-from-byte-array array 0) #xBABE))
      (ok (= (read-half-word-from-byte-array array 2) #xCAFE)))))

(deftest test-sign-conversion
  (testing "to-signed-byte-32"
    (ok (= (to-signed-byte-32 #xFFFFFFFF) -1))
    (ok (= (to-signed-byte-32 #xFF) 255))
    (ok (= (to-signed-byte-32 0) 0)))
  (testing "byte-sign-extension"
    (ok (= (sign-extend-byte 1) 1))
    (ok (= (sign-extend-byte #xFF) #xFFFFFFFF)))
  (testing "half-word-sign-extension"
    (ok (= (sign-extend #xFFFF) #xFFFFFFFF))
    (ok (= (sign-extend #x1FFF) #x1FFF))
    (ok (= (sign-extend #x8FFF) #xFFFF8FFF))))
