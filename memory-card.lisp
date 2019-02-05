(defpackage #:psx-memory-card
  (:nicknames #:memcard)
  (:use :cl
        :memory-constants)
  (:export #:memory-card #:make-and-initialize-memory-card))

(in-package :psx-memory-card)
(declaim (optimize (speed 3) (safety 1)))

(defstruct memory-card
  "Basic representation of a 128 KiB psx memory card as produced by Sony."
  (flag #x8 :type (unsigned-byte 8))
  ; A psx memory card has a total storage of 128KiB (#x20000 bytes) which is
  ; divided into 16 blocks. A block on a psx memory card has a total storage of
  ; 8KiB (#x2000 bytes) which is divided into 64 frames. Finally, a frame has
  ; a total storage of 128 (#x80) bytes.
  (data
   (make-array #x20000
               :element-type '(unsigned-byte 8)
               :initial-element 0)
   :type (simple-array (unsigned-byte 8) (#x20000))))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-frame))
(defun make-frame ()
  (make-array #x80 :element-type '(unsigned-byte 8)
              :initial-element 0))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-header-frame))
(defun make-header-frame ()
  "Allocates and initializes the header frame of a psx memory card.
  This frame contains identification information as well as a checksum."
  (let ((header (make-frame)))
    (setf (aref header 0) (char-code #\M))
    (setf (aref header 1) (char-code #\C))
    ; All other bytes in between are unused.

    (setf (aref header 127)
          (reduce #'logxor header))
    header))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-write-test-frame))
(defun make-write-test-frame ()
  "Allocates and initializes the write test frame of a psx memory card.
  Upon initialization, this is the same as the header frame."
  (make-header-frame))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-title-frame))
(defun make-title-frame ()
  (let ((title (make-frame)))
    title))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-icon-frame))
(defun make-icon-frame ()
  (let ((icon (make-frame)))
    icon))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-data-frame))
(defun make-data-frame ()
  (let ((data (make-frame)))
    data))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-broken-sector-list-frame))
(defun make-broken-sector-list-frame ()
  (let ((broken-sector (make-frame)))
    (write-word-to-byte-array broken-sector 0 #xFFFFFFFF)
    (setf (aref broken-sector 127)
          (reduce #'logxor broken-sector))
    broken-sector))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x80)))
                make-broken-sector-data-frame))
(defun make-broken-sector-data-frame ()
  (let ((broken-sector (make-frame)))
    broken-sector))

(defun make-directory-frame ()
  (let ((directory (make-frame)))
    (write-word-to-byte-array directory 0 #xA0)
    ; Filesize is zero as it is freshly allocated.
    (setf (aref directory 8) #xFF)
    (setf (aref directory 9) #xFF)
    (setf (aref directory 127)
          (reduce #'logxor directory))
    directory))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x2000)))
                make-block))
(defun make-block ()
  (make-array #x2000 :element-type '(unsigned-byte 8)
              :initial-element 0))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x2000)))
                make-block-zero))
(defun make-block-zero ()
  "Allocates and initializes block zero of a psx memory card.
  The initial block on the memory card contains identifying information in
  addition to functioning as a basic file allocation table (Frame: [1, 15]),
  broken sector identification (Frame: [16, 35]) and
  mitigation (Frame: [36, 55]), and write testing (Frame: 63). All other
  frames ([56, 62]) are unused, though filled with #xFF."
  (let ((block-zero (make-block)))
    (setf (subseq block-zero 0)
          (make-header-frame))
    (loop for i from 1 to 15
      do (setf (subseq block-zero (* i 128))
               (make-directory-frame)))
    (loop for i from 16 to 35
      do (setf (subseq block-zero (* i 128))
               (make-broken-sector-list-frame)))
    (loop for i from 36 to 55
      do (setf (subseq block-zero (* i 128))
               (make-broken-sector-data-frame)))
    (setf (subseq block-zero (* 63 128))
          (make-write-test-frame))
    block-zero))

(declaim (ftype (function ()
                          (simple-array (unsigned-byte 8) (#x2000)))
                make-data-block))
(defun make-data-block ()
  "Allocates and initializes a data block of a psx memory card.
  Data blocks contain not only the save information for a particular title,
  but also the title for the particular software (in either ASCII or Japanese),
  and image data for both animated and static icons."
  (let ((data-block (make-block)))
    (setf (subseq data-block 0)
          (make-title-frame))
    (setf (subseq data-block 128)
          (make-icon-frame))
    (loop for i from 2 to 63
      do (setf (subseq data-block (* i 128))
               (make-data-frame)))
    data-block))

(declaim (ftype (function ()
                          memory-card)
                make-and-initialize-memory-card))
(defun make-and-initialize-memory-card ()
  "Allocates and initializes a psx memory card to a default power on and
  formatted state."
  (let* ((memory-card (make-memory-card))
         (data (memory-card-data memory-card)))
    (setf (subseq data 0 #x2000)
          (make-block-zero))
    (loop for i from 1 to 15
      do (setf (subseq data (* i #x2000))
               (make-data-block)))
    memory-card))

(declaim (ftype (function (memory-card (integer 0 #x1FFFF))
                          (unsigned-byte 8))
                read-memory-card))
(defun read-memory-card (memory-card offset)
  (aref (memory-card-data memory-card) offset))

(declaim (ftype (function (memory-card (integer 0 #x1FFFF) (unsigned-byte 8))
                          (unsigned-byte 8))
                write-memory-card))
(defun write-memory-card (memory-card offset value)
  (setf (ldb (byte 1 3) (memory-card-flag memory-card))
        0)
  (setf (aref (memory-card-data memory-card) offset)
        value))
