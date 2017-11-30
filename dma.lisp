(defpackage #:psx-dma
  (:nicknames #:dma)
  (:use :cl)
  (:export #:dma #:make-dma #:dma-control-register #:dma-interrupt-register
           #:dma-channels))

(in-package :psx-dma)
(declaim (optimize (speed 3) (safety 1)))

(defstruct channel
  (base 0 :type (unsigned-byte 24))
  (block-control 0 :type (unsigned-byte 24)))

(defstruct dma
  (control-register 0 :type (unsigned-byte 32))
  (interrupt-register 0 :type (unsigned-byte 32))
  (channels
   (make-array 7
               :element-type 'channel
               ; It would be nice to use :initial-element, but the argument to
               ; it is only evaluated once.
               :initial-contents `(,(make-channel) ,(make-channel)
                                   ,(make-channel) ,(make-channel)
                                   ,(make-channel) ,(make-channel)
                                   ,(make-channel)))
   :type (simple-array channel (7))))
