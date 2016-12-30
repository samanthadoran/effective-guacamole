(in-package #:asdf-user)

(defsystem #:psx
  :components ((:file "cpu")
               (:file "console" :depends-on ("cpu"))))
