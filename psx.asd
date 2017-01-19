(in-package #:asdf-user)

(defsystem #:psx
  :components ((:file "cpu")
               (:file "console" :depends-on ("cpu"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros")
               (:file "instructions/arithmeticops")
               (:file "instructions/branchops")
               (:file "instructions/loadstoreops")
               (:file "instructions/instructions")))
