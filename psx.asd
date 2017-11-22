(in-package #:asdf-user)

(defsystem #:psx
  :components (
               (:file "cop0")
               (:file "cpu" :depends-on ("cop0"))
               (:file "console" :depends-on ("cpu"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros")
               (:file "instructions/arithmeticops")
               (:file "instructions/branchops")
               (:file "instructions/loadstoreops")
               (:file "instructions/instructions")
               (:file "util/missing_ops" :depends-on ("cpu"))))
