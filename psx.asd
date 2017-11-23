(in-package #:asdf-user)

(defsystem #:psx
  :components (
               (:file "cop0")
               (:file "cpu" :depends-on ("cop0"))
               (:file "console" :depends-on ("cpu"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu"))))
