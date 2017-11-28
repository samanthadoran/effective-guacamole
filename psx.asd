(in-package #:asdf-user)

(defsystem #:psx
  :components (
               (:file "memory-constants")
               (:file "cop0")
               (:file "cpu" :depends-on ("cop0" "memory-constants"))
               (:file "gpu")
               (:file "console" :depends-on ("cpu" "memory-constants" "gpu"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu"))))
