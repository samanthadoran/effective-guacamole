(in-package #:asdf-user)

(defsystem #:psx
  :components (
               (:file "memory-constants")
               (:file "cop0")
               (:file "cpu" :depends-on ("cop0" "memory-constants"))
               (:file "gpu")
               (:file "spu" :depends-on ("memory-constants"))
               (:file "dma" :depends-on ("memory-constants"))
               (:file "console" :depends-on ("cpu" "memory-constants" "gpu" "dma"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu"))))
