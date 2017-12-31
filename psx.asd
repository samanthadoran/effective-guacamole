(in-package #:asdf-user)

(defsystem #:psx
  :author "Samantha Doran <samanthadoran3@gmail.com>"
  :license "GPLv3"
  :homepage "https://github.com/samanthadoran/effective-guacamole"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:swank
               #:livesupport
               #:cepl.skitter.sdl2
               #:cepl.devil)
  :serial t
  :components (
               (:file "memory-constants")
               (:file "cop0")
               (:file "irq")
               (:file "timers")
               (:file "cpu" :depends-on ("cop0" "memory-constants"))
               (:file "gpu")
               (:file "cdrom")
               (:file "spu" :depends-on ("memory-constants"))
               (:file "dma" :depends-on ("memory-constants"))
               (:file "console" :depends-on ("cpu" "memory-constants" "gpu" "dma" "cdrom" "irq" "timers"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu"))))
