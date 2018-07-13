(in-package #:asdf-user)

(defsystem #:psx
  :author "Samantha Doran <samanthadoran3@gmail.com>"
  :license "GPLv3"
  :homepage "https://github.com/samanthadoran/effective-guacamole"
  :depends-on (#:cepl
               #:cepl.sdl2
               #:swank
               #:rtg-math.vari
               #:livesupport
               #:skitter
               #:cepl.skitter.sdl2
               #:cepl.devil
               #:varjo)
  :serial t
  :components (
               (:file "memory-constants")
               (:file "cop0")
               (:file "irq")
               (:file "timers")
               (:file "cache-control")
               (:file "cpu" :depends-on ("cop0" "memory-constants" "cache-control"))
               (:file "gpu")
               (:file "cdrom")
               (:file "joypads")
               (:file "spu" :depends-on ("memory-constants"))
               (:file "dma" :depends-on ("memory-constants"))
               (:file "console" :depends-on ("cpu" "cache-control" "memory-constants" "gpu" "dma" "cdrom" "irq" "timers" "joypads"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu"))))
