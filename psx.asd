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
               #:varjo
               #:sdl2-game-controller-db
               #:fset)
  :serial t
  :components (
               (:file "memory-constants")
               (:file "memory-card" :depends-on ("memory-constants"))
               (:file "cop0")
               (:file "irq")
               (:file "timers")
               (:file "cache-control")
               (:file "cpu" :depends-on ("cop0" "memory-constants" "cache-control"))
               (:file "gpu")
               (:file "renderer" :depends-on ("gpu"))
               (:file "input")
               (:file "cdrom")
               (:file "joypads" :depends-on ("memory-card"))
               (:file "spu" :depends-on ("memory-constants"))
               (:file "dma" :depends-on ("memory-constants"))
               (:file "console" :depends-on ("cpu" "cache-control" "memory-constants" "gpu" "dma" "cdrom" "irq" "timers" "joypads" "renderer" "input"))
               (:file "mmu" :depends-on ("console"))
               (:file "instructions/instruction-macros" :depends-on ("cpu"))
               (:file "instructions/arithmeticops" :depends-on ("cpu"))
               (:file "instructions/branchops" :depends-on ("cpu"))
               (:file "instructions/loadstoreops" :depends-on ("cpu"))
               (:file "instructions/instructions" :depends-on ("cpu"))
               (:file "util/missing_ops" :depends-on ("cpu")))
  :in-order-to ((test-op (test-op #:psx/tests))))

(defsystem #:psx/tests
  :depends-on (#:psx #:rove)
  :components ((:file "tests/cdrom")
               (:file "tests/memory-constants"))
  :perform (test-op (o c) (symbol-call :rove '#:run c)))
