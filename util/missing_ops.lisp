(in-package :psx-cpu)

(defvar instructions-test (make-hash-table :test 'equal))

; Regulars
; This instruction encodes four instructions.
(setf (gethash #x01 instructions-test) (list "b-cond-z" 'b-cond-z))
(setf (gethash #x02 instructions-test) (list "j" 'j))
(setf (gethash #x03 instructions-test) (list "jal" 'jal))
(setf (gethash #x04 instructions-test) (list "beq" 'beq))
(setf (gethash #x05 instructions-test) (list "bne" 'bne))
(setf (gethash #x06 instructions-test) (list "blez" 'blez))
(setf (gethash #x07 instructions-test) (list "bgtz" 'bgtz))
(setf (gethash #x08 instructions-test) (list "addi" 'addi))
(setf (gethash #x09 instructions-test) (list "addiu" 'addiu))
(setf (gethash #x0A instructions-test) (list "slti" 'slti))
(setf (gethash #x0B instructions-test) (list "sltiu" 'sltiu))
(setf (gethash #x0C instructions-test) (list "andi" 'andi))
(setf (gethash #x0D instructions-test) (list "ori" 'ori))
(setf (gethash #x0E instructions-test) (list "xori" 'xori))
(setf (gethash #x0F instructions-test) (list "lui" 'lui))
(setf (gethash #x10 instructions-test) (list "cop0" 'cop0))
(setf (gethash #x11 instructions-test) (list "cop1" 'cop1))
(setf (gethash #x12 instructions-test) (list "cop2" 'cop2))
(setf (gethash #x13 instructions-test) (list "cop3" 'cop3))

(setf (gethash #x20 instructions-test) (list "lb" 'lb))
(setf (gethash #x21 instructions-test) (list "lh" 'lh))
(setf (gethash #x22 instructions-test) (list "lwl" 'lwl))
(setf (gethash #x23 instructions-test) (list "lw" 'lw))
(setf (gethash #x24 instructions-test) (list "lbu" 'lbu))
(setf (gethash #x25 instructions-test) (list "lhu" 'lhu))
(setf (gethash #x26 instructions-test) (list "lwr" 'lwr))

(setf (gethash #x28 instructions-test) (list "sb" 'sb))
(setf (gethash #x29 instructions-test) (list "sh" 'sh))
(setf (gethash #x2A instructions-test) (list "swl" 'swl))
(setf (gethash #x2B instructions-test) (list "sw" 'sw))

(setf (gethash #x2E instructions-test) (list "swr" 'swr))

(setf (gethash #x30 instructions-test) (list "lwc0" 'lwc0))
(setf (gethash #x31 instructions-test) (list "lwc1" 'lwc1))
(setf (gethash #x32 instructions-test) (list "lwc2" 'lwc2))
(setf (gethash #x33 instructions-test) (list "lwc3" 'lwc3))

(setf (gethash #x38 instructions-test) (list "swc0" 'swc0))
(setf (gethash #x39 instructions-test) (list "swc1" 'swc1))
(setf (gethash #x3A instructions-test) (list "swc2" 'swc2))
(setf (gethash #x3B instructions-test) (list "swc3" 'swc3))
;
; ; Specials occur when bits [26, 31] = #x00, we encode as
; ; `(logior #xFF00 (ldb (byte 6 0) instruction-as-word))` to differentiate.
(setf (gethash #xFF00 instructions-test) (list "sll" 'sll))

(setf (gethash #xFF02 instructions-test) (list "srl" 'srl))
(setf (gethash #xFF03 instructions-test) (list "sra" 'sra))
(setf (gethash #xFF04 instructions-test) (list "sllv" 'sllv))

(setf (gethash #xFF06 instructions-test) (list "srlv" 'srlv))
(setf (gethash #xFF07 instructions-test) (list "srav" 'srav))
(setf (gethash #xFF08 instructions-test) (list "jr" 'jr))
(setf (gethash #xFF09 instructions-test) (list "jalr" 'jalr))

(setf (gethash #xFF0C instructions-test) (list "syscall" 'syscall))
(setf (gethash #xFF0D instructions-test) (list "break" 'break))

(setf (gethash #xFF10 instructions-test) (list "mfhi" 'mfhi))
(setf (gethash #xFF11 instructions-test) (list "mthi" 'mthi))
(setf (gethash #xFF12 instructions-test) (list "mflo" 'mflo))
(setf (gethash #xFF13 instructions-test) (list "mtlo" 'mtlo))

(setf (gethash #xFF18 instructions-test) (list "mult" 'mult))
(setf (gethash #xFF19 instructions-test) (list "multu" 'multu))
(setf (gethash #xFF1A instructions-test) (list "div" 'div))
(setf (gethash #xFF1B instructions-test) (list "divu" 'divu))

(setf (gethash #xFF20 instructions-test) (list "add" 'add))
(setf (gethash #xFF21 instructions-test) (list "addu" 'addu))
(setf (gethash #xFF22 instructions-test) (list "sub" 'sub))
(setf (gethash #xFF23 instructions-test) (list "subu" 'subu))
(setf (gethash #xFF24 instructions-test) (list "and" 'and))
(setf (gethash #xFF25 instructions-test) (list "or" 'or))
(setf (gethash #xFF26 instructions-test) (list "xor" 'xor))
(setf (gethash #xFF27 instructions-test) (list "nor" 'nor))

(setf (gethash #xFF2A instructions-test) (list "slt" 'slt))
(setf (gethash #xFF2B instructions-test) (list "sltu" 'sltu))

; TODO(Samantha): Add coprocessor specific opcodes like #xC0004

; TODO(Samantha): I think Alexandria has a function for this, consider just
; pulling it in as a dependency?
(defun hash-keys (hash-map)
  (loop for key being the hash-keys of hash-map collect key))

(defun check-missing ()
  (loop for opcode in (set-difference (hash-keys instructions-test) (hash-keys instructions)) do
    (format t "~A~%" (car (gethash opcode instructions-test)))))
