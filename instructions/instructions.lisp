(in-package :psx-cpu)
(declaim (optimize (speed 3) (safety 1)))

; TODO(Samantha): Remove all of this to use new instruction macros.

; Regulars
; (setf (gethash #x01 instructions) (list "" #'b)-cond-z)
; (setf (gethash #x02 instructions) (list "j" #'j))
; (setf (gethash #x03 instructions) (list "jal" #'jal))
; (setf (gethash #x04 instructions) (list "beq" #'beq))
; (setf (gethash #x05 instructions) (list "bne" #'bne))
; (setf (gethash #x06 instructions) (list "blez" #'blez))
; (setf (gethash #x07 instructions) (list "bgtz" #'bgtz))
; (setf (gethash #x08 instructions) (list "addi" #'addi))
; (setf (gethash #x09 instructions) (list "addiu" #'addiu))
; (setf (gethash #x0A instructions) (list "slti" #'slti))
; (setf (gethash #x0B instructions) (list "sltiu" #'sltiu))
; (setf (gethash #x0C instructions) (list "andi" #'andi))
; (setf (gethash #x0D instructions) (list "ori" #'ori))
; (setf (gethash #x0E instructions) (list "xori" #'xori))
; (setf (gethash #x0F instructions) (list "lui" #'lui))
; (setf (gethash #x10 instructions) (list "cop0" #'cop0))
; (setf (gethash #x11 instructions) (list "cop1" #'cop1))
; (setf (gethash #x12 instructions) (list "cop2" #'cop2))
; (setf (gethash #x13 instructions) (list "cop3" #'cop3))
;
; (setf (gethash #x20 instructions) (list "lb" #'lb))
; (setf (gethash #x21 instructions) (list "lh" #'lh))
; (setf (gethash #x22 instructions) (list "lwl" #'lwl))
; (setf (gethash #x23 instructions) (list "lw" #'lw))
; (setf (gethash #x24 instructions) (list "lbu" #'lbu))
; (setf (gethash #x25 instructions) (list "lhu" #'lhu))
; (setf (gethash #x26 instructions) (list "lwr" #'lwr))
;
; (setf (gethash #x28 instructions) (list "sb" #'sb))
; (setf (gethash #x29 instructions) (list "sh" #'sh))
; (setf (gethash #x2A instructions) (list "swl" #'swl))
; (setf (gethash #x2B instructions) (list "sw" #'sw))
;
; (setf (gethash #x2E instructions) (list "swr" #'swr))
;
; (setf (gethash #x30 instructions) (list "lwc0" #'lwc0))
; (setf (gethash #x31 instructions) (list "lwc1" #'lwc1))
; (setf (gethash #x32 instructions) (list "lwc2" #'lwc2))
; (setf (gethash #x33 instructions) (list "lwc3" #'lwc3))
;
; (setf (gethash #x38 instructions) (list "swc0" #'swc0))
; (setf (gethash #x39 instructions) (list "swc1" #'swc1))
; (setf (gethash #x3A instructions) (list "swc2" #'swc2))
; (setf (gethash #x3B instructions) (list "swc3" #'swc3))
;
; ; Specials occur when bits [26, 31] = #x00, we encode as
; ; `(logior #xFF00 (ldb (byte 6 0) instruction-as-word))` to differentiate.
; (setf (gethash #xFF00 instructions) (list "sll" #'sll))
;
; (setf (gethash #xFF02 instructions) (list "srl" #'srl))
; (setf (gethash #xFF03 instructions) (list "sra" #'sra))
; (setf (gethash #xFF04 instructions) (list "sllv" #'sllv))
;
; (setf (gethash #xFF06 instructions) (list "srlv" #'srlv))
; (setf (gethash #xFF07 instructions) (list "srav" #'srav))
; (setf (gethash #xFF08 instructions) (list "jr" #'jr))
; (setf (gethash #xFF09 instructions) (list "jalr" #'jalr))
;
; (setf (gethash #xFF0C instructions) (list "syscall" #'syscall))
; (setf (gethash #xFF0D instructions) (list "break" #'break))
;
; (setf (gethash #xFF10 instructions) (list "mfhi" #'mfhi))
; (setf (gethash #xFF11 instructions) (list "mthi" #'mthi))
; (setf (gethash #xFF12 instructions) (list "mflo" #'mflo))
; (setf (gethash #xFF13 instructions) (list "mtlo" #'mtlo))
;
; (setf (gethash #xFF18 instructions) (list "mult" #'mult))
; (setf (gethash #xFF19 instructions) (list "multu" #'multu))
; (setf (gethash #xFF1A instructions) (list "div" #'div))
; (setf (gethash #xFF1B instructions) (list "divu" #'divu))
;
; (setf (gethash #xFF20 instructions) (list "add" #'add))
; (setf (gethash #xFF21 instructions) (list "addu" #'addu))
; (setf (gethash #xFF22 instructions) (list "sub" #'sub))
; (setf (gethash #xFF23 instructions) (list "subu" #'subu))
; (setf (gethash #xFF24 instructions) (list "and" #'and))
; (setf (gethash #xFF25 instructions) (list "or" #'or))
; (setf (gethash #xFF26 instructions) (list "xor" #'xor))
; (setf (gethash #xFF27 instructions) (list "nor" #'nor))
;
; (setf (gethash #xFF2A instructions) (list "slt" #'slt))
; (setf (gethash #xFF2B instructions) (list "sltu" #'sltu))


; From [Nocash PSX Specifications](http://problemkaputt.de/psx-spx.htm#cpuopcodeencoding)
; ; Primary opcode field (Bit 26..31)
; ;
; ;   00h=SPECIAL 08h=ADDI  10h=COP0 18h=N/A   20h=LB   28h=SB   30h=LWC0 38h=SWC0
; ;   01h=BcondZ  09h=ADDIU 11h=COP1 19h=N/A   21h=LH   29h=SH   31h=LWC1 39h=SWC1
; ;   02h=J       0Ah=SLTI  12h=COP2 1Ah=N/A   22h=LWL  2Ah=SWL  32h=LWC2 3Ah=SWC2
; ;   03h=JAL     0Bh=SLTIU 13h=COP3 1Bh=N/A   23h=LW   2Bh=SW   33h=LWC3 3Bh=SWC3
; ;   04h=BEQ     0Ch=ANDI  14h=N/A  1Ch=N/A   24h=LBU  2Ch=N/A  34h=N/A  3Ch=N/A
; ;   05h=BNE     0Dh=ORI   15h=N/A  1Dh=N/A   25h=LHU  2Dh=N/A  35h=N/A  3Dh=N/A
; ;   06h=BLEZ    0Eh=XORI  16h=N/A  1Eh=N/A   26h=LWR  2Eh=SWR  36h=N/A  3Eh=N/A
; ;   07h=BGTZ    0Fh=LUI   17h=N/A  1Fh=N/A   27h=N/A  2Fh=N/A  37h=N/A  3Fh=N/A
;
;
; ; Secondary opcode field (Bit 0..5) (when Primary opcode = 00h)
; ;
; ;   00h=SLL   08h=JR      10h=MFHI 18h=MULT  20h=ADD  28h=N/A  30h=N/A  38h=N/A
; ;   01h=N/A   09h=JALR    11h=MTHI 19h=MULTU 21h=ADDU 29h=N/A  31h=N/A  39h=N/A
; ;   02h=SRL   0Ah=N/A     12h=MFLO 1Ah=DIV   22h=SUB  2Ah=SLT  32h=N/A  3Ah=N/A
; ;   03h=SRA   0Bh=N/A     13h=MTLO 1Bh=DIVU  23h=SUBU 2Bh=SLTU 33h=N/A  3Bh=N/A
; ;   04h=SLLV  0Ch=SYSCALL 14h=N/A  1Ch=N/A   24h=AND  2Ch=N/A  34h=N/A  3Ch=N/A
; ;   05h=N/A   0Dh=BREAK   15h=N/A  1Dh=N/A   25h=OR   2Dh=N/A  35h=N/A  3Dh=N/A
; ;   06h=SRLV  0Eh=N/A     16h=N/A  1Eh=N/A   26h=XOR  2Eh=N/A  36h=N/A  3Eh=N/A
; ;   07h=SRAV  0Fh=N/A     17h=N/A  1Fh=N/A   27h=NOR  2Fh=N/A  37h=N/A  3Fh=N/A
