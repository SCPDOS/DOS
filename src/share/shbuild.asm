;Share! Handle file sharing in this module here.
;This is a kernel module with the key difference that it is not linked 
; into the main kernel image. Instead, we produce a standalone .EXE file
; which hooks itself into DOS if the user runs the program.
;We run as a TSR with two switches:
;/F:xxxxxxx specifies the amount of bytes to allocate for MFT space in bytes.
; Max value: 1048576 bytes (1Mb).
; Default: 2048 bytes (2Kb).
;/L:xxxx specifies the number of concurrent file locks allowed.
; Max value: 9999
; Default: 20

[DEFAULT REL]
BITS 64

%include "./inc/dosMacro.mac"
%include "./inc/fatStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/shstruc.inc"

;========================START OF SHARE MODULE=========================

Segment dosSeg bss private align=1
;---------------------------
;   DOS data segment here
;---------------------------
%include "./src/dos/Kernel/BSS/dosSeg.asm"

Segment .bss bss private align=1 
;---------------------------
;       Share data seg
;---------------------------
%include "./src/share/data/shbss.asm"


Segment .text code private align=1 use64
;---------------------------
;       Share code seg
;---------------------------
%include "./src/share/text/shmain.asm"
%include "./src/share/text/shutils.asm"

Segment .init code private align=1 use64
%include "./src/share/text/shinit.asm"
;=========================END OF SHARE MODULE==========================