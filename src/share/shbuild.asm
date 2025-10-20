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

[LIST -]
%include "./inc/dosMacro.mac"
%include "./inc/fatStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosStruc.inc"
%include "./inc/drvStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/shstruc.inc"
[LIST +]
;========================START OF SHARE MODULE=========================
EXTERN resLenParas
GLOBAL ep
;---------------------------
;   DOS data segment here
;---------------------------
;Included as an absolute 
;segment at address 0 to
;allow using the dos data 
;segment as a struc
;---------------------------
absolute 0x0
%include "./src/dos/Kernel/BSS/dosSeg.asm"

;---------------------------
;       Share data seg
;---------------------------
segment bss$r bss private align=16
%include "./src/share/data/shbss.asm"

;---------------------------
;       Share code seg
;---------------------------
segment .text code private align=16 use64
;Place this explicit .text section to calm nasm down
segment code$r code private align=16 use64
%include "./src/share/text/shmain.asm"
%include "./src/share/text/shutils.asm"

segment code$i code private align=16 use64
%include "./src/share/text/shinit.asm"
;=========================END OF SHARE MODULE==========================