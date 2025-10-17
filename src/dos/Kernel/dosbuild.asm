[DEFAULT REL]
BITS 64

%include "./inc/dosMacro.mac"
%include "./inc/drvStruc.inc"
%include "./inc/fatStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/dosExec.inc"
%include "./src/dos/Kernel/Debug/debSym.inc"    ;Always define debug symbols!

;========================START OF DOS MODULE=========================
%include "./src/dos/Kernel/dos.inc"

Segment dBSS bss private align=1
;---------------------------
;   DOS BSS data segment
;---------------------------
%include "./src/dos/Kernel/BSS/dosSeg.asm"
dSegLen equ ($-$$)

Segment dtext code private align=1 use64
;---------------------------
;       DOS segment
;---------------------------
;DOS main data/code seg. 
;No separation, as this is a single binary blob.
%include "./src/dos/Kernel/Data/staticData.asm"
%include "./src/dos/Kernel/Data/dispTbl.asm"
%if DEBUG
%include "./src/dos/Kernel/Debug/debug.asm" ;Only include if debug symbols on!
%endif
%include "./src/dos/Kernel/FAT/buffun.asm"
%include "./src/dos/Kernel/FAT/fat.asm"
%include "./src/dos/Kernel/FAT/findfile.asm"
%include "./src/dos/Kernel/FAT/dirfun.asm"
%include "./src/dos/Kernel/FileIO/devio.asm"
%include "./src/dos/Kernel/FileIO/charFunc.asm"
%include "./src/dos/Kernel/FileIO/dtFunc.asm"
%include "./src/dos/Kernel/FileIO/fcbFunc.asm"
%include "./src/dos/Kernel/FileIO/hdlFunc.asm"
%include "./src/dos/Kernel/FileIO/ioctl.asm"
%include "./src/dos/Kernel/MemExec/memFunc.asm"
%include "./src/dos/Kernel/MemExec/progMgmt.asm"
%include "./src/dos/Kernel/MemExec/exec.asm"
%include "./src/dos/Kernel/Local/local.asm"
%include "./src/dos/Kernel/Error/harderr.asm"
%include "./src/dos/Kernel/Error/ctrlc.asm"
%include "./src/dos/Kernel/Error/execpt.asm"
%include "./src/dos/Kernel/Core/dosPrim.asm"
%include "./src/dos/Kernel/Core/dosUtils.asm"
%include "./src/dos/Kernel/Core/dosMain.asm"
%include "./src/dos/Kernel/Net/server.asm"
%include "./src/dos/Kernel/Net/multiplx.asm"
%include "./src/dos/Kernel/Net/share.asm"

;=========================END OF DOS MODULE==========================