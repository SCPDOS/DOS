[DEFAULT REL]
BITS 64

%include "./src/Include/dosMacro.mac"
%include "./src/Include/drvStruc.inc"
%include "./src/Include/fatStruc.inc"
%include "./src/Include/fcbStruc.inc"
%include "./src/Include/dosStruc.inc"
%include "./src/Debug/debSym.inc"
%include "./src/Include/dosError.inc"
%include "./src/Include/dosExec.inc"

;========================START OF DOS MODULE=========================
%include "./src/Kernel/dos.inc"

Segment dBSS nobits align=1 start=0
;---------------------------
;   DOS BSS data segment
;---------------------------
%include "./src/Kernel/BSS/dosSeg.asm"
dSegLen equ ($-$$)

Segment dtext follows=sdata align=1 vfollows=dBSS valign=1 
;---------------------------
;       DOS segment
;---------------------------
;DOS main data/code seg. 
;No separation, as this is a single binary blob.
%define currSegVBase section.dtext.vstart

%include "./src/Kernel/Data/staticData.asm"
%include "./src/Kernel/Data/dispTbl.asm"
%if DEBUG
%include "./src/Kernel/Debug/debug.asm"
%endif
%include "./src/Kernel/FAT/buffun.asm"
%include "./src/Kernel/FAT/fat.asm"
%include "./src/Kernel/FAT/findfile.asm"
%include "./src/Kernel/FAT/dirfun.asm"
%include "./src/Kernel/FileIO/devio.asm"
%include "./src/Kernel/FileIO/charFunc.asm"
%include "./src/Kernel/FileIO/dtFunc.asm"
%include "./src/Kernel/FileIO/fcbFunc.asm"
%include "./src/Kernel/FileIO/hdlFunc.asm"
%include "./src/Kernel/FileIO/ioctl.asm"
%include "./src/Kernel/MemExec/memFunc.asm"
%include "./src/Kernel/MemExec/progMgmt.asm"
%include "./src/Kernel/MemExec/exec.asm"
%include "./src/Kernel/Local/local.asm"
%include "./src/Kernel/Error/ctrlc.asm"
%include "./src/Kernel/Core/dosPrim.asm"
%include "./src/Kernel/Core/dosUtils.asm"
%include "./src/Kernel/Core/dosMain.asm"
%include "./src/Kernel/Net/server.asm"
%include "./src/Kernel/Net/multiplx.asm"
%include "./src/Kernel/Net/share.asm"
dtextL  equ ($-$$)

;=========================END OF DOS MODULE==========================