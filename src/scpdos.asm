[map all ./lst/SCPDOS/scpdos.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./src/Include/dosMacro.mac"
%include "./src/Include/drvStruc.inc"
%include "./src/Include/fatStruc.inc"
%include "./src/Include/fcbStruc.inc"
%include "./src/Include/dosStruc.inc"
%include "./src/Debug/debSym.inc"
%include "./src/Include/dosError.inc"
%include "./src/Include/dosExec.inc"
Segment .text align=1   ;Init code
%define currSegVBase 0
%include "./src/Sysinit/oemreloc.asm"
%include "./src/Sysinit/sysinit.asm"
%include "./src/Sysinit/oeminit.asm"
Segment dSeg nobits align=1 start=0     ;BSS data segment
%include "./src/BSS/dosSeg.asm"
Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 ;DOS main code seg
%define currSegVBase section.resSeg.vstart
%include "./src/Data/staticData.asm"
%include "./src/Data/dispTbl.asm"
%if DEBUG
%include "./src/Debug/debug.asm"
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
%include "./src/Kernel/ctrlc.asm"
%include "./src/Kernel/dosPrim.asm"
%include "./src/Kernel/dosUtils.asm"
%include "./src/Kernel/dosMain.asm"
%include "./src/Kernel/Net/server.asm"
%include "./src/Kernel/Net/multiplx.asm"
%include "./src/Kernel/Net/share.asm"
;These driver files are to be written by an OEM.
%include "./src/Drivers/drvHdrs.asm"
%include "./src/Drivers/charDrv.asm"
%include "./src/Drivers/diskDrv.asm"
%include "./src/Drivers/drvInits.asm"
dosLen  equ ($-$$)  ;Get the length of the Segment
Segment drvbss follows=resSeg align=1 nobits
%include "./src/Drivers/drvBuf.asm"
    alignb 10h  ;Ensure paragraph alignment
dosEnd: ;Used to compute the size of resident DOS