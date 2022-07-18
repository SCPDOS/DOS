[map all ./Listings/SCPDOS/scpdos.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/driverStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Debug/debSym.inc"
%include "./Source/Include/dosError.inc"
Segment .text align=1   ;Init code
%define currSegVBase 0
%include "./Source/Sysinit/sysInit.asm"
Segment dSeg nobits align=1 start=0     ;BSS data segment
%include "./Source/BSS/dosSeg.asm"
Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 ;DOS main code seg
%define currSegVBase section.resSeg.vstart
%include "./Source/Data/staticData.asm"
%include "./Source/Data/dispTbl.asm"
%include "./Source/Debug/debug.asm"
%include "./Source/Kernel/FAT/buffun.asm"
%include "./Source/Kernel/FAT/fat.asm"
%include "./Source/Kernel/FAT/findfile.asm"
%include "./Source/Kernel/FAT/dirfun.asm"
%include "./Source/Kernel/FileIO/devio.asm"
%include "./Source/Kernel/FileIO/charFunc.asm"
%include "./Source/Kernel/FileIO/dtFunc.asm"
%include "./Source/Kernel/FileIO/fcbFunc.asm"
%include "./Source/Kernel/FileIO/hdlFunc.asm"
%include "./Source/Kernel/MemExec/memFunc.asm"
%include "./Source/Kernel/MemExec/execTerm.asm"
%include "./Source/Kernel/Local/Local.asm"
%include "./Source/Kernel/ctrlc.asm"
%include "./Source/Kernel/dosPrim.asm"
%include "./Source/Kernel/dosUtils.asm"
%include "./Source/Kernel/dosMain.asm"
%include "./Source/Kernel/multiplx.asm"
%include "./Source/Misc/int44h.asm"
%include "./Source/Drivers/drvData.asm"
%include "./Source/Drivers/charDrv.asm"
%include "./Source/Drivers/diskDrv.asm"
Segment dynamicDataArea nobits valign=10h vfollows=resSeg
;Paragraph alignment
%include "./Source/BSS/dosDynaDataArea.asm"
