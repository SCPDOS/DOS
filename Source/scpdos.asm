[map all ./Listings/SCPDOS/scpdos.map]
[DEFAULT REL]
BITS 64
;Defs and strucs
%include "./Source/Include/driverStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Debug/debSym.inc"
%include "./Source/Include/dosError.inc"
Segment .text align=1   ;Init code
%include "./Source/Sysinit/sysInit.asm"
Segment dSeg nobits align=1 start=0     ;BSS data segment
%include "./Source/BSS/dosSeg.asm"
Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 ;DOS main code seg
%include "./Source/Data/staticData.asm"
%include "./Source/Data/dispTbl.asm"
%include "./Source/Debug/debug.asm"
%include "./Source/Kernel/FAT/bufferFunctions.asm"
%include "./Source/Kernel/FAT/fat.asm"
%include "./Source/Kernel/CharDevs/charFunctions.asm"
%include "./Source/Kernel/CharDevs/dateTimeFunctions.asm"
%include "./Source/Kernel/FileIO/fcbFunctions.asm"
%include "./Source/Kernel/FileIO/handleFunctions.asm"
%include "./Source/Kernel/MemExec/memFunc.asm"
%include "./Source/Kernel/Local/Local.asm"
%include "./Source/Kernel/dosPrim.asm"
%include "./Source/Kernel/dosUtils.asm"
%include "./Source/Kernel/dosMain.asm"
%include "./Source/Misc/int44h.asm"
%include "./Source/Drivers/drvData.asm"
%include "./Source/Drivers/charDrv.asm"
%include "./Source/Drivers/diskDrv.asm"
Segment dynamicDataArea nobits valign=10h vfollows=resSeg
;Paragraph alignment
%include "./Source/BSS/dosDynaDataArea.asm"
