[map all ./Listings/SCPDOS/scpdos.map]
[DEFAULT REL]
BITS 64
;Defs and strucs
%include "./Source/Include/driverStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Debug/debSym.inc"
Segment .text align=1   ;Init code
%include "./Source/Sysinit/sysInit.asm"
Segment dSeg nobits align=1 start=0     ;BSS data segment
%include "./Source/BSS/dosSeg.asm"
Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 ;DOS main code seg
%include "./Source/Data/staticData.asm"
%include "./Source/Debug/debug.asm"
%include "./Source/Kernel/bufferFunctions.asm"
%include "./Source/Kernel/fat.asm"
%include "./Source/Kernel/charFunctions.asm"
%include "./Source/Kernel/fcbFunctions.asm"
%include "./Source/Kernel/handleFunctions.asm"
%include "./Source/Kernel/memoryFunctions.asm"
%include "./Source/Kernel/dateTimeFunctions.asm"
%include "./Source/Kernel/localisation.asm"
%include "./Source/Kernel/dosKernel.asm"
%include "./Source/Misc/int44h.asm"
%include "./Source/Drivers/drvData.asm"
%include "./Source/Drivers/charDrv.asm"
%include "./Source/Drivers/diskDrv.asm"
Segment dynamicDataArea nobits valign=1 vfollows=resSeg
%include "./Source/BSS/dosDynaDataArea.asm"
