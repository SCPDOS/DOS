[map all scpdos.map]
[DEFAULT REL]
BITS 64
;Defs and strucs
%include "driverStruc.inc"
%include "fatStruc.inc"
%include "dosStruc.inc"
Segment .text align=1   ;Init code
%include "sysInit.asm"
Segment dSeg nobits align=1 start=0     ;BSS data segment
%include "dosSeg.asm"
Segment resSeg follows=.text align=1 vfollows=dSeg valign=1 ;DOS main code seg
%include "staticData.asm"
%include "fat.asm"
%include "dosKernel.asm"
%include "int44h.asm"
%include "dosDrivers.asm" 
Segment dynamicDataArea nobits valign=1 vfollows=resSeg
%include "dosDynaDataArea.asm"
