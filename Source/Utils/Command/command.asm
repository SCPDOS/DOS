;COMMAND.COM main file
[map all ./Source/Utils/COMMAND/Listings/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/driverStruc.inc"
%include "./Source/Include/fatStruc.inc"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Include/dosError.inc"
Segment .data align=1 
%include "./Source/Utils/COMMAND/Data/cmdData.asm"
Segment .bss align=1 follows=.data
%include "./Source/Utils/COMMAND/Data/cmdBSS.asm"
Segment .text align=1 follows=.data vfollows=.data
%define currSegVBase section.text.vstart
%include "./Source/Utils/COMMAND/Source/cmdMain.asm"
%include "./Source/Utils/COMMAND/Source/int44h.asm"
%include "./Source/Utils/COMMAND/Source/cmdLdr.asm"