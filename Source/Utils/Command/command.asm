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
%include "./Source/Include/dosExec.inc"
Segment .text align=1   ;Init code
%define currSegVBase 0
%include "./Source/Utils/COMMAND/Source/cmdMain.asm"
%include "./Source/Utils/COMMAND/Source/int44h.asm"