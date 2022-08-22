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
%include "./Source/Utils/COMMAND/Source/cmdMain.asm"
%include "./Source/Utils/COMMAND/Source/cmdFunc.asm"
%include "./Source/Utils/COMMAND/Source/cmdUtils.asm"
%include "./Source/Utils/COMMAND/Source/int44h.asm"
%include "./Source/Utils/COMMAND/Data/cmdEnv.asm"
%include "./Source/Utils/COMMAND/Source/cmdLdr.asm"