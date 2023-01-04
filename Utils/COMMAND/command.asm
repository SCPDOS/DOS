;COMMAND.COM main file
[map all ./Utils/COMMAND/Listings/command.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./Source/Include/dosMacro.mac"
%include "./Source/Include/dosStruc.inc"
%include "./Source/Include/fcbStruc.inc"
%include "./Source/Include/dosError.inc"
%include "./Utils/COMMAND/Data/cmdEqu.asm"
Segment .data align=1 
%include "./Utils/COMMAND/Data/cmdData.asm"
%include "./Utils/COMMAND/Data/cmdTable.asm"
%include "./Utils/COMMAND/Data/cmdMsg.asm"
%include "./Utils/COMMAND/Source/cmdMain.asm"
%include "./Utils/COMMAND/Source/cmdFunc.asm"
%include "./Utils/COMMAND/Source/cmdUtils.asm"
%include "./Utils/COMMAND/Source/int44h.asm"
%include "./Utils/COMMAND/Data/cmdEnv.asm"
%include "./Utils/COMMAND/Source/cmdLdr.asm"