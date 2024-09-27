[DEFAULT REL]
BITS 64

%include "./src/Include/dosMacro.mac"
%include "./src/Include/drvStruc.inc"
%include "./src/Include/fatStruc.inc"
%include "./src/Include/fcbStruc.inc"
%include "./src/Include/dosStruc.inc"
%include "./src/Include/dosError.inc"
%include "./src/Include/dosExec.inc"

;=======================START OF SYSINIT MODULE======================
%include "./src/Sysinit/sys.inc"

;Segment stext follows=odata align=1 vfollows=odata valign=1
Segment stext code private align=1 use64
;---------------------------
;   SYSINIT code segment
;---------------------------
%include "./src/Sysinit/sysinit.asm"
%include "./src/Sysinit/cfginit.asm"

Segment sdata data private align=1
;---------------------------
;   SYSINIT data segment
;---------------------------
%include "./src/Sysinit/sysdat.asm"
;========================END OF SYSINIT MODULE=======================