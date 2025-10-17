[DEFAULT REL]
BITS 64

%include "./inc/dosMacro.mac"
%include "./inc/drvStruc.inc"
%include "./inc/fatStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/dosExec.inc"

;=======================START OF SYSINIT MODULE======================
%include "./src/dos/Sysinit/sys.inc"

;Segment stext follows=odata align=1 vfollows=odata valign=1
Segment stext code private align=1 use64
;---------------------------
;   SYSINIT code segment
;---------------------------
%include "./src/dos/Sysinit/sysinit.asm"
%include "./src/dos/Sysinit/cfginit.asm"

Segment sdata data private align=1
;---------------------------
;   SYSINIT data segment
;---------------------------
%include "./src/dos/Sysinit/sysdat.asm"
;========================END OF SYSINIT MODULE=======================