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
Segment stext align=1 valign=1
;---------------------------
;   SYSINIT code segment
;---------------------------
;%define currSegVBase section.stext.vstart
%define currSegVBase sys$_start

%include "./src/Sysinit/sysinit.asm"
%include "./src/Sysinit/cfginit.asm"

Segment sdata follows=stext align=1 vfollows=stext valign=1
;---------------------------
;   SYSINIT data segment
;---------------------------
%include "./src/Sysinit/sysdat.asm"
;========================END OF SYSINIT MODULE=======================