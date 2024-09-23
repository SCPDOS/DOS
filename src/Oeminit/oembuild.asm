[DEFAULT REL]
BITS 64

%include "./src/Include/dosMacro.mac"
%include "./src/Include/dosStruc.inc"

;=======================START OF OEMINIT MODULE======================
%include "./src/Oeminit/oem.inc"

Segment otext align=1 valign=1
;---------------------------
;   OEMINIT code segment
;---------------------------

%include "./src/Oeminit/oeminit.asm"

Segment odata follows=otext align=1 vfollows=otext valign=1
;---------------------------
;   OEMINIT data segment
;---------------------------
;No OEM DATA
;========================END OF OEMINIT MODULE=======================