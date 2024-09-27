[DEFAULT REL]
BITS 64

%include "./src/Include/dosMacro.mac"
%include "./src/Include/dosStruc.inc"

;=======================START OF OEMINIT MODULE======================
%include "./src/Oeminit/oem.inc"

Segment otext code private align=1 use64
;---------------------------
;   OEMINIT code segment
;---------------------------
%include "./src/Oeminit/oeminit.asm"

Segment odata data private align=1
;---------------------------
;   OEMINIT data segment
;---------------------------
;No OEM DATA
;========================END OF OEMINIT MODULE=======================