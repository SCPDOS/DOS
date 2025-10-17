[DEFAULT REL]
BITS 64

%include "./inc/dosMacro.mac"
%include "./inc/dosStruc.inc"

;=======================START OF OEMINIT MODULE======================
%include "./src/dos/Oeminit/oem.inc"

Segment otext code private align=1 use64
;---------------------------
;   OEMINIT code segment
;---------------------------
%include "./src/dos/Oeminit/oeminit.asm"

Segment odata data private align=1
;---------------------------
;   OEMINIT data segment
;---------------------------
;No OEM DATA
;========================END OF OEMINIT MODULE=======================