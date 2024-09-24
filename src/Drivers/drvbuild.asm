[DEFAULT REL]
BITS 64

%include "./src/Include/dosMacro.mac"
%include "./src/Include/drvStruc.inc"
%include "./src/Include/fatStruc.inc"
%include "./src/Include/fcbStruc.inc"
%include "./src/Include/dosStruc.inc"
%include "./src/Include/dosError.inc"
%include "./src/Include/dosExec.inc"

;====================START OF OEM DRIVER MODULE======================
; This driver module needs to have no external linkage except to
; OEMINIT.
;====================================================================
%include "./src/Drivers/drv.inc"

Segment kDrvText code private align=1 use64
;---------------------------
;  OEM Driver code segment
;---------------------------
;All drivers are linked into the kDrvText segment
%include "./src/Drivers/drvHdrs.asm"
%include "./src/Drivers/charDrv.asm"
%include "./src/Drivers/diskDrv.asm"
%include "./src/Drivers/drvInits.asm"

Segment kDrvDat data private align=1
;---------------------------
;  OEM Driver data segment
;---------------------------

Segment kDrvBSS bss private align=1
;---------------------------
;  OEM Driver bss segment
;---------------------------
%include "./src/Drivers/drvBuf.asm"
    alignb 10h  ;Ensure paragraph alignment

;=====================END OF OEM DRIVER MODULE=======================