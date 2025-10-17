[DEFAULT REL]
BITS 64

%include "./inc/dosMacro.mac"
%include "./inc/drvStruc.inc"
%include "./inc/fatStruc.inc"
%include "./inc/fcbStruc.inc"
%include "./inc/dosStruc.inc"
%include "./inc/dosError.inc"
%include "./inc/dosExec.inc"

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
%include "./src/Drivers/charDrv.asm"
%include "./src/Drivers/diskDrv.asm"
%include "./src/Drivers/drvInits.asm"

Segment kDrvDat data private align=1
;---------------------------
;  OEM Driver data segment
;---------------------------
%include "./src/Drivers/drvHdrs.asm"

Segment kDrvBSS bss private align=1
;---------------------------
;  OEM Driver bss segment
;---------------------------
%include "./src/Drivers/drvBuf.asm"
    alignb 10h  ;Ensure paragraph alignment

;=====================END OF OEM DRIVER MODULE=======================