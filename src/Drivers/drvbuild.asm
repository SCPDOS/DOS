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

;Segment kDrvText follows=dtext vfollows=dtext align=1 valign=1
Segment kDrvText align=1 valign=1
;---------------------------
;  OEM Driver code segment
;---------------------------
;All drivers are linked into the kDrvText segment
;%define currSegVBase section.kDrvText.vstart
%define currSegVBase 0

%include "./src/Drivers/drvHdrs.asm"
%include "./src/Drivers/charDrv.asm"
%include "./src/Drivers/diskDrv.asm"
%include "./src/Drivers/drvInits.asm"

Segment kDrvDat follows=kDrvText vfollows=kDrvText align=1 valign=1
;---------------------------
;  OEM Driver data segment
;---------------------------

Segment kDrvBSS bss follows=kDrvDat align=1 nobits
;---------------------------
;  OEM Driver bss segment
;---------------------------
%include "./src/Drivers/drvBuf.asm"
    alignb 10h  ;Ensure paragraph alignment

;=====================END OF OEM DRIVER MODULE=======================