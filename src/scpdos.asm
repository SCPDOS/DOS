[map all ./lst/SCPDOS/scpdos.map]
[DEFAULT REL]
BITS 64
;Defs, strucs and macros
%include "./src/Include/dosMacro.mac"
%include "./src/Include/drvStruc.inc"
%include "./src/Include/fatStruc.inc"
%include "./src/Include/fcbStruc.inc"
%include "./src/Include/dosStruc.inc"
%include "./src/Debug/debSym.inc"
%include "./src/Include/dosError.inc"
%include "./src/Include/dosExec.inc"
Segment otext align=1   
;OEMINIT code segment
%include "./src/Oeminit/oeminit.asm"
Segment odata follows=otext align=1 vfollows=otext valign=1
;OEMINIT data segment
Segment stext follows=odata align=1 vfollows=odata valign=1
;SYSINIT code segment
%define currSegVBase section.stext.vstart
%include "./src/Sysinit/sysinit.asm"
%include "./src/Sysinit/cfginit.asm"
Segment sdata follows=stext align=1 vfollows=stext valign=1
;SYSINIT data segment
%include "./src/Sysinit/sysdat.asm" 
Segment dBSS nobits align=1 start=0
;DOS BSS data segment
%include "./src/BSS/dosSeg.asm"
Segment dtext follows=sdata align=1 vfollows=dBSS valign=1 
;DOS main data/code seg. No separation, as this is a single binary blob
%define currSegVBase section.dtext.vstart
%include "./src/Data/staticData.asm"
%include "./src/Data/dispTbl.asm"
%if DEBUG
%include "./src/Debug/debug.asm"
%endif
%include "./src/Kernel/FAT/buffun.asm"
%include "./src/Kernel/FAT/fat.asm"
%include "./src/Kernel/FAT/findfile.asm"
%include "./src/Kernel/FAT/dirfun.asm"
%include "./src/Kernel/FileIO/devio.asm"
%include "./src/Kernel/FileIO/charFunc.asm"
%include "./src/Kernel/FileIO/dtFunc.asm"
%include "./src/Kernel/FileIO/fcbFunc.asm"
%include "./src/Kernel/FileIO/hdlFunc.asm"
%include "./src/Kernel/FileIO/ioctl.asm"
%include "./src/Kernel/MemExec/memFunc.asm"
%include "./src/Kernel/MemExec/progMgmt.asm"
%include "./src/Kernel/MemExec/exec.asm"
%include "./src/Kernel/Local/local.asm"
%include "./src/Kernel/ctrlc.asm"
%include "./src/Kernel/dosPrim.asm"
%include "./src/Kernel/dosUtils.asm"
%include "./src/Kernel/dosMain.asm"
%include "./src/Kernel/Net/server.asm"
%include "./src/Kernel/Net/multiplx.asm"
%include "./src/Kernel/Net/share.asm"
dtextL  equ ($-$$)
Segment kDrvText follows=dtext vfollows=dtext align=1 valign=1
;All drivers are linked into the kDrvText segment
%define currSegVBase section.kDrvText.vstart
%include "./src/Drivers/drvHdrs.asm"
%include "./src/Drivers/charDrv.asm"
%include "./src/Drivers/diskDrv.asm"
%include "./src/Drivers/drvInits.asm"
kDrvTextL  equ ($-$$)
Segment kDrvDat follows=kDrvText vfollows=kDrvText align=1 valign=1
kDrvDatL  equ ($-$$)
Segment kDrvBSS follows=kDrvDat align=1 nobits
%include "./src/Drivers/drvBuf.asm"
    alignb 10h  ;Ensure paragraph alignment
dosEnd: ;Used to compute the size of resident DOS
dosLen equ kDrvDatL + kDrvTextL + dtextL