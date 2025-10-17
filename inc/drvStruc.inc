
;-------------------------------------------------------;
; This include file contains miscellaneous SCP/DOS      ;
; device driver related structures.                     ;
;-------------------------------------------------------;

struc drvHdr  ;Device Driver Header for character and block devices

    .nxtPtr dq ?  ;Pointer to the next driver header, -1 if at the end
    .attrib dw ?  ;Attribute Word
    .strPtr dq ?  ;Strategy Entry Pointer
    .intPtr dq ?  ;Interrupt Entry Pointer
    .drvUnt:        ;Number of units pointer
    .drvNam db 8 dup (?)  ;Driver name (Char) or Number of units byte (Block)

endstruc

;Command Code equates
    drvINIT         equ 0
    drvMEDCHK       equ 1
    drvBUILDBPB     equ 2
    drvIOCTLREAD    equ 3
    drvREAD         equ 4
    drvNONDESTREAD  equ 5
    drvINSTATUS     equ 6
    drvFLUSHINBUF   equ 7
    drvWRITE        equ 8
    drvWRITEVERIFY  equ 9
    drvOUTSTATUS    equ 10
    drvFLUSHOUTBUF  equ 11
    drvIOCTLWRITE   equ 12
    drvOPEN         equ 13
    drvCLOSE        equ 14
    drvREMMEDCHECK  equ 15
    drvOUTTILBUSY   equ 16
    drvIOCTL        equ 19
    drvGETDRVMAP    equ 23
    drvSETDRVMAP    equ 24
    drvMAXCMD       equ 24
;Driver Error equates 
    drvWPErr        equ 0   ;Write Protect Violation
    drvBadUnit      equ 1   ;Unknown unit
    drvNotReady     equ 2   ;Device not ready
    drvBadCmd       equ 3   ;Unknown command
    drvCRCErr       equ 4   ;CRC Error
    drvBadDrvReq    equ 5   ;Bad drive request structure length
    drvBadSeek      equ 6   ;Seek error
    drvBadMed       equ 7   ;Unknown media
    drvSecNotFnd    equ 8   ;Sector not found
    drvNoPaper      equ 9   ;Printer out of paper
    drvWriteFault   equ 10  ;Write Fault
    drvReadFault    equ 11  ;Read Fault
    drvGenFault     equ 12  ;General Fault
    drvBadDskChnge  equ 15  ;Invalid disk change

;Driver status word status bit meanings
    drvErrStatus    equ 8000h  
    drvDonStatus    equ 0100h
    drvBsyStatus    equ 0200h

;Driver Attribute Word bit meanings
    devDrvConIn     equ 1h  ;If set, device is CON input
    devDrvConOut    equ 2h  ;If set, device is CON output
    devDrvNulDev    equ 4h  ;If set, device is NULL device
    devDrvClockDev  equ 8h  ;If set, device is CLOCK device
    devDrvFastOut   equ 10h ;If set, device supports Int 29h
    devDrvIOCTL     equ 40h ;If set, IOCTL and GET/SET suported
    devDrvIOCTLQ    equ 80h ;If set, IOCTL Query supported (21h/4410h, 4411h)
    devDrvOpClRem   equ 800h ;If set, Open, Close and Rem media supported
    devDrvNotIBM    equ 2000h ;If set, media byte is meaningful for disk comps
    devDrvOutToBsy  equ 2000h ;If set, char dev supports output until busy
    devDrvIOCTLCtl  equ 4000h ;If set, IOCTL control strings supported
    devDrvChar      equ 8000h ;If set, Char device
;Bit 0 1=Console input
;    1 1=Console output (character devices)
;    2 1=NUL device
;    3 1=CLOCK device
;    4 1=Int 29 output (CON)
;    6 1=Generic IOCTL and GET/SET logical drive supported (13,17,18) (3.2+)
;    7 1=Query IOCTL function supported (19) (5.0+)
;   11 1=Open/close/removable media supported (3.0+)
;   13 1=Output until busy supported (character devices) (3.0+)
;      1=Media descriptor in FAT required (block devices)
;   14 1=IOCTL read and write control strings supported
;   15 0=Block device
;      1=Character device

;Driver structures

%macro drvReqPktM 0 ;Driver Request Header Macro

    .hdrlen db ?  ;Length of the request header
    .unitnm db ?  ;Unit number, meaningless for character devs
    .cmdcde db ?  ;Command code
    .status dw ?  ;Status word
    .devptr dq ?  ;Device queue pointer field, reserved for future use

%endmacro

struc drvReqPkt
    drvReqPktM
endstruc

struc initReqPkt  ;Init Request Packet

    drvReqPktM   ;Common Driver Header Macro
    .numunt db ?  ;Number of logical units (Block only, 0 for char)
    .endptr dq ?  ;Pointer to first free byte after driver
    .optptr dq ?  ;Ptr to BPB array (blkdev) or cmdline args
    .drvnum db ?  ;First Drive number

endstruc

struc mediaCheckReqPkt ;Media Check Request Packet

    drvReqPktM    ;Common Driver Header Macro
    .medesc db ?  ;DOS media descriptor
    .medret db ?  ;Return byte (Has media been changed?)
    .desptr dq ?  ;Pointer to a valid volume id field

endstruc

struc bpbBuildReqPkt  ;Build BPB Request Packet

    drvReqPktM    ;Common Driver Header Macro
    .medesc db ?  ;DOS media descriptor
    .bufptr dq ?  ;Transfer buffer
    .bpbptr dq ?  ;Pointer to the BPB

endstruc

struc ioReqPkt   ;IO Request Packet

    drvReqPktM    ;Common Driver Header Macro
    .medesc db ?  ;DOS media descriptor
    .bufptr dq ?  ;Transfer buffer
    .tfrlen dd ?  ;Number of Sectors/bytes to transfer
    .strtsc dq ?  ;Starting sector for transfer
    .desptr dq ?  ;Pointer to a valid volume id field if error

endstruc

struc ndInNoWaitPkt    ;Nondestructive Input No Wait Request Packet
    
    drvReqPktM    ;Common Driver Header Macro
    .retbyt db ?  ;Byte read non destructively

endstruc

struc statusReqPkt   ;Status Request Packet

    drvReqPktM    ;Common Driver Header Macro

endstruc

struc flushReqPkt ;Flush Request Packet, terminate all pending requests

    drvReqPktM    ;Common Driver Header Macro
     
endstruc

struc openReqPkt ;Open Device Request Packet

    drvReqPktM    ;Common Driver Header Macro
     
endstruc

struc closeReqPkt ;Close Device Request Packet

    drvReqPktM    ;Common Driver Header Macro
     
endstruc

struc remMediaReqPkt ;Removeable Media Request Packet

    drvReqPktM    ;Common Driver Header Macro
     
endstruc

struc outTilBusyPkt
    drvReqPktM    ;Common Driver Header Macro
    .pXfrBuffer dq ?    ;Pointer to the transfer buffer
    .wXfrCount  dw ?    ;Byte count to xfr. Returns bytes xfred.
endstruc

struc ioctlReqPkt    ;Generic IOCTL Request Packet

    drvReqPktM    ;Common Driver Header Macro  
    .majfun db ?  ;Major function number
    .minfun db ?  ;Minor function number
    .rsival dq ?  ;Contents of RSI
    .rdival dq ?  ;Contents of RDI
    .ctlptr dq ?  ;Pointer to Generic IOCTL Request Packet

endstruc

struc getDevReqPkt ;Get Logical Device Request Packet
    
    drvReqPktM    ;Common Driver Header Macro

endstruc

struc setDevReqPkt ;Set Logical Device Request Packet
    
    drvReqPktM    ;Common Driver Header Macro

endstruc
;IOCTL structs

struc lbaParamsBlock
    .bSize          db ?
    .bSpecFuncs     db ?    ;0 = Update drive info from medium and return that
                            ;1 = Get current saved drive info w/o updating
                            ;2 = Get physical parameters of the medium
                            ;Only meaningful on get. Set must have this as 0
    .wDevFlgs       dw ?    ;Only bits 0 and 1 are xmitted/read
    .wFSType        dw ?    ;If one, it is a FAT type. Only valid on getlba
                            ; If one, can use CHS functions with returned vals
    .wRes           dw ?    ;Reserved word
    .qSectorSize    dq ?    ;Only the lower word is valid here
    .qNumSectors    dq ?    ;Only the lower dword is valid here
;The below is the absolute start sector of the partition. It is the 
; partition at which to find the BPB.
    .qStartSector   dq ?    ;Only the lower dword is valid here.
endstruc

%macro lbIOCmnM 0   ;IOCTL LBA request common portion macro
    .size           db ?
    .numSectors     db ?  ;# sectors to act on. Returns sectors acted on
    .res            db 6 dup (?)
    .startSector    dq ?  ;Sector to start action on
%endmacro

struc lbaIOBlock
    lbIOCmnM
    .xferBuffer     dq ?  ;Transfer buffer for data
endstruc

struc lbaFormatBlock
    lbIOCmnM
endstruc

struc chsParamsBlock
    .bSpecFuncs db ?    ;
    .bDevType   db ?    ;5 if fixed, 7 otherwise
    .wDevFlgs   dw ?    ;Only bits 0 and 1 are xmitted/read
    .wNumCyl    dw ?
    .bMedTyp    db ?    ;Perma 0 for us, meaningless. Reserved.
    .deviceBPB  db 53 dup (?)   ;Full length with reserved bytes of BPB32
    .TrackLayout:
endstruc

;Used for get and set params. Refer to DOS docs for meaning of bit.
specFuncBPB equ 1<<0    ;Does BPB stuff
;Only used in setparams requests. Ignored for getparams 
specFuncTrk equ 1<<1    ;Set if just track layout cpy. Clear if set all.
specFuncSec equ 1<<2    ;Set if all sectors samze size. Clear if not.


struc chsIOBlock
    .bSpecFuncs     db ?
    .wStartHead     dw ?
    .wStartCyl      dw ?    ;Zero based.
    .wStartSector   dw ?    ;Zero based.
    .wNumSectors    dw ?
    .pXferBuffer    dq ?
endstruc

struc chsFormatBlock
    .bSpecFuncs db ?
    .wStartHead dw ?
    .wStartCyl  dw ?        ;Zero based.
endstruc

struc accFlgBlk
    .bSpecFuncs db ?    ;Must be 0
    .bAccMode   db ?    ;Set if access allowed. Clear if not.
endstruc

struc idParamBlk
    .wCallLvl   dw ?    ;Should be zero
    .volId      dd ?    ;Volume Id
    .volLab     db 11 dup (?)   ;Volume label string
    .filSysType db 8 dup (?)    ;File system type string
endstruc