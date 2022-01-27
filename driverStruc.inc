
;-------------------------------------------------------;
; This include file contains miscellaneous SCP/DOS      ;
; device driver related structures.                     ;
;-------------------------------------------------------;

drvHdr STRUC  ;Device Driver Header for character and block devices

    nxtPtr dq ?  ;Pointer to the next driver header, -1 if at the end
    atWord dw ?  ;Attribute Word
    strPtr dq ?  ;Strategy Entry Pointer
    intPtr dq ?  ;Interrupt Entry Pointer
    drvNam db 8 dup (?)  ;Driver name (Char) or Unit number byte (Block)

drvHdr ENDS

drvReqHdrM MACRO  ;Driver Request Header Macro

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    devptr dq ?  ;Device queue pointer field

ENDM

drvReqHdr   STRUC
    drvReqHdrM
drvReqHdr   ENDS

initReqPkt STRUC  ;Init Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    numunt db ?  ;Number of logical units (Block only, 0 for char)
    endptr dq ?  ;Pointer to first free byte after driver
    optptr dq ?  ;Pointer to the BPB array (block) or optional args (char)
    drvnum db ?  ;Drive number

initReqPkt ENDS

mediaCheckReqPkt STRUC ;Media Check Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    medesc db ?  ;DOS media descriptor
    medret db ?  ;Return byte (Has media been changed?)
    desptr dq ?  ;Pointer to a valid volume id field

mediaCheckReqPkt ENDS

bpbBuildReqPkt STRUC  ;Build BPB Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    medesc db ?  ;DOS media descriptor
    bufptr dq ?  ;Transfer buffer
    bpbptr dq ?  ;Pointer to the BPB

bpbBuildReqPkt ENDS

ioReqPkt STRUC   ;IO Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    medesc db ?  ;DOS media descriptor
    bufptr dq ?  ;Transfer buffer
    tfrlen dd ?  ;Number of Sectors/bytes to transfer
    strtsc dq ?  ;Starting sector for transfer
    desptr dq ?  ;Pointer to a valid volume id field if error

ioReqPkt ENDS

nonDestInNoWaitReqPkt STRUC    ;Nondestructive Input No Wait Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro
    retbyt db ?  ;Byte read non destructively

nonDestInNoWaitReqPkt ENDS

statusReqPkt STRUC   ;Status Request Packet

    drvReqHdrM    ;Common Driver Header Macro

statusReqPkt ENDS

flushReqPkt STRUC ;Flush Request Packet, terminate all pending requests

    drvReqHdrM    ;Common Driver Header Macro
     
flushReqPkt ENDS

openReqPkt STRUC ;Open Device Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
openReqPkt ENDS

closeReqPkt STRUC ;Close Device Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
closeReqPkt ENDS

remMediaReqPkt STRUC ;Removeable Media? Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
remMediaReqPkt ENDS

ioctlReqPkt STRUC    ;Generic IOCTL Request Packet

    drvReqHdrM    ;Common Driver Header Macro  
    majfun db ?  ;Major function number
    minfun db ?  ;Minor function number
    rsival dq ?  ;Contents of RSI
    rdival dq ?  ;Contents of RDI
    ctlptr dq ?  ;Pointer to Generic IOCTL Request Packet

ioctlReqPkt ENDS

getDevReqPkt STRUC ;Get Logical Device Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro
    getcmd db ?  ;Command code
    cmdsts dw ?  ;Command status word

getDevReqPkt ENDS

setDevReqPkt STRUC ;Set Logical Device Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro
    setcmd db ?  ;Command code
    cmdsts dw ?  ;Command status word

setDevReqPkt ENDS
