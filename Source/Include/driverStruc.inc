
;-------------------------------------------------------;
; This include file contains miscellaneous SCP/DOS      ;
; device driver related structures.                     ;
;-------------------------------------------------------;

struc drvHdr  ;Device Driver Header for character and block devices

    .nxtPtr resq 1  ;Pointer to the next driver header, -1 if at the end
    .attrib resw 1  ;Attribute Word
    .strPtr resq 1  ;Strategy Entry Pointer
    .intPtr resq 1  ;Interrupt Entry Pointer
    .drvNam resb 8  ;Driver name (Char) or Number of units byte (Block)

endstruc

%macro drvReqHdrM 0 ;Driver Request Header Macro

    .hdrlen resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .devptr resq 1  ;Device queue pointer field

%endmacro
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
    drvOUTTILBUSY   equ 16  ;Not implemented
    drvIOCTL        equ 19
    drvGETDRVMAP    equ 23
    drvSETDRVMAP    equ 24
    
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

;Drive structures
struc drvReqHdr
    drvReqHdrM
endstruc

struc initReqPkt  ;Init Request Packet

    drvReqHdrM   ;Common Driver Header Macro
    .numunt resb 1  ;Number of logical units (Block only, 0 for char)
    .endptr resq 1  ;Pointer to first free byte after driver
    .optptr resq 1  ;Pointer to the BPB array (block) or optional args (char)
    .drvnum resb 1  ;Drive number

endstruc

struc mediaCheckReqPkt ;Media Check Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    .medesc resb 1  ;DOS media descriptor
    .medret resb 1  ;Return byte (Has media been changed?)
    .desptr resq 1  ;Pointer to a valid volume id field

endstruc

struc bpbBuildReqPkt  ;Build BPB Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    .medesc resb 1  ;DOS media descriptor
    .bufptr resq 1  ;Transfer buffer
    .bpbptr resq 1  ;Pointer to the BPB

endstruc

struc ioReqPkt   ;IO Request Packet

    drvReqHdrM    ;Common Driver Header Macro
    .medesc resb 1  ;DOS media descriptor
    .bufptr resq 1  ;Transfer buffer
    .tfrlen resd 1  ;Number of Sectors/bytes to transfer
    .strtsc resq 1  ;Starting sector for transfer
    .desptr resq 1  ;Pointer to a valid volume id field if error

endstruc

struc nonDestInNoWaitReqPkt    ;Nondestructive Input No Wait Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro
    .retbyt resb 1  ;Byte read non destructively

endstruc

struc statusReqPkt   ;Status Request Packet

    drvReqHdrM    ;Common Driver Header Macro

endstruc

struc flushReqPkt ;Flush Request Packet, terminate all pending requests

    drvReqHdrM    ;Common Driver Header Macro
     
endstruc

struc openReqPkt ;Open Device Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
endstruc

struc closeReqPkt ;Close Device Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
endstruc

struc remMediaReqPkt ;Removeable Media Request Packet

    drvReqHdrM    ;Common Driver Header Macro
     
endstruc

struc ioctlReqPkt    ;Generic IOCTL Request Packet

    drvReqHdrM    ;Common Driver Header Macro  
    .majfun resb 1  ;Major function number
    .minfun resb 1  ;Minor function number
    .rsival resq 1  ;Contents of RSI
    .rdival resq 1  ;Contents of RDI
    .ctlptr resq 1  ;Pointer to Generic IOCTL Request Packet

endstruc

struc getDevReqPkt ;Get Logical Device Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro

endstruc

struc setDevReqPkt ;Set Logical Device Request Packet
    
    drvReqHdrM    ;Common Driver Header Macro

endstruc