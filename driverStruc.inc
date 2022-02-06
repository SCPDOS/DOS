
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
