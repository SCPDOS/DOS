;--------------------------------
;       DATA FOR SYSINIT        :
;--------------------------------
strtmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
badCom  db "Bad or missing Command interpreter",0Ah,0Dh,"$"
conName db "CON",0
auxName db "AUX",0
prnName db "PRN",0

cfgspec db "CONFIG.SYS",0 ;ASCIIZ for CONFIG
cmdLine db "_:\COMMAND.COM",0   ;ASCIIZ FOR COMMAND.COM

cmdBlock:   ;Used also for overlay block
    istruc execProg
    at execProg.pEnv,       dq 0    ;Is set to point at the above line
    at execProg.pCmdLine,   dq 0    ;Points to just a 0Dh
    at execProg.pfcb1,      dq 0    ;Set to DOS's fcb 1 and 2
    at execProg.pfcb2,      dq 0
    iend
exceptData:
    dq i0
    dq i1
    dq i2
    dq i3
    dq i4
    dq i5
    dq i6
    dq i7
    dq i8
    dq i9
    dq i10
    dq i11
    dq i12
    dq i13
    dq i14
    dq i15
    dq i16
    dq i17
    dq i18
    dq i19
    dq i20
    dq i21

intData:
    dq terminateProcess ;Int 20h
    dq functionDispatch ;Int 21h
    dq OEMHALT          ;Int 22h, If sysinit terminates, halt system
    dq defaultIretq     ;Int 23h, ignore any CTRL+C during init
    dq dosDefCritErrHdlr    ;Int 24h, return fail, CF=CY, leading to OEMHALT
    dq absDiskRead      ;Int 25h
    dq absDiskWrite     ;Int 26h
    dq terminateRes     ;Int 27h
    dq defaultIretq     ;Int 28h
    dq defaultIretq     ;Int 29h
    dq defaultIretq     ;Int 2Ah
    dq defaultIretq     ;Int 2Bh
    dq defaultIretq     ;Int 2Ch
    dq defaultIretq     ;Int 2Dh
    dq defaultIretq     ;Int 2Eh
    dq multiplexHdlr    ;Int 2Fh, multiplex default handler
nData:
    dq 0    ;We link here to the head of the OEM driver chain
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data

localIDTpointer: ;Local IDT pointer
    .Limit  dw 0
    .Base   dq 0

;DOS Data given by OEM
FINALDOSPTR dq 0    ;Pointer to where dSeg should be loaded
MCBANCHOR   dq 0    ;Pointer to the Anchor MCB
FILES       db 0    ;Default number of FILES
BUFFERS     db 0    ;Default number of BUFFERS
DFLTDRIVE   db 0    ;Default drive number (0-25), this is the boot drive
LASTDRIVE   db 0    ;Default last drive number (0-25)
OEMBIOS     db 0    ;Set if to use IO.SYS or clear if to use SCPBIOS.SYS
OEMMEMPTR:  ;Used to save the allocated 64k block for OEMCALLBK
OEMDRVCHAIN dq 0    ;Pointer to the uninitialised device drivers
OEMPTR      dq 0    ;Pointer to store at biosPtr
OEMVERSION  dd 0    ;BIOS number, to be used by drivers for id-ing

initDrvBlk  db initReqPkt_size dup (0)  ;Used for making driver init reqs
tempPSP: ;Points to a 256 byte space that is set up appropriately
    istruc psp
    at psp.return,      db 0CDh, 20h
    at psp.allocSize,   dd 0, 0 ;Second 0 is for the reserved dword
    at psp.oldInt22h,   dq 0
    at psp.oldInt23h,   dq 0
    at psp.oldInt24h,   dq 0
    at psp.parentPtr,   dq 0
    at psp.jobFileTbl,  db 20 dup (0FFh)
    at psp.envPtr,      dq 0
    at psp.rspPtr,      dq 0
    at psp.jftSize,     dw 20 
    at psp.unixEntry,   db 0CDh, 21h, 0C3h
    at psp.prevPSP,     dq 0
    at psp.fcb1,        db 16 dup (0)
    at psp.fcb2,        db 20 dup (0)
    at psp.dta,         db 0, CR, 126 dup (0)   ;Dummy empty command line
    iend