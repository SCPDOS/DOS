;This file contains the main DOS data segment

Segment dSeg nobits align=1 
    dosSegPtr   resq 1    ;Pointer to the data Segment itself
    bootDrive   resb 1    ;The Int 33h device we booted from
    charReqHdr  resb ioReqPkt_size  ;Character IO Request header
    diskReqHdr  resb ioReqPkt_size  ;Disk Action Request header
    ;The device driver header with space for the largest possible packet
    sysVarsPtr  resq 1    ;Pointer to dpbHeadPtr, head of Sys Vars struc below
    mcbChainPtr resq 1    ;Pointer to the MCB chain
    dpbHeadPtr  resq 1    ;Pointer to the first DPB in the DPB chain
    sftHeadPtr  resq 1    ;Pointer to the first SFT header in SFT chain
    clockPtr    resq 1    ;Pointer to the current active CLOCK$ device header
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    conPtr      resq 1    ;Pointer to the current active CON device header 
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec resw 1    ;Maximum number of bytes per sector (size of buffers)
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array
    sfcbHeadPTr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    numMSDdrv   resb 1    ;Number of mass storage devices detected in system
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]
    numJoinDrv  resb 1    ;Number of Joined Drives
    nulDevHdr   resb drvHdr_size

;Start of Swappable Data Area, this bit can remain static
    critErrFlag resb 1  ;Critical error flag, set on entry to INT 44h
    inDOS       resb 1  ;Inc on each DOS call, dec when leaving
    errorDrv    resb 1  ;Drive on which error occured or FFh
    errorLocus  resb 1  ;Where the error took place  
    errorExt    resw 1  ;Extended Error Code
    errorAction resb 1  ;Suggested action for error  
    errorClass  resb 1  ;Error Class

    currentDTA  resq 1  ;Address of the current DTA
    currentPSP  resq 1  ;Address of current PSP
    rdiErrorPtr resq 1  ;Saves RDI value of last error
    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    lastRetCode resw 1  ;Last return code returned by Int 41h/4Ch
    currentDrv  resb 1  ;Default, last accessed drive
    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls
;SDA, needs to be replaced between processes
    xInt44hRSP  resq 1  ;RSP across an Int 44h call

    Int44RetVal resb 1  ;Saves a copy of the Int 44 return value
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
;Time stuff
    CLOCKrecrd  resb 6  ;Clock driver record
    dayOfMonth  resb 1  ;1 - 31 BCD
    monthOfYear resb 1  ;1 - 12 BCD
    years       resw 1  ;0000 - 9999 BCD
    yearsOffset resw 1  ;Current Year - 1980
    daysOffset  resd 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

;Stacks
    critStack   resq 165
    critStakTop resq 1
    IOStack     resq 199
    IOStakTop   resq 1
    DiskStack   resq 199
    DiskStakTop resq 1
    dSegLen     equ     $