;This file contains the main DOS data segment

    dosSegPtr   resq 1    ;Pointer to the data Segment itself x
    bootDrive   resb 1    ;The Int 33h device we booted from x
    numRemMSD   resb 1    ;Number of physical removable MSDs in system x
    numLRemDrives  resb 1 ;Number of logical removable drives in system x
    ;if numRemMSD = 1 then numLRemDrives = 2 and we have a single drive system
    charReqHdr  resb ioReqPkt_size  ;Character IO Request header x
    diskReqHdr  resb ioReqPkt_size  ;Disk Action Request header x
    ;The device driver header with space for the largest possible packet
    mcbChainPtr resq 1    ;Pointer to the MCB chain x
sysVarsPtr:
    dpbHeadPtr  resq 1    ;Pointer to the first DPB in the DPB chain x
    sftHeadPtr  resq 1    ;Pointer to the first SFT header in SFT chain
    clockPtr    resq 1    ;Pointer to the current active CLOCK$ device header x
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    conPtr      resq 1    ;Pointer to the current active CON device header  x
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec resw 1    ;Maximum number of bytes per sector (size of buffers)x
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array x
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]x
    sfcbHeadPtr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
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
    rdiErrorPtr resq 1  ;Saves RDI value of last error (could make this di)
    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    lastRetCode resw 1  ;Last return code returned by Int 41h/4Ch
    currentDrv  resb 1  ;Default drive
    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls
    verifyFlag  resb 1  ;If set, writes are replaces with write/verify
;SDA, needs to be replaced between processes
    xInt44hRSP  resq 1  ;RSP across an Int 44h call
;Only used on single remdrive systems, marks if drive A or B was last accessed
    singleDrv   resb 1  
;This is done to allow for DOS to give the user a change to swap devices

    Int44RetVal resb 1  ;Saves a copy of the Int 44 return value
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
;Time stuff
    CLOCKrecrd  resb 6  ;Clock driver record
    dayOfMonth  resb 1  ;01h - 1Fh (1 - 31)
    monthOfYear resb 1  ;01h - 0Ch (1 - 12)
    years       resb 1  ;00h - FFh (00 = 1980 - 128 = 2107)
    daysOffset  resw 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

;Stacks
    critStack   resq 165
    critStakTop resq 1
    IOStack     resq 199
    IOStakTop   resq 1
    DiskStack   resq 199
    DiskStakTop resq 1
    dSegLen     equ     $