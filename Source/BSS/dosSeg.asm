;This file contains the main DOS data segment
dosAPT: ;Additional Page tables
    resb dosAPTsize    ;60kb of space for the page tables
dosDataArea:
    dosSegPtr   resq 1    ;Pointer to the data Segment itself x
    biosUBase   resq 1    ;Ptr to the BIOS userbase
    bootDrive   resb 1    ;The Int 33h device we booted from x
    numRemDrv   resb 1    ;Number of physical removable MSDs in system x
    numFixDrv   resb 1    ;Number of physical fixed drives in system
    numLogDrv   resb 1    ;Number of logical drives in system x
    loProtMem   resd 1    ;Num bytes free in (lo) protected from userbase
    hiProtMem   resd 1    ;Num bytes in hi protec. arena (or 0 if no ISA hole)
    longMem     resq 1    ;Num bytes in long memory arena
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
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain x
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array x
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]x
    numBuffers  resb 1    ;Buffers=30 default
    numFiles    resw 1    ;FILES=5 default
    maxHndls    resw 1    ;Initially hardcoded 20, will be made changable later
    sfcbHeadPtr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    numJoinDrv  resb 1    ;Number of Joined Drives
    nulDevHdr   resb drvHdr_size

    critPtchTbl resq 4  ;Offsets from DosDataArea addr to the 4 funcs
                resb 1  ;Alignment byte
sda:    ;Start of Swappable Data Area, this bit can remain static
    oldRAX      resq 1  ;Store rax on entering Int41h or returning Int 43h
    critErrFlag resb 1  ;Critical error flag, set on entry to INT 44h x
    inDOS       resb 1  ;Inc on each DOS call, dec when leaving x
    errorDrv    resb 1  ;Drive on which error occured or FFh x
    errorLocus  resb 1  ;Where the error took place  
    errorExCde  resw 1  ;Extended Error Code
    errorAction resb 1  ;Suggested action for error  
    errorClass  resb 1  ;Error Class

    dosReturn   resq 1  ;Used as a var to return when juggling stack

    currentDTA  resq 1  ;Address of the current DTA x
    currentPSP  resq 1  ;Address of current PSP x
    rdiErrorPtr resq 1  ;Saves RDI value of last error (could make this di)
    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    errorLevel  resw 1  ;Last return code returned by Int 41h/4Ch x
    allocStrat  resb 1  ;Allocation strategy. First, Best or Last fit
    currentDrv  resb 1  ;Default drive x
;Only used on single remdrive systems, marks if drive A or B was last accessed
    singleDrv   resb 1  ;Set if last drive accessed was drive B x
;This is done to allow for DOS to give the user a change to swap devices
    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls x
    verifyFlag  resb 1  ;If set, writes are replaces with write/verify x
;SDA, needs to be replaced between processes
    firstMCB    resq 1  ;First fit MCB for request
    bestMCB     resq 1  ;Best fit MCB for request
    lastMCB     resq 1  ;Last fit MCB for request
    xInt44hRSP  resq 1  ;RSP across an Int 44h call
    Int44RetVal resb 1  ;Saves a copy of the Int 44 return value
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
    dosInvoke   resb 1  ;FIXED 0, any other value fails calls (-1 = server invoke)
    critExit    resb 1  ;-1 => CTRL+BREAK termination, 0 otherwise
;The above flag tells DOS to print ^C in the termination function

;Time stuff
    dayOfMonth  resb 1  ;01h - 1Fh (1 - 31)
    monthOfYear resb 1  ;01h - 0Ch (1 - 12)
    years       resb 1  ;00h - FFh (00 = 1980 - 128 = 2107)
    daysOffset  resw 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

;Buffers
    buffer1     resb 128  ;Space for one path and file name
    buffer2     resb 128  ;Space for a second path and file name
    CLOCKrecrd  resb 6  ;Clock driver record
    singleIObyt resb 1  ;For single IO byte buffers
;Misc bookkeeping flags and vars
    ;secClusConv resb 1  ;For networking, do we convert sector to cluster?
    rwFlag      resb 1  ;00h=Read, 01h=Write
    fileFDflg   resb 1  ;01h = File Found!, 04h = File deleted!
    fileOpenMd  resb 1  ;Open mode (compat, r/w/rw?)
    typePSPcopy resb 1  ;00=Simple copy, -1=Make Child process
    spliceFlag  resb 1  ;01 = file name and directory name together

    workingDrv  resb 1  ;Working drive number
    workingDPB  resq 1  ;Ptr to the DPB of the drive being accessed
    workingCDS  resq 1  ;Ptr to the CDS of the drive being accessed
    curDrvCDS   resb cds_size   ;Working cp of CDS of drv being accessed
    currentJFT  resq 1  ;Ptr to JFT num in caller PSP of file being accessed
    currentSFT  resq 1  ;Ptr to the SFT of the file being accessed
    currentHdl  resw 1  ;The current file handle is saved here
    currBuff    resq 1  ;Ptr to the Current Buffer (hdr) being accessed
;Temp vars, used when walking FAT or changing sectors, in the event of failure
    
;Needs to be set up before any file access  |
    currClust   resd 1  ;Relative cluster in file being r/w to/from
    currClustA  resd 1  ;Current Cluster (abs) on disk being r/w to/from
    clustFact   resb 1  ;NUMBER of sectors per cluster
    currSect    resb 1  ;Current Sector in Cluster being r/w to/from
    currSectA   resq 1  ;Current absolute Sector number on Disk
    currByte    resw 1  ;Current Byte in sector being r/w to/from
    currByteA   resd 1  ;Current Byte in file being r/w to/from
;*****************************************  |
    lastClust   resd 1  ;Number of the last (rel) cluster of the file
    lastClustA  resd 1  ;Number of the last (abs) cluster of file on disk
    bytesAdded  resd 1  ;Number of bytes added to file (max 2Gb filesize!)
;Directory stuff
    dirClust    resd 1  ;Cluster number of current directory
    dirClustA   resd 1  ;Absolute cluster number of current directory
    dirSect     resb 1  ;Sector of current directory
    dirEntry    resb 1  ;32 byte offset in dir sect for file being searched for

    
;Stacks and scratch SFT
    critStack   resq 165
    critStakTop resq 1

    scratchSFT  resb sft_size

    IOStack     resq 199
    IOStakTop   resq 1
    DiskStack   resq 199
    DiskStakTop resq 1
    diskChange  resb 1  ;-1 = disk has been changed!
    dSegLen     equ     $