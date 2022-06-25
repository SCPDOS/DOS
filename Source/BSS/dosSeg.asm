;This file contains the main DOS data segment
dosAPT: ;Additional Page tables
    resb dosAPTsize    ;60kb of space for the page tables
dosDataArea:
    dosSegPtr   resq 1    ;Pointer to the data Segment itself x
    biosUBase   resq 1    ;Ptr to the BIOS userbase
    bootDrive   resb 1    ;The Int 33h device we booted from x
    numRemDrv   resb 1    ;Number of physical removable MSDs in system x
    numFixDrv   resb 1    ;Number of physical fixed drives in system
    loProtMem   resd 1    ;Num bytes free in (lo) protected from userbase
    hiProtMem   resd 1    ;Num bytes in hi protec. arena (or 0 if no ISA hole)
    longMem     resq 1    ;Num bytes in long memory arena

;A request routed through the FCB or handle uses primReqHdr for its main IO.
;A secondary header is present to allow simultaneous echoing to console 
; without forcing to re-build the whole primary request block.
;Thus all disk io uses the primary and CharIO goes through the primary
; with secondary char output going through the secondary header
;(i.e the char input functions use the primary for main input and secondary 
; for output)
;ioReqPkt is the largest possible packet
    secdReqHdr  resb ioReqPkt_size  ;Secondary, Character IO Request header x
    primReqHdr  resb ioReqPkt_size  ;Primary Disk AND Char. IO Request header x
    vConUnread  resb 1    ;vCon: no unread data = 0, unread data != 0
    mcbChainPtr resq 1    ;Pointer to the MCB chain x
sysVarsPtr:
    dpbHeadPtr  resq 1    ;Pointer to the first DPB in the DPB chain x
    sftHeadPtr  resq 1    ;Pointer to the first SFT header in SFT chain
    clockPtr    resq 1    ;Pointer to the current active CLOCK$ device header x
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    vConPtr     resq 1    ;Ptr to the devdrv of the char dev controlling vCon x
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec resw 1    ;Maximum number of bytes per sector (size of buffers)x
    bufHeadPtr  resq 1    ;Pointer to the head of the disk buffer chain x
    cdsHeadPtr  resq 1    ;Pointer to the head of the CDS array x
    sfcbHeadPtr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    ;Old numLogicalDrives is now numPhysical volumes
    numPhysVol  resb 1    ;Number of physical volumes in the system x
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]x
    numBuffers  resb 1    ;Buffers=30 default
    numJoinDrv  resb 1    ;Number of Joined Drives
    nulDevHdr   resb drvHdr_size
;Create SFT header and corresponding array of five default sft entries
    firstSftHeader  resb sfth_size
    firstSft    resb sft_size
    secondSft   resb sft_size
    thirdSft    resb sft_size
    fourthSft   resb sft_size
    fifthSft    resb sft_size

;Virtual CONsole Buffers
    stdinBuf    resb 128   ;Buffer for Console Input buffer (read)
    stdoutBuf   resb 128   ;Buffer for Console Output buffer (write)
    bufpad      resb 3     ;Used to pad so can use stdout with 41h/0Ah
   
;Additional internal variables
    numFiles    resw 1    ;FILES=5 default
    maxHndls    resw 1    ;Initially hardcoded 20, will be made changable later

    switchChar  resb 1  ;Editable by 41h/37h. Set to / by default
    allocStrat  resb 1  ;Allocation strategy. First, Best or Last fit
;Server stuff. Default to all zeros (blank)
    serverCnt   resb 1  ;Increments on each 41h/5D01h call
    machineName resb 16 ;Machine name (Set via 41h/5D01h) (set to SPC)    
;Swappable Data Area
    critPtchTbl resq 4  ;Offsets from DosDataArea addr to the 4 funcs
                resb 1  ;Alignment byte
sda:    ;Start of Swappable Data Area, this bit can remain static
    oldRAX      resq 1  ;Store rax on entering Int41h or returning Int 43h
    sharePSP    resq 1  ;PSP of the share program
    machineNum  resw 1  ;for sharing/networking 00h = default number (us)
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
    Int44Error  resw 1  ;Saves Error code from request status word
    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    errorLevel  resw 1  ;Last return code returned by Int 41h/4Ch x

    currentDrv  resb 1  ;Default drive x
;Only used on single remdrive systems, marks if drive A or B was last accessed
    singleDrv   resb 1  ;Set if last drive accessed was drive B x

    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls x
    verifyFlag  resb 1  ;If set, writes are replaces with write/verify x
;SDA, needs to be replaced between processes
    firstMCB    resq 1  ;First fit MCB for request
    bestMCB     resq 1  ;Best fit MCB for request
    lastMCB     resq 1  ;Last fit MCB for request
    STDIOuse    resb 1  ;Set if STDIO is being used during current task
    xInt44RDI   resq 1  ;Preserved rdi across a critical error
    xInt44hRSP  resq 1  ;RSP across an Int 44h call
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    Int44Fail   resb 1  ;Counts the number of fails that have occured
    Int44Trans  resb 1  ;Set to -1 if Abort translated to Fail
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
    dosInvoke   resb 1  ;0= Int 41h, -1 = 41h/5D01h
    critExit    resb 1  ;-1 => CTRL+BREAK termination, 0 otherwise
;The above flag tells DOS to print ^C in the termination function

;Time stuff
    dayOfMonth  resb 1  ;01h - 1Fh (1 - 31)
    monthOfYear resb 1  ;01h - 0Ch (1 - 12)
    years       resb 1  ;00h - FFh (00 = 1980 - 128 = 2107)
    daysOffset  resw 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

;Swappable Buffers
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
qPtr:       ;Stores working DPB and/or device driver (if r/w a char device)
workingDD:  ;Create a symbol for the working device driver too
    workingDPB  resq 1  ;Ptr to the DPB of the drive being accessed
    workingCDS  resq 1  ;Ptr to the CDS of the drive being accessed
    workingSFT  resq 1  ;Temporary SFT (may not be not current) ptr
    tmpCDS      resb cds_size   ;Temp CDS for Server calls that need tmp CDS
    curJFTNum   resq 1  ;Ptr to JFT num in caller PSP of file being accessed
    currentSFT  resq 1  ;Ptr to the SFT of the file being accessed
    currentHdl  resw 1  ;The current file handle is saved here
    currBuff    resq 1  ;Ptr to the Current Buffer (hdr) being accessed
;Temp vars, used when walking FAT or changing sectors
    tempSect    resq 1  ;A scratch sector number
    entries     resw 1  ;FAT entries per FAT sector
;***************************************************|
; Needs to be set up before any file access         |
; These vars keep track of file access properties   |
;   and must be used only for such purposes.        |
;***************************************************|
    currClustF  resd 1  ;Relative cluster in file being r/w to/from
    currClustD  resd 1  ;Current Disk Cluster being r/w to/from

    clustFact   resb 1  ;NUMBER of sectors per cluster

    currSectF   resd 1  ;Current Sector in File being r/w to/from
    currSectC   resb 1  ;Current Sector in Cluster being r/w to/from
    currSectD   resq 1  ;Current absolute Sector number on Disk

    currByteS   resw 1  ;Current Byte in sector being r/w to/from
    currByteF   resd 1  ;Current Byte in file being r/w to/from
;***************************************************|
    lastClust   resd 1  ;Number of the last (rel) cluster of the file
    lastClustA  resd 1  ;Number of the last (abs) cluster of file on disk
    bytesAdded  resd 1  ;Number of bytes added to file (max 2Gb filesize!)
    tfrLen      resd 1  ;Number of bytes to transfer
    tfrCntr     resd 1  ;Number of bytes left to transfer
;Directory stuff
    dirClust    resd 1  ;Cluster number of current directory
    dirClustA   resd 1  ;Absolute cluster number of current directory
    dirSect     resb 1  ;Sector of current directory
    dirEntry    resb 1  ;32 byte offset in dir sect for file being searched for
;Error DPB 
    tmpDPBPtr   resq 1  ;A DPB for error/temporary  situations
    
;Stacks and scratch SFT
    critStack   resq 165
    critStakTop resq 1

    scratchSFT  resb sft_size   ;Used in FCB calls to emulate a SFT

    AuxStack    resq 199
    AuxStakTop  resq 1  ;Auxilliary stack (Char IO, INT 45h/46h etc)
    DiskStack   resq 199
    DiskStakTop resq 1

    diskChange  resb 1  ;-1 = disk has been changed!
    lookahead   resb 1  ;-1 => Lookahead on select Char function calls!  
    dSegLen     equ     $