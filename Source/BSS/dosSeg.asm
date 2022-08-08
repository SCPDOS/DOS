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
;Above is the system stats
;Below is the DOS vars
    vConHdlOff  resq 1    ;Ptr into buff to the next char to process in hdl req
    ;   A value of 0 means no chars buffered.
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
;Additional internal variables
;Only used on single remdrive systems, marks if drive A or B was last accessed
    singleDrv   resb 1    ;Set if last drive accessed was drive B x
    numFiles    resb 1    ;FILES=5 default, max 255
    maxHndls    resw 1    ;Initially hardcoded 20, will be made changable later
    ;Share hook functions here
;Create SFT header and corresponding array of five default sft entries
    firstSftHeader  resb sfth_size
    firstSft    resb sft_size
    secondSft   resb sft_size
    thirdSft    resb sft_size
    fourthSft   resb sft_size
    fifthSft    resb sft_size

;Virtual CONsole Buffers
    vConCursPos resb 1     ;Keeps track for tabs stops (and var with 7)
    ;Only incremented when CON device runs vCon
vConBuf:    ;Proper buffer symbol
    vConCurCnt  resb 1     ;Current count of chars in vConBuffer
    vConBuffer  resb 128   ;General Buffer for vCon 256 bytes. 
    ;Only 128 bytes at a time if doing CON IO via handle
    vConInBuf   resb 128   ;vConsole buffer for reads ONLY
    bufpad      resb 1     ;Used to pad with LF

    printEcho   resb 1  ;If 0, no echo. Non-zero => Echo to PRN
    verifyFlag  resb 1  ;If set, writes are replaces with write/verify x
    switchChar  resb 1  ;Editable by 41h/37h. Set to / by default
    vConErr     resb 1  ;Inc on each char output call
    ;Is and-ed with 03h, checks for ^C on every fourth char output

    allocStrat  resb 1  ;Allocation strategy. First, Best or Last fit
;Server stuff. Default to all zeros (blank)
    shareFlag   resb 1  ;Sharing flag, set to 0 for now (future expansion)
    ;When share is loaded, this flag is set to -1 !!!!!
    serverCnt   resb 1  ;Increments on each 41h/5D01h call
    machineName resb 16 ;Machine name (Set via 41h/5D01h) (set to SPC)    
;Swappable Data Area
    critPtchTbl resq 4  ;Offsets from DosDataArea addr to the 4 funcs
                resb 1  ;Alignment byte
sda:    ;Start of Swappable Data Area, this bit can remain static
    critErrFlag resb 1  ;Critical error flag, set on entry to INT 44h x
    inDOS       resb 1  ;Inc on each DOS call, dec when leaving x
    errorDrv    resb 1  ;Drive on which error occured or FFh x
    errorLocus  resb 1  ;Where the error took place  
    errorExCde  resw 1  ;Extended Error Code
    errorAction resb 1  ;Suggested action for error  
    errorClass  resb 1  ;Error Class
    xInt44RDI   resq 1  ;Preserved rdi across a critical error
    currentDTA  resq 1  ;Address of the current DTA x
    currentPSP  resq 1  ;Address of current PSP x

    xInt43hRSP  resq 1  ;Saves RSP across an Int 43h call
    errorLevel  resw 1  ;Last return code returned by Int 41h/4Ch x
    ;Upper byte: 0=Normal, 1=Abort Occured, 2=CtrlC, 3=TSR 41h/31h
    ;Lower byte: User Specified
    currentDrv  resb 1  ;Default drive x
    breakFlag   resb 1  ;If set, check for CTRL+C on all DOS calls x
;SDA, needs to be replaced between processes
sdaMainSwap:
    oldRAX      resq 1  ;Store rax on entering Int41h or returning Int 43h
    serverPSP   resq 1  ;PSP of prog making server request, used by net & share
    machineNum  resw 1  ;for sharing/networking 00h = default number (us)
    firstMCB    resq 1  ;First fit MCB for request
    bestMCB     resq 1  ;Best fit MCB for request
    lastMCB     resq 1  ;Last fit MCB for request
    dirEntryNum resw 1  ;Offset into directory of entry we are looking for
    xInt44hRSP  resq 1  ;RSP across an Int 44h call
    Int44bitfld resb 1  ;Copies the bit field given to the Int 44h handler
    fileDirFlag resb 1  ;File/Directory flag. 0 = Dir, ¬0 = File
    Int44Fail   resb 1  ;Set if Int 44h returned fail

    oldoldRSP   resq 1  ;RSP at prev Int 41h entry if called from within Int 41h
    dosReturn   resq 1  ;Used as a var to return when juggling stack
    oldRSP      resq 1  ;RSP when entering Int 41h
    oldRBX      resq 1  ;Temp var to save value of rbx during an Int 41 call
    dirFlag     resb 1  ;Directory Flag. 0 => Search for Dir, 1 => for File
;The below flag tells DOS to print ^C in the termination function
    ctrlCExit   resb 1  ;-1 => CTRL+BREAK termination, 0 otherwise
    fcbSpaceOk  resb 1  ;If set, we allow embedded spaces in the filenames
;Time stuff
;Read the below two as a word
    dayOfMonth  resb 1  ;01h - 1Fh (1 - 31)
    monthOfYear resb 1  ;01h - 0Ch (1 - 12)
    years       resb 1  ;00h - 7Fh (00 = 1980 - 127 = 2107)
    daysOffset  resw 1  ;Days since 1-1-1980
    dayOfWeek   resb 1  ;0 = Sunday <-> 6 = Saturday

    vConDrvSwp  resb 1  ;Set if vCon controlled by a different driver to vConPtr
    int48Flag   resb 1  ;If set, Int 48h should be called, if clear no
    Int44Trans  resb 1  ;Set to -1 if Abort translated to Fail
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
    critReqHdr  resb ioReqPkt_size  ;Used for ^C detection!
    pspCopyFlg  resb 1  ;Set to -1 for child process PSP, 0 for simple PSP copy
;Swappable Buffers
    CLOCKrecrd  resb 6  ;Clock driver record
    ;We add an additional byte to save ah too
    singleIObyt resw 1  ;For single IO byte buffers
    buffer1     resb 128  ;Space for one path and file name
    buffer2     resb 128  ;Space for a second path and file name
    fname1Ptr   resq 1  ;Ptr to first filename argument
    fname2Ptr   resq 1  ;Ptr to second filename argument
    skipDisk    resb 1  ;Set => Read Disk, Clear => Skip checking on disk
;Misc bookkeeping flags and vars
    dosffblock  resb ffBlock_size   ;Internal searching block
    curDirCopy  resb fatDirEntry_size   ;Copy of directory being accessed
    fcbName     resb 11+1   ;11 chars for 8.3 ( w/o the dot) and terminating 0
    wcdFcbName  resb 11+1   ;Used to expand any wildcards in fcbName
    fileDirSect resq 1  ;File/Directory starting sector, for each level
    tmpCDS      resb cds_size   ;Temp CDS for Server calls that need tmp CDS
    searchAttr  resb 1  ;Directory Search attributes
    fileOpenMd  resb 1  ;Open mode (compat, r/w/rw?)
    fileFDflg   resb 1  ;01h = File Found!, 04h = File deleted!
    badNameRen  resb 1  ;Device name or File not found for rename
    rwFlag      resb 1  ;00h=Read, 01h=Write
    spliceFlag  resb 1  ;00 = Relative path, !0 = Full path
    dosInvoke   resb 1  ;0 = Invoked via Int 41h, -1 = Invoked via 41h/5D01h

    vConInsert  resb 1  ;Insert mode on 41/0ah (0 = not insert, !0 = insert)
    fileExist   resb 1  ;-1 if file in pathspec exists (create/open)
    parDirExist resb 1  ;-1 if parent directory for file exists (create/open)
    exitType    resb 1  ;Forms the upper byte of the errorlvl
    openCreate  resb 1  ;If open, set to 0, if Create set to -1
    delChar     resb 1  ;Char to replace first byte of deleted file's name
    workingDrv  resb 1  ;Working drive number
qPtr:       ;Stores working DPB and/or device driver (if r/w a char device)
workingDD:  ;Create a symbol for the working device driver too
    workingDPB  resq 1  ;Ptr to the DPB of the drive being accessed
    workingCDS  resq 1  ;Ptr to the CDS of the drive being accessed
;Below is the symbol for saving the oldSFTptr during a char func
vConAltSFTPtr: ;Alternate symbol for working SFT (used when CON is swapped)
    workingSFT  resq 1  ;Temporary SFT (may not be not current) ptr being used
    curHdlPtr   resq 1  ;Ptr to JFT handle entry in current PSP
    currentSFT  resq 1  ;Ptr to the SFT of the file being accessed
    currentNdx  resw 1  ;Used to access the current SFTNdx being opened/created
    currentHdl  resw 1  ;The current file handle is saved here
    currBuff    resq 1  ;Ptr to the Current Buffer (hdr) being accessed
;Temp vars, used when walking FAT or changing sectors, or reporting sector num
; and 32 byte offset into the sector for directory
    tempSect    resq 1  ;A scratch sector number
    entry       resw 1  ;32 byte offset into a sector or #fats sectors/fat
;***************************************************|
; Needs to be set up before any file access         |
; These vars keep track of file access properties   |
;   and must be used only for such purposes.        |
;***************************************************|
    currClustF  resd 1  ;Relative cluster in file being r/w to/from
    currClustD  resd 1  ;Current Disk Cluster being r/w to/from

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
    dirClustPar resd 1  ;Absolute disk cluster of the start of the parent dir
    dirClustA   resd 1  ;Absolute cluster number of current directory
    dirSect     resw 1  ;Sector of current directory cluster
    dirEntry    resd 1  ;32 byte offset in dir for file being searched for
;Error DPB 
    tmpDPBPtr   resq 1  ;A DPB for error/temporary  situations
    mediaByte   resb 1  ;Calls 1Bh and 1Ch return ptr to here
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
    sdaLen      equ     $ - sda 
    sdaMSLen    equ     $ - sda

;Additional variables NOT in the SDA
    bkupReqHdr  resb ioReqPkt_size  ;A backup header to allow copying to
    ;for saving the current header when quickly doing a second request

    lastDiskNum resb 1  ;Last drive that operated
    lastOpTime  resw 1  ;Packed Time of last successful disk operation
    ;Prevent toggling print if in the middle of reading an extended ASCII char
inExtASCII:
    noPrintTog  resb 1  ;00 = Toggle as usual, 01 = Prevent toggle
    keybTicks   resw 1  ;Counts the number of cycles spent in a kb loop.
    ;Every time this overflows, we read the clock and update the DOS internal
    ; copy of the date/time record
    dSegLen     equ     $