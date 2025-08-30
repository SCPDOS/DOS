;This file contains the main DOS data segment
    anchorMcb   db mcb_size dup (?) ;This is space for the anchor MCB
dosDataArea:    ;The returned pointer points to the variables w/o anchor MCB
    bootDrive   db ?    ;The logical drive we booted from
    biosVers    dd ?    ;Indicates BIOS type. Each OEM picks a number
    biosPtr     dq ?    ;For saving a data ptr to be used by BIOS/drivers
;Above is the system stats
;Below is the DOS vars, DO NOT TOUCH FROM validNetNam TO NUMJOINDRV
;Both below variables can be edited with Int 21h AX=440Bh
    validNetNam dw ?    ;Flag if machinename valid, deflt no=0
    shareCount  dw ?    ;Share Retry Count, number of repeats before fail.
    shareDelay  dw ?    ;Share Delay, in multiples of ms. (TEMP, just loop)
                dq ?    ;Unused ptr for future, current disk buffer
    vConHdlOff  dq ?    ;Ptr into buff to the next char to process in hdl req
    ;   A value of 0 means no chars buffered.
    mcbChainPtr dq ?    ;Pointer to the MCB chain x
sysVarsPtr:
    dpbHeadPtr  dq ?    ;Pointer to the first DPB in the DPB chain x
    sftHeadPtr  dq ?    ;Pointer to the first SFT header in SFT chain
    clockPtr    dq ?    ;Pointer to the current active CLOCK$ device header x
    ;                    The last driver loaded with the CLOCK$ bit[3] set 
    vConPtr     dq ?    ;Ptr to the devdrv of the char dev controlling vCon x
    ;                    The last driver loaded with the STDIN bit[0] set
    maxBytesSec dw ?    ;Maximum number of bytes per sector (size of buffers)x
    bufHeadPtr  dq ?    ;Pointer to the head of the disk buffer chain x
    cdsHeadPtr  dq ?    ;Pointer to the head of the CDS array x
    fcbsHeadPtr dq ?    ;Pointer to the head of the System FCB chain
    numSafeSFCB dw ?    ;Number of protected FCBs (y in FCBS=x,y)
    numPhysVol  db ?    ;Number of physical volumes in the system x
    lastdrvNum  db ?    ;Value of LASTDRIVE (default = 5) [Size of CDS array]x
    numBuffers  db ?    ;Buffers=30 default
    nulDevHdr   db drvHdr_size dup (?)
    numJoinDrv  db ?    ;Number of Joined Drives
;Additional internal variables
    numFiles    db ?    ;FILES=5 default, max 255
    ;DOSMGR hook functions and variable here
    ;All DOSMGR hooks are 8 byte pointers and have been introduced to allow
    ; an external application to install itself as a multitasker into the
    ; DOS kernel. DOS's behaviour changes accordingly when a multitasker 
    ; is installed. It is recommended that a multitasker NOT be installed
    ; when a file sharing broker is not installed but that is up to the 
    ; implementer to decide.  
    ;Three function hooks are provided. NOTE, all functions must preserve
    ; ALL registers used.
    ;
    ;launchTask:
    ;This allows for a multitasker to install its handling routine for 
    ; launching tasks. Note this is called after all setup
    ; for the EXE has been done except for setting the PSP.
    ; If the mode bSuFunc = 4, then we have we have the following:
    ;       ecx = mode of termination
    ;           = 00 -> Upon terminating, leave task in Zombie mode
    ;                   awaiting for a task to read it's return code
    ;           = 01 -> Upon terminating, discard all resources allocated
    ;                   to the task.
    ;           > 01 -> Error code, unknown function (01h).
    ;This function must return to the caller via DOS with CF=CY if an error 
    ;   and eax = Error code or CF=NC if all ok. DOS will then return to
    ;   the parent task, with the background task hopefully scheduled to run
    ;   in the DOSMGR.
    ;Either before or on initial run, DOSMGR must set currentDTA in a bgTasks'
    ; SDA to psp+80h. This can be done in launchBgTask.
    ;
    ;terminateTask:
    ;This allows for a multitasker to install its handling routine for
    ; cleaning up resources allocated to a task. 
    ;
    ;Specific function definitions:
    ;
    ;If we enter
    ;   Input:  bx = FCB drive statuses
    ;           ecx = Termination mode setting
    ;           rsi = RSP value to start with
    ;           rbp = execFrame. Use this to get parentPSP data et al.
    ;           qword [rbp - execFrame.pProgEP] = RIP value to launch from
    ;   Output: CF=NC -> Proceed with launch of bg task
    ;           CF=CY -> Error exit, errorcode in eax
dosMgrHooks:
    dosMgrPresent   db ?    ;Clear if no mgr, else set to -1
    launchTask      dq ?    ;Registers a new task, with specifics based on bSubfunc
    terminateTask   dq ?    ;Called to tell the MGR that this task is ending

    ;DLL Manager hook functions here
    ;All DLLMGR hooks are 8 byte pointers and are new to the DOS kernel.
    ;They allow for the installation of a DLL manager program, which hooks
    ; these pointers, to point to their own subroutines in the DLLMGR prog.
    ;These hooks are called from within EXEC, only for PE type executables
    ; and from within EXIT. 
    ;
    ;In EXEC mode, modes 0 and 1 create a PSP for the task and thus the 
    ; task is self standing and has a PSP as a Unique ID. 
    ;If mode 3, this is an overlay EXE. This means it is not it's own task and 
    ; is an extension of the parent task. In such a case, the DLL Manager must 
    ; look at where the overlay is to be loaded (execFrame.pProgBase), and if 
    ; there is already an overlay there, to remove it's functions from the 
    ; registery, replacing them with the new overlay's functions. All overlay
    ; exports must be flagged as belonging to the parent task PSP so that
    ; on EXIT, they can be removed from the registry. 
    ;
    ;In EXIT, the PSP of the ending task must be taken
    ; into consideration, as if the PSP isn't registered then the task ending
    ; is a .COM file or an .EXE with no exports.
    ;Furthermore, if register fails (due to memory or namespace constraints),
    ; it must return CF=CY.
dllHooks:
    registerDLL     dq ?  ;Entered with rbp = execFrame
    unloadDLLHook   dq ?  ;

    ;Share hook functions here
    ;All share hooks now take 8 bytes rather than 4 bytes as before
    ;Thus ALL offsets from SFT header increase by 4 bytes and each entry
    ; is a QWORD entry. Please adjust SHARE.EXE access as necessary.
shareHooks:
    markerShare         dq ?  ;Marker Share hook
    openShare           dq ?  ;Share called on open. 
    closeShare          dq ?  ;Share called on close.
    closeCompShare      dq ?  ;Share to close all files for a machine.
    closeTaskShare      dq ?  ;Share to close all files for a task.
    closeNameShare      dq ?  ;Share to close file by name.
    lockFileShare       dq ?  ;Share to lock file region.
    unlockFileShare     dq ?  ;Share to unlock file region.
    checkFileLockShare  dq ?  ;Share to check file region locked.
    openFileListShare   dq ?  ;Share to get open file list entry.
    updateFCBfromSFTShr dq ?  ;Share to update FCB from the SFT.    UNUSED
    fstClstOfFCBShare   dq ?  ;Share to get first cluster of FCB.   UNUSED
    closeDupFileShare   dq ?  ;Share to close file if dup for proc.
    closeNewHdlShare    dq ?  ;Share to close hdls of rec opened file.
    updateDirShare      dq ?  ;Share to update dir info in SFT. 

;Create SFT header and corresponding array of five default sft entries
    firstSftHeader  db sfth_size dup (?)
    firstSft        db sft_size dup (?)
    secondSft       db sft_size dup (?)
    thirdSft        db sft_size dup (?)
    fourthSft       db sft_size dup (?)
    fifthSft        db sft_size dup (?)

;Virtual CONsole Buffers
    vConCursPos db ?     ;Keeps track for tabs stops (and var with 7)
    ;Only incremented when CON device runs vCon
vConBuf:    ;Proper buffer symbol
    vConCurCnt  db ?     ;Current count of chars in vConBuffer
    vConBuffer  db 128 dup (?)   ;General Buffer for vCon 256 bytes. 
    ;Only 128 bytes at a time if doing CON IO via handle
    vConInBuf   db 128 dup (?)   ;vConsole buffer for reads ONLY
                db ?     ;Padding Buffer!

    printEcho   db ?  ;If 0, no echo. Non-zero => Echo to PRN
    verifyFlag  db ?  ;If set, writes are replaces with write/verify x
    switchChar  db ?  ;Editable by 21h/37h. Set to / by default
    vConErr     db ?  ;Inc on each char output call
    ;Is and-ed with 03h, checks for ^C on every fourth char output

    allocStrat  db ?  ;Allocation strategy. First, Best or Last fit
;Server stuff. Default to all zeros (blank)
    shareFlag   db ?  ;Sharing flag, set to 0 for now (future expansion)
    ;When share is loaded, this flag is set to -1 !!!!!
    serverCnt   db ?  ;Increments on each 21h/5E01h call
    machineName db 16 dup (?) ;Machine name (Set via 21h/5E01h) (set to SPC)    
;Swappable Data Area
    critPtchTbl dq 4 dup (?)  ;Offsets from DosDataArea addr to the 4 funcs
                db ?  ;Alignment byte
sda:    ;Start of Swappable Data Area, this bit can remain static
    critErrFlag db ?  ;Critical error flag, set on entry to Int 24h x
    inDOS       db ?  ;Inc on each DOS call, dec when leaving x
    errorDrv    db ?  ;Drive on which error occured or FFh x
    errorLocus  db ?  ;Where the error took place  
    errorExCde  dw ?  ;Extended Error Code
    errorAction db ?  ;Suggested action for error  
    errorClass  db ?  ;Error Class
    errorVolLbl dq ?    ;Sets a ptr to the volume label of the error disk
    currentDTA  dq ?  ;Address of the current DTA x
    currentPSP  dq ?  ;Address of current PSP x

    xInt23hRSP  dq ?  ;Saves RSP across an Int 23h call
    errorLevel  dw ?  ;Last return code returned by Int 21h/4Ch x
    ;Upper byte: 0=Normal, 1=Abort Occured, 2=CtrlC, 3=TSR 21h/31h
    ;Lower byte: User Specified
    currentDrv  db ?  ;Default drive x
    breakFlag   db ?  ;If set, check for CTRL+C on all DOS calls x
;SDA, needs to be replaced between processes
sdaDOSSwap:
    oldRAX      dq ?  ;Store rax on entering Int21h or returning Int 23h
    serverPSP   dq ?  ;PSP of prog making server request, used by net & share
    machineNum  dw ?  ;for sharing/networking 00h = default number (us)
    firstMCB    dq ?  ;First fit MCB for request
    bestMCB     dq ?  ;Best fit MCB for request
    lastMCB     dq ?  ;Last fit MCB for request
    dirEntryNum dw ?  ;Offset into directory of entry we are looking for
    volIdFlag   db ?   ;If set, we are searching for a volume ID
    xInt24hRSP  dq ?  ;RSP across an Int 24h call
    Int24bitfld db ?  ;Copies the bit field given to the Int 24h handler
    fileDirFlag db ?  ;File/Directory flag. 0 = Dir, ¬0 = File
    Int24Fail   db ?  ;Set if Int 24h returned fail

    oldoldRSP   dq ?  ;RSP at prev Int 21h entry if called from within Int 21h
    dosReturn   dq ?  ;Used as a var to return when juggling stack
    oldRSP      dq ?  ;RSP when entering Int 21h
    oldRBX      dq ?  ;Temp var to save value of rbx during an Int 21 call
    dirFlag     db ?  ;Directory Flag. 0 => Search for Dir, 1 => for File
;The below flag tells DOS to print ^C in the termination function
    ctrlCExit   db ?  ;-1 => CTRL+BREAK termination, 0 otherwise
    fcbSpaceOk  db ?  ;If set, we allow embedded spaces in the filenames
;Time stuff
;Read the below two as a word
    dayOfMonth  db ?  ;01h - 1Fh (1 - 31)
    monthOfYear db ?  ;01h - 0Ch (1 - 12)
    years       db ?  ;00h - 7Fh (00 = 1980 - 127 = 2107)
    daysOffset  dw ?  ;Days since 1-1-1980
    dayOfWeek   db ?  ;0 = Sunday <-> 6 = Saturday

    vConDrvSwp  db ?  ;Set if vCon controlled by a different driver to vConPtr
    int28Flag   db ?  ;If set, Int 28h should be called, if clear no
    procExiting db ?  ;Set to -1 if in process termination
;A request routed through the FCB or handle uses primReqPkt for its main IO.
;A secondary header is present to allow simultaneous echoing to console 
; without forcing to re-build the whole primary request block.
;Thus all disk io uses the primary and CharIO goes through the primary
; with secondary char output going through the secondary header
;(i.e the char input functions use the primary for main input and secondary 
; for output)
;ioReqPkt is the largest possible packet
    secdReqPkt  db ioReqPkt_size dup (?) ;Secondary, Char IO Reqhdr
    primReqPkt  db ioReqPkt_size dup (?) ;Main Drv Reqhdr 
altRet: ;Accessed as a qword
    critReqPkt  db ioReqPkt_size dup (?)  ;Used for ^C detection!
    pspCopyFlg  db ?  ;Set to -1 for child process PSP, 0 for simple PSP copy
;Swappable Buffers
    CLOCKrecrd  db 6 dup (?)  ;Clock driver record
    ;We add an additional byte to save ah too
    singleIObyt dw ?  ;For single IO byte buffers
extErrByteBuf:  ;Used by DOS execpt hdlr to build strings. Immediate abort!
exeHdrSpace:    ;This needs 112 bytes in EXEC only, buffer is free for use!
    buffer1     db 2*MAX_FSPEC dup (?)  ;Space for max expanded MAX_FSPEC
sectHdr:        ;This needs 20 bytes in EXEC only
    buffer2     db 2*MAX_FSPEC dup (?) ;Space for a second path
    fname1Ptr   dq ?  ;Ptr to first filename argument
    fname2Ptr   dq ?  ;Ptr to second filename argument
    skipDisk    db ?  ;Set => Read Disk, Clear => Skip checking on disk
;Misc bookkeeping flags and vars
    dosffblock  db ffBlock_size dup (?)  ;FF block (fullsize unlike DOS)
    curDirCopy  db fatDirEntry_size dup (?)  ;Dir copy
    tmpCDS      db cds_size dup (?)  ;For server calls that need a tmp CDS
;These two are used to expand filenames into FCB format. The extra char 
; is used to store the terminator of the portion (either a pathsep or null)
    fcbName     db MAX_NAME_FCBZ dup (?)
    wcdFcbName  db MAX_NAME_FCBZ dup (?)  ;Expands wildcards for rename
    fileDirSect dq ?  ;File/Directory starting sector, for each level
    volIncmpFCB db ?  ;Set to -1 if the volume uses FAT32 (or all incompat FS)
    extFCBAttr  db ?  ;Extended FCB file attribute
    extFCBFlag  db ?  ;Set to -1 if Extended FCB
    searchAttr  db ?  ;Directory Search attributes
    fileOpenMd  db ?  ;Open mode (compat, r/w/rw?)
    fileFDflg   db ?  ;01h = File Found!, 04h = File deleted!
    badNameRen  db ?  ;Device name or File not found for rename
    rwFlag      db ?  ;00h=Read, 1=Write, read/write/share error reporting
    spliceFlag  db ?  ;00 = Relative path, !0 = Full path
    dosInvoke   db ?  ;0 = Invoked via Int 21h, -1 = Invoked via 21h/5D01h

    vConInsert  db ?  ;Insert mode on 21/0ah (0 = not insert, !0 = insert)
    fileExist   db ?  ;-1 if file in pathspec exists (create/open)
    parDirExist db ?  ;-1 if parent directory for file exists (create/open)
    exitType    db ?  ;Forms the upper byte of the errorlvl
    openCreate  db ?  ;If open, set to 0, if Create set to -1
;Set to E5h for renaming and deletion. Can be set to 0 if *.* chosen
; to speed up the deletion but we don't use this (yet) as we would not 
; be releasing the FAT sectors of the remaining entries in the directory.
    delChar     db ?
    workingDrv  db ?  ;Working drive number, 0 based, from DPB
qPtr:       ;Stores working DPB and/or device driver (if r/w a char device)
workingDD:  ;Create a symbol for the working device driver too
    workingDPB  dq ?  ;Ptr to the DPB of the drive being accessed
    workingCDS  dq ?  ;Ptr to the CDS of the drive being accessed
    workingFCB  dq ?  ;Ptr to the caller FCB for FCB function
;Below is the symbol for saving the oldSFTptr during a char func
vConAltSFTPtr: ;Alternate symbol for working SFT (used when CON is swapped)
    workingSFT  dq ?  ;Temporary SFT (may not be not current) ptr being used
    curHdlPtr   dq ?  ;Ptr to JFT handle entry in current PSP
    currentSFT  dq ?  ;Ptr to the SFT of the file being accessed
    currentNdx  dw ?  ;Used to access the current SFTNdx being opened/created
    currentHdl  dw ?  ;The current file handle is saved here
    currBuff    dq ?  ;Ptr to the Current Buffer (hdr) being accessed
;Temp vars, used when walking FAT or changing sectors, or reporting sector num
; and 32 byte offset into the sector for directory
    tempSect    dq ?  ;A scratch sector number
pathLen:    ;Used to store the length of a path string for removal strcmp
    entry       dw ?  ;32 byte offset into a sector or #fats sectors/fat
;***************************************************|
; Needs to be set up before any file access         |
; These vars keep track of file access properties   |
;   and must be used only for such purposes.        |
;***************************************************|
    currClustF  dd ?  ;Relative cluster in file being r/w to/from
    currClustD  dd ?  ;Current Disk Cluster being r/w to/from

    currSectF   dd ?  ;Current Sector in File being r/w to/from
    currSectC   db ?  ;Current Sector in Cluster being r/w to/from
    currSectD   dq ?  ;Current absolute Sector number on Disk

    currByteS   dw ?  ;Current Byte in sector being r/w to/from
    currByteF   dd ?  ;Current Byte in file being r/w to/from
;***************************************************|
    lastClust   dd ?  ;Number of the last (rel) cluster of the file
    lastClustA  dd ?  ;Number of the last (abs) cluster of file on disk
    tfrLen      dd ?  ;Number of bytes to transfer
    tfrCntr     dd ?  ;Number of bytes left to transfer
;Directory stuff
    dirClustPar dd ?  ;Absolute disk cluster of the start of the parent dir
    dirClustA   dd ?  ;Absolute cluster number of current directory
    dirSect     dw ?  ;Sector of current directory cluster
    dirEntry    dd ?  ;32 byte offset in dir for file being searched for
;Error DPB 
    tmpDPBPtr   dq ?  ;A DPB for error/temporary situations
;No clash recycling below var as the vars in SDA are invalid if in CPU 
; exception hdlr. This var gets cleared on entry to the exception handler. 
;If it remains clear, the task will Abort. If it gets set, DOS or COMMAND.COM 
; caused CPU exception or we have an NMI. Then we freeze as we cant guarantee 
; anything anymore.
haltDOS:
    mediaByte   db ?  ;Calls 1Bh and 1Ch return ptr to here
    
    renameFFBlk db ffBlock_size dup (?)  ;Source file "find first" block
    renameDir   db fatDirEntry_size dup (?)  ;Build new file dir entry here
;Stacks and scratch SFT
    alignb  8
    critStack   dq 165 dup (?)
    critStakTop dq ?

    scratchSFT  db sft_size dup (?)  ;Used in FCB calls to emulate a SFT
    
    alignb  8
    AuxStack    dq 199 dup (?)
    AuxStakTop  dq ?  ;Auxilliary stack (Char IO, Int 25h/46h etc)
    DiskStack   dq 199 dup (?)
    DiskStakTop dq ?

    lookahead   db ?  ;-1 => Lookahead on select Char function calls!
;Below is used in create and delete for vol lbl only. Else is -1.
    rebuildDrv  db ?  ;Stores the drive letter of the dpb to reset.
    sdaLen      equ     $ - sda 
    sdaDOSLen   equ     $ - sdaDOSSwap

;Additional variables NOT in the SDA
    serverDispTblPtr    dq ?  ;DO NOT MOVE! Used to find server dispatch tbl
;A backup header to allow copying to for saving the current header when 
; quickly doing a second request
    bkupReqHdr          db ioReqPkt_size dup (?)  
;Prevent toggling print if in the middle of reading an extended ASCII char
inExtASCII:
    noPrintTog  db ?  ;00 = Toggle as usual, 01 = Prevent toggle
    keybTicks   dw ?  ;Counts the number of cycles spent in a kb loop.
;Every time this overflows, we read the clock and update the DOS internal
; copy of the date/time record
;The idt doesnt need to be in the SDA as we will halt interrupts
; until we get/set the address. Thus the IDT entry returned is the 
; correct one AT the time of calling up to "the time it takes to get
; to the read IDT routine".
dosIdtPtr:          ;21h/25h will always read a new copy of IDT here
    .limit  dw ?    ;Overlap this with stack below as no call overlap
    .base   dq ?
;Lseek and IOCTL return data in registers as well as on the caller's 
; stack. In Int 2Fh, this could overwrite user data if the functions
; were allowed to write to original callers register stack. 
; So we have this structure below that is used by these functions to 
; write their "return" data onto a "stack", even though when accessed 
; through the multiplexer we never will read this structure. 
; Really only 4 qwords are needed (rax-rdx) but yaknow... safety
    mplxRegStack    db callerFrame_size dup (?) 