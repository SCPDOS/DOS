;This file contains the main DOS data segment
dosDataArea:
    anchorMcb   resb mcb_size   ;This is space for the anchor MCB
    bootDrive   resb 1    ;The logical drive we booted from
    biosVers    resd 1    ;Indicates BIOS type. Each OEM picks a number
    biosPtr     resq 1    ;For saving a data ptr to be used by BIOS/drivers
;Above is the system stats
;Below is the DOS vars, DO NOT TOUCH FROM validNetNam TO NUMJOINDRV
;Both below variables can be edited with Int 41h AX=440Bh
    validNetNam resw 1    ;Flag if machinename valid, deflt no=0
    shareCount  resw 1    ;Share Retry Count, number of repeats before fail.
    shareDelay  resw 1    ;Share Delay, in multiples of ms. (TEMP, just loop)
                resq 1    ;Unused ptr for future, current disk buffer
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
    fcbsHeadPtr resq 1    ;Pointer to the head of the System FCB chain
    numSafeSFCB resw 1    ;Number of protected FCBs (y in FCBS=x,y)
    numPhysVol  resb 1    ;Number of physical volumes in the system x
    lastdrvNum  resb 1    ;Value of LASTDRIVE (default = 5) [Size of CDS array]x
    numBuffers  resb 1    ;Buffers=30 default
    nulDevHdr   resb drvHdr_size
    numJoinDrv  resb 1    ;Number of Joined Drives
;Additional internal variables
    numFiles    resb 1    ;FILES=5 default, max 255
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
    registerDLL     resq 1  ;Entered with rbp = execFrame
    unloadDLLHook   resq 1  ;
    ;Share hook functions here
    ;All share hooks now take 8 bytes rather than 4 bytes as before
    ;Thus ALL offsets from SFT header increase by 4 bytes and each entry
    ; is a QWORD entry. Please adjust SHARE.EXE access as necessary.

    ;Note to programmer - Please adjust as necessary:
    ;Functions which are nowhere called (yet) are noted as UNUSED in caps.
    ;Those which are not meant to be used are noted as unused in lower case.
    ;Those suffixed with a ? have their future in question.
    ;Those suffixed with a / are done partially wrt MSDOS.

    ;SCPDOS has an optional handle, openFileCheck. This function can be used 
    ; to see if there are any open handles. Its implementation is completely
    ; optional, and an equivalent form of the function is provided for older
    ; SHARE versions that might be ported that don't have a particular function.
    ;This function is defined as follows:
    ; Input: fname1Ptr -> Filename we want to see if there are any open records
    ;                       for.
    ; Output: CF=CY -> Handle not supported.
    ;         CF=NC -> Handle supported. 
    ;           ZF=ZE -> No Files open.
    ;           ZF=NZ -> Some files are open.
shareHooks:
    ;markerShare resq 1  ;Marker Share hook
    openFileCheck   resq 1  ;Check if share record for file exist       DONE
    openShare   resq 1  ;Share called on open.                          DONE 
    closeShare  resq 1  ;Share called on close.                         DONE/
    closeCompShare  resq 1  ;Share to close all files for a machine.    DONE
    closeTaskShare  resq 1  ;Share to close all files for a task.       DONE
    closeNameShare  resq 1  ;Share to close file by name.               DONE/
    lockFileShare   resq 1  ;Share to lock file region.                 DONE
    unlockFileShare resq 1  ;Share to unlock file region.               DONE
    checkFileLockShare  resq 1  ;Share to check file region locked.     DONE
    openFileListShare   resq 1  ;Share to get open file list entry.     DONE
    updateFCBfromSFTShr resq 1  ;Share to update FCB from the SFT.      UNUSED?
    fstClstOfFCBShare   resq 1  ;Share to get first cluster of FCB.     UNUSED?
    closeDupFileShare   resq 1  ;Share to close file if dup for proc.   DONE
    closeNewHdlShare    resq 1  ;Share to close hdls of rec opened file. DONE
    updateDirShare      resq 1  ;Share to update dir info in SFT.       DONE 
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
    serverCnt   resb 1  ;Increments on each 41h/5E01h call
    machineName resb 16 ;Machine name (Set via 41h/5E01h) (set to SPC)    
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
sdaDOSSwap:
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
    altRet: ;Accessed as a qword
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
    dosffblock  resb ffBlock_size   ;Internal search block (fullsize unlike DOS)
    curDirCopy  resb fatDirEntry_size   ;Copy of directory being accessed
    tmpCDS      resb cds_size   ;Temp CDS for Server calls that need a tmp CDS
    fcbName     resb 11+1   ;11 chars for 8.3 ( w/o the dot) and terminating 0
    wcdFcbName  resb 11+1   ;Used to expand any wildcards for rename
    fileDirSect resq 1  ;File/Directory starting sector, for each level

    volIncmpFCB resb 1  ;Set to -1 if the volume uses FAT32 (or all incompat FS)
    extFCBAttr  resb 1  ;Extended FCB file attribute
    extFCBFlag  resb 1  ;Set to -1 if Extended FCB
    searchAttr  resb 1  ;Directory Search attributes
    fileOpenMd  resb 1  ;Open mode (compat, r/w/rw?)
    fileFDflg   resb 1  ;01h = File Found!, 04h = File deleted!
    badNameRen  resb 1  ;Device name or File not found for rename
    rwFlag      resb 1  ;00h=Read, -1=Write, read/write/share error reporting
    spliceFlag  resb 1  ;00 = Relative path, !0 = Full path
    dosInvoke   resb 1  ;0 = Invoked via Int 41h, -1 = Invoked via 41h/5D01h

    vConInsert  resb 1  ;Insert mode on 41/0ah (0 = not insert, !0 = insert)
    fileExist   resb 1  ;-1 if file in pathspec exists (create/open)
    parDirExist resb 1  ;-1 if parent directory for file exists (create/open)
    exitType    resb 1  ;Forms the upper byte of the errorlvl
    openCreate  resb 1  ;If open, set to 0, if Create set to -1
    delChar     resb 1  ;Char to replace first byte of deleted file's name
    workingDrv  resb 1  ;Working drive number, 0 based, from DPB
qPtr:       ;Stores working DPB and/or device driver (if r/w a char device)
workingDD:  ;Create a symbol for the working device driver too
    workingDPB  resq 1  ;Ptr to the DPB of the drive being accessed
    workingCDS  resq 1  ;Ptr to the CDS of the drive being accessed
    workingFCB  resq 1  ;Ptr to the caller FCB for FCB function
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
sectTfr:    ;Symbol to use this var to hold a counter on disk read/write ops 
pathLen:    ;Used to store the length of a path string for removal strcmp
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
    fileGrowing resb 1  ;Flag to indicate the file is growing
    bytesAppend resd 1  ;Number of bytes by which a file has been extended by
    tfrLen      resd 1  ;Number of bytes to transfer
    tfrCntr     resd 1  ;Number of bytes left to transfer
;Directory stuff
    dirClustPar resd 1  ;Absolute disk cluster of the start of the parent dir
    dirClustA   resd 1  ;Absolute cluster number of current directory
    dirSect     resw 1  ;Sector of current directory cluster
    dirEntry    resd 1  ;32 byte offset in dir for file being searched for
;Error DPB 
    tmpDPBPtr   resq 1  ;A DPB for error/temporary situations
    mediaByte   resb 1  ;Calls 1Bh and 1Ch return ptr to here
    
    renameFFBlk resb ffBlock_size   ;Source file "find first" block
    renameDir   resb fatDirEntry_size   ;Build new file dir entry here
;Stacks and scratch SFT
    alignb  8
    critStack   resq 165
    critStakTop resq 1

    scratchSFT  resb sft_size   ;Used in FCB calls to emulate a SFT
    
    alignb  8
    AuxStack    resq 199
    AuxStakTop  resq 1  ;Auxilliary stack (Char IO, INT 45h/46h etc)
    DiskStack   resq 199
    DiskStakTop resq 1

    diskChange  resb 1  ;-1 = disk has been changed!
    lookahead   resb 1  ;-1 => Lookahead on select Char function calls! 
;Putting this in SDA as multiple tasks can try to parse EXE's simultaneously
    exeHdrSpace resb imageFileOptionalHeader_size   ;Use for parsing an EXE hdr
    sectHdr     resb imageSectionHdr_size   ;Use to load one sctn hdr at a time
;Exception handler vars in SDA now 
    byteBuffer  resb 16 ;Used by DOS exception handler to build strings
    haltDOS     resb 1  ;Set by DOS exception handler to indicate DOS will halt
    sdaLen      equ     $ - sda 
    sdaDOSLen   equ     $ - sdaDOSSwap

;Additional variables NOT in the SDA
    serverDispTblPtr    resq 1  ;DO NOT MOVE! Used to find server dispatch tbl
    xActDrv     resb 1  ;0 based number of last drive to transact
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
    ;The idt doesnt need to be in the SDA as we will halt interrupts
    ; until we get/set the address. Thus the IDT entry returned is the 
    ; correct one AT the time of calling up to "the time it takes to get
    ; to the read IDT routine".
    dosIdtPtr:          ;41h/25h will always read a new copy of IDT here
        .limit  dw ?
        .base   dq ?
    ;Lseek and IOCTL return data in registers as well as on the caller's 
    ; stack. In Int 4Fh, this could overwrite user data if the functions
    ; were allowed to write to original callers register stack. 
    ; So we have this structure below that is used by these functions to 
    ; write their "return" data onto a "stack", even though when accessed 
    ; through the multiplexer we never will read this structure. 
    ; Really only 4 qwords are needed (rax-rdx) but yaknow... safety
    mplxRegStack    db callerFrame_size dup (?) 
    dSegLen     equ     $