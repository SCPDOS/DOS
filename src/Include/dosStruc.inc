
;-------------------------------------------------------;
; This include file contains miscellaneous internal     ;
; SCP/DOS structures.                                   ;
;-------------------------------------------------------;

;File/path length equates.
MAX_FNAME_FCB   equ 8
MAX_EXT_FCB     equ 3
MAX_NAME_FCB    equ MAX_FNAME_FCB + MAX_EXT_FCB
MAX_NAME_FCBZ   equ MAX_NAME_FCB + 1    ;Space for a null terminator
;Values below include the null terminator.
MAX_FILE    equ 13  ;Maximum length of a filename (includes dot)
MAX_DRIVE   equ 3   ;Maximum length of a drive string (X:<NUL>)
MAX_FSPEC   equ 64  ;Maximum length of a path to a file (filespec)
MAX_PATH    equ 67  ;Maximum length of a fully qualified pathspec
;MAX_PATH is X:\<string, maxlen 63 chars>,<null> so 67 chars total.
;Use this symbol to check the length of a filename after qualification.
MAX_PSPEC   equ MAX_PATH - MAX_FILE - 1 ;54 bytes
;MAX_PSPEC is the max length of a path with space for a filename!
;Remember, filenames for us are just a string of, at most, 13 characters,
; which we always resolve into FCB format. Buffers need to make space for
; both the dot and terminating null
;------------------------------------------------------------------------;
; Each mass storage device up to LASTDRIVE gets a DPB, describing 
; its file system structure. This is used to locate files and structures
; on the device itself and to translate file requests to cluster chains
; to LBAs to pass to the disk driver..
; The DPBs form a linked list, with each DPB formed after the devices 
; drivers Initialise routine has been called.
;------------------------------------------------------------------------;
struc dpb        ;Drive Parameter Block

    .bDriveNumber           db ?  ;Drive number (0=A, 1=B ...)
    .bUnitNumber            db ?  ;Unit number in device
    .wBytesPerSector        dw ?  ;min 512, max 4096
    .bMaxSectorInCluster    db ?  ;(Maximum sector in cluster) - 1
    .bSecPerClustShift      db ?  ;Sectors per cluster exponent
    .wFAToffset             dw ?  ;Number of reserved sectors in partition
    .bNumberOfFATs          db ?  ;Number of FATs
    .wNumberRootDirEntries  dw ?  ;In 32 byte units, this is 0 for FAT32
    .dClusterHeapOffset     dd ?  ;Start sector of data area
    .dMaxClusterAddr        dd ?  ;Max Cluster Addr = # of data clusters + 1
    .dFATlength             dd ?  ;FAT length, in sectors
    .dFirstUnitOfRootDir    dd ?  
    ;Unit=Sector/Cluster of Root Directory (FAT12/16 = Sector, FAT32 = Cluster)
    .qDriverHeaderPtr       dq ?  ;Pointer to device driver header
    .bMediaDescriptor       db ?  ;Media descriptor
    .bAccessFlag            db ?  ;Access Flag (0 if accessed, else -1)
    .qNextDPBPtr            dq ?  ;Pointer to next DPB, -1 if at end
    .dNextFreeClst          dd ?  ;Starting cluster of free space search
    .dFreeClustCnt          dd ?  ;Number of free clusters, -1 unknown

endstruc

fat12MaxClustCnt    equ 4085    ;Cluster count less than this is FAT12
fat16MaxClustCnt    equ 65525   ;Cluster count less than this is FAT16
;------------------------------------------------------------------------;
; Each mass storage device up to LASTDRIVE has a CDS associated with it.
; All disk pathing requests go via the CDS for that drive.
; The default drive is the last accessed drive, this number is stored
; in the DOS data area.
; The CDS is stored as an array with the offset into the array being 
; given by the drive letter's offset into the alphabet (with A=0).
;
; DEFAULT LASTDRIVE=E, MINIMUM LASTDRIVE=B
;------------------------------------------------------------------------;
struc cds       ;Current Directory structure
    .sCurrentPath        db MAX_PATH dup (?)    ;Current Directory Path String
    .wFlags              dw ?   ;CDS Flags - 
    .qDPBPtr             dq ?   ;Ptr to the DPB of the dev using this CDS
    .dStartCluster       dd ?   ;Starting cluster of the directory on disk
    .qIFSPtr             dq ?   ;Reserved for IFS driver expansions
    .dNetStore           dd ?   ;For storing a owner dword for IFS (21h/5F03h)
    .wBackslashOffset    dw ?   ;How many chars to skip for getting the path
; currentPath (usually 2 to skip C: in C:\, but when SUBSTing a drive letter to 
; another, we save the number of chars to skip to that deep level).
; I.E. if we SUBST A:\DOS G: then this value for the CDS for drive G is 6
; Only affects for SUBST drives, not JOIN
; Flags: Bit 15 set = Network drive
;        Bit 14 set = Physical drive
;        Bit 13 set = JOIN'ed drive
;        Bit 12 set = SUBST'ed drive
;        Bit 7  set = Redirected, not net. drive (This is how MSCDEX worked)
endstruc
;CDS equates
cdsRedirDrive   equ 8000h   ;All drives which use the Redirector interface
cdsValidDrive   equ 4000h   ;CDS valid
cdsJoinDrive    equ 2000h   ;CDS for joined drives
cdsSubstDrive   equ 1000h   ;Drives formed by Subst
cdsRdirLocDrive equ 0080h   ;Redirector but Local
;------------------------------------------------------------------------;
; The SFT is a way to allow applications to open file handles to files 
; and devices. As things stand, the SFT chain will have 5 file handles
; under the first header, and then a second header will be linked to the 
; first one after CONFIG.SYS has been read, to allow for FILES= to specify
; a size for the second SFT (default 10 files). Same for System FCBs, 
; using the FCBS= command. Default FCBS=5.
;
; A file handle describes the file, and the location within the file that
; we are reading. The file handle can be manipulated without the file
; being in memory, thus allowing the user to access data without needing 
; to take up large portions of memory. Similarly, file handles allow for
; accessing devices, by treating them as files with their drivers 
; responding and translating the manipulations of the file handle itself.
; Neither the driver nor the application should see or use the SFT for
; the handle, as it is subject to change (and will change when file 
; sharing provisions are included). The DOS will make requests to the 
; driver in a generic way and the driver will respond, with DOS keeping
; track of the information in the SFT entry for that handle. 
;
; One SFT entry can have multiple handles and multiple SFT entries
; can point to the same file. It all depends on what the applications 
; want to do with the File.
;
; !!!!IMPORTANT!!!!
; Currently there are plans for File Sharing, but I include minimal 
; provisions for it. I really just wanna get a DOS out the door.
;
;Each SFT may look like this:
;sysvars-->sfth------------>sfth------------>|ENDOFLIST|
;            |                |
;       wNumFiles*sft    wNumFiles*sft
;
; Two SFT chains are built; one for files, and a one for so called 
; System FCBs.
;
; Each SFT header has an array of SFT's below it, with the number of 
; SFTs given by wNumFiles. The SFT headers form a linked list.
;
; DEFAULT FILES=8, MINIMUM FILES=5
; DEFAULT FCBS=5,  MINIMUM FCBS=5
;------------------------------------------------------------------------;
struc sfth   ;System file table header
; This is the SFT header.
; This structure comes before the start of the first file in the array/table.
    .qNextSFTPtr dq ?    ;Pointer to the next SFT
    .wNumFiles   dw ?    ;Number of files in this table
endstruc

struc sft
;This is the file struc itself
    .wNumHandles dw ?    ;Number of handles pointing to this file
    .wOpenMode   dw ?    ;File access permissions and behvaiours
    .bFileAttrib db ?    ;Regular File Attributes
    .wDeviceInfo dw ?    ;Includes unit number if a block device
    .qPtr        dq ?    ;IF char, ptr to device driver header, ELSE DPBptr
    .dStartClust dd ?    ;Start cluster of file pointed to by SFT entry
    .wTime       dw ?    ;File Time, updated on open and writes
    .wDate       dw ?    ;File Date, updated on open and writes
    .dFileSize   dd ?    ;File Size, in bytes
    .dCurntOff   dd ?    ;Current Offset pointed to by SFT entry, in bytes
    .dRelClust   dd ?    ;Rel cluster pointed to by SFT entry
    .dAbsClusr   dd ?    ;Absolute cluster pointed to by SFT entry
    .qDirSect    dq ?    ;Abs number of sector containing directory entry 
    .bNumDirEnt  db ?    ;Directory entry within dir sector (byte offset/32)
    .sFileName   db 11 dup (?)   ;8.3 Filename
    .qSharePtr   dq ?    ;SHARE pointer to previous SFT sharing same file
    .wMachNum    dw ?    ;SHARE number of network machine opening file
    .qPSPOwner   dq ?    ;Addr of PSP of program which first opened this file
    .wShareRec   dw ?    ;offset within SHARE code segment of sharing record
;                           0 means no record
endstruc
;Open Mode equates
;Access modes   Bits[2:0]
openRdAcc   equ 0h
openWrAcc   equ 1h
openRWAcc   equ 2h
;Bit 3 reserved 0
;Sharing modes  Bits[6:4]
openCompat      equ 0h  ;Deny all, compatibility mode
openDenRWShr    equ 10h ;Deny all, explicity read/write
openDenWrShr    equ 20h ;Deny writes
openDenRdShr    equ 30h ;Deny reads
openDenNoShr    equ 40h ;Don't deny anything!
;NetFCB is for files open by a local server, handling IO from a remote
; client by proxy opened by an FCB! Sets the Net bit to indicate this. 
;Thus, this is a handle with no JFT entry! Server handles its own JFT!
openNetFCBShr   equ 70h
openNoInherit   equ 80h

;Bits 8-12 reserved 0
;The next two are extended open mode bits! 
;Can be set
openFailOnI24   equ 2000h   ;If set, automatically FAIL all Int 24h
openFlushWrites equ 4000h   ;If set, commit after each write call.
;Once share is loaded, if a local FCB open occurs, mark the sft as so.
openSFTFCB      equ 8000h   ;If set, file is SFT-FCB.

;Attribute Byte
attrFileRO          equ 1h
attrFileHidden      equ 2h
attrFileSys         equ 4h
attrFileVolLbl      equ 8h
attrFileDir         equ 10h ;Directory entry
attrFileArchive     equ 20h
attrFileChar        equ 40h ;Reserved, never found in dir

;Device Info word equates
;For a block device, bits [5:0] contain the drive number
devCharConIn    equ 1h  ;If set, device is CON input
devCharConOut   equ 2h  ;If set, device is CON output
devCharNulDev   equ 4h  ;If set, device is NULL device
devCharClkDev   equ 8h  ;If set, device is CLOCK device

devCharFastOut  equ 10h ;If set, device supports INT 29h
devCharBinary   equ 20h ;If set, r/w char device in Binary mode
devCharNotEOF   equ 40h ;If set, device not yet at end of file
devDiskNoFlush  equ 40h ;Set if file is synchronised with hard store!
devCharDev      equ 80h ;If set, it is a char device!

devDiskI24onFull    equ 100h    ;If set, Int 24h on disk full or disk EOF
devCharNetSpool equ 800h    ;If set, device is a network spooler

devNoInherit    equ 1000h   ;If set, handle not inherited by child processes
devCharNamePipe equ 2000h   ;If set, device is a named pipe
devDiskNoDTonClose equ 4000h   ;If set, dont update date and time when closing
devRedir        equ 8000h   ;If set, dev/file is redirected!

;Extended open flags. For the var [wEOFlags]
;These are not related to the SFT but are used during an extended open
; call to keep track of whats going on and thus help build an SFT.
eoInExtOpen     equ 0001h   ;Doing Extended Open
eoFailI24       equ 0002h   ;If EO file hard fails, skip and fail Int 24h
eoFileNoExist   equ 0004h   ;File doesnt exist

;Can combine the third option with one of the first two options.
;The first two options cannot be combined.
; - If only create is selected, then we try to do a create new. If the 
;       file exists, this fails with a "File exists" error.
;       Acts like a normal create new call. (21/5Bh)
;
; - If only open is selected then we try to do an open. If the file doesn't
;       doesn't exist, this fails with a "File not found" error.
;       Acts like a normal open call. (21/3Dh)
;
; - If only truncate is selected then we try to do a create. If the file
;       doesn't exist, this fails a "File not found" error.
;       New behaviour.
;
; - If open and create are selected then we try and open the file. If 
;       the open fails due to the file not existing, we create a new file. 
;       New behaviour.
;
; - If truncate and create are selected then we try to truncate the file. 
;       If the file doesnt exist then we create a new file instead.
;       Acts like a normal create call. (21/3Ch)

eoActOpen       equ 01h ;Open the file. 
eoActTruncate   equ 02h ;Truncate an existing file.
eoActCreate     equ 10h ;Create a file if one doesnt exist.
;Extended Open Action Return 
eoRetActOpen    equ 1   ;Open existing
eoRetActCreate  equ 2   ;Create new and open
eoRetActTruncate    equ 3    ;Recreate and open
;------------------------------------------------------------------------;
; Buffers allow for buffered drive IO, by first transferring a sector 
; to an internal buffer, and then copying it to the specified location
; in memory. The buffer is kept in the buffer chain,  in the event that 
; another request for the same sector from the same device is made, so 
; that is can be read from the device directly. Buffers are uniform in 
; size, selected according to the largest detected sector size during 
; the initial loadup.
;
; The buffer pointer in SysVars points to most recently used Buffer
;
; DEFAULT BUFFERS=30, MINIMUM BUFFERS=2
;------------------------------------------------------------------------;

struc bufferHdr
    .nextBufPtr  dq  ?   ;Pointer to the next buffer in the chain or -1 at end
    .wDrvNumFlg:         ;Read as a word to get the following two bytes!
    .driveNumber db  ?   ;Drive number the buffer is used for (or -1 for free)
    .bufferFlags db  ?   ;Standard Flags
    .bufferLBA   dq  ?   ;LBA the buffer contains
    .bufFATcopy  db  ?   ;Number of copies of the FAT to write (1 if non-FAT)
    .bufFATsize  dd  ?   ;Number of sectors per FAT (or 0 if non-FAT)
    .driveDPBPtr dq  ?   ;Pointer to the device DPB block
    .reserved    db  ?   ;Alignment byte
    .dataarea:             ;Points to the start of the data area
endstruc
;Buffer Flag equates
dosBuffer   equ 1h  ;Sector 0 (BPB/VPB) in buffer
fatBuffer   equ 2h  ;FAT sector in buffer
dirBuffer   equ 4h  ;Directory sector in buffer
dataBuffer  equ 8h  ;File Data sector in buffer
refBuffer   equ 20h ;Data in buffer has been referenced by an application
dirtyBuffer equ 40h ;Data in buffer modified by an application
freeBuffer  equ 00FFh   ;A word which goes into the drive number to clear both fields
;Note! A buffer is marked as referenced when the buffer is "obtained" by a syscall
;------------------------------------------------------------------------;
; Memory control block for each memory arena
;------------------------------------------------------------------------;
struc mcb
    .marker     db ?  ;M = Member of the MCB chain. Z = Last member
    .owner      dq ?  ;Owning application PSP (para aligned) or 0 for free
    .blockSize  dd ?  ;Number of contiguous paragraphs owned by this block
    ;The following byte is reserved outside of sysinit
    .subSysMark db ?  ;Marks the optional DOS subsystem of the MCB
    .reserved   db 2 dup (?) 
    .program:
endstruc
;MCB equates
mcbOwnerHole    equ 7   ;Owner = 7 => Memory hole, always fail free/realloc
mcbOwnerFree    equ 0   ;Owner = 0 => Free and available space
mcbOwnerDOS     equ 8   ;Owner = 8 => Owned by DOS
mcbOwnerNewDOS  equ 9   ;Owner = 9 => New Kernel driver alloc. Needs setting to 8.
mcbMarkCtn      equ "M"
mcbMarkEnd      equ "Z"
;The following are used on MCB's that belong to DOS, that are not part of the
; main kernel (i.e. the CONFIG.SYS optional parts).
mcbSubDriver    equ "D" ;Any memory allocated for a device driver's code.
mcbSubDrvExtra  equ "L" ;Any memory allocated by a device driver.
mcbSubDrvDPB    equ "P" ;Blocks used for DPB's are marked so.
mcbSubIFS       equ "I" ;Unused for now
mcbSubFiles     equ "F" ;Any memory allocated for Files.
mcbSubFCBS      equ "X" ;Unused for now
mcbSubBuffers   equ "B" ;Any memory allocated for Buffers.
mcbSubCDS       equ "L" ;Any memory allocated to store a CDS.
mcbSubStacks    equ "S" ;Unused for now


memFirstFit     equ 0   ;First free block goes to application
memBestFit      equ 1   ;Block whos size closest matches requirement
memLastFit      equ 2   ;Last free block goes to application

;Owner = Para aligned address => Owning application PSP
;...To consider...
;Owner = Non-para aligned address => Error? Should maybe lock up system?
;blockSize counts the number of paragraphs FROM .program TO THE NEXT MCB!
;------------------------------------------------------------------------;
; For Int 21h, the callers registers are saved on their stack in the 
; following order, thus allowing for return values to be placed in the 
; registers by accessing the caller register stack frame.
;------------------------------------------------------------------------;
struc callerFrame
    .rax    dq ?  ;Bottom of frame, pointer to rax saved in oldRSP
    .rbx    dq ?    
    .rcx    dq ?
    .rdx    dq ?
    .rsi    dq ?
    .rdi    dq ?
    .rbp    dq ?
    .r8     dq ?  ;To be removed if truly not needed
    .r9     dq ?
;Pushed by Processor when invoked via Interrupt
    .rip    dq ?
    .cs     dq ?
    .flags  dq ?
endstruc

struc dpl
;DOS parameter list, pointer to this stack is passed to DOS in rdx on
; server calls
    .rax    dq ?
    .rbx    dq ?
    .rcx    dq ?
    .rdx    dq ?
    .rsi    dq ?
    .rdi    dq ?
    .r8     dq ?
    .r9     dq ?
    .align  dq ?
    .compID dq ?  ;Computer id, low word used only
    .procID dq ?  ;Process ID
endstruc

struc cfgFrame  ;Frame used for config.sys parsing
    .oldRBP         dq ?    ;Ptr to DOSSEG
    .newBuffers     dq ?    ;New buffers value
    .newSFTVal      dq ?
    .newFCBSVal     dq ?
    .newProtFCBSVal dq ?    ;Number of protected FCBS
    .newLastdrive   dq ?
    .newStacks      dq ?    ;Ignored for now
    .newStackSize   dq ?    ;Ignored for now
    .cfgHandle      dq ?    ;CONFIG.SYS handle
    .lastLine       dq ?    ;Set if the last line is current
    .linePtr        dq ?    ;Ptr to line buffer of line being processed
    .driverBreak    dq ?    ;Ptr to the pathname terminator 
    .breakChar      dq ?    ;Char that was replaced by null for terminator
endstruc

struc filename
;8.3 File name struct
    .fName  db 8 dup (?)
    .fExt   db 3 dup (?)
endstruc

struc ffBlock   ;The structure of the Find First structure at the DTA
    .driveNum   db ?  ;Drive we are searching on, 1 based number
    .template   db 11 dup (?) ;Search template (? meaning anything)
    .attrib     db ?  ;Search attribute
    .dirOffset  dd ?  ;32 byte entry within dir cluster
    .parDirClus dd ?  ;Parent Directory Cluster number
    .attribFnd  db ?  ;Attrib of file found
    .fileTime   dw ?  ;File time
    .fileDate   dw ?  ;File date
    .fileSize   dd ?  ;Number of bytes
    .asciizName db 13 dup (?) ;ASCIIZ name with dot and terminating nul
endstruc

dfltJFTsize equ 20  ;Default size of PSP JFT
struc psp
    .return     db 2 dup (?)  ;Should always be CDh 20h, same place as DOS
    .allocSize  dd ?  ;Number of paras in init alloc or when exiting as TSR.
                dd ?  ;Reserved 4 bytes
    .oldInt22h  dq ?  ;Int 22h pointer on overlay load
    .oldInt23h  dq ?  ;Int 23h pointer on overlay load
    .oldInt24h  dq ?  ;Int 24h pointer on overlay load
    .parentPtr  dq ?  ;Pointer to parent process PSP
;If jftSize > 20, then the QWORD at .jobFileTbl becomes a pointer
; to the actual in use JFT and the other 12 bytes are left undefined.
;If jftSize < 20 (DOS will never set it so), then it is considered as 20
;If the user tries to reduce handle count to 20 or less, then the PSP JFT is 
; always used and jftSize.
    .externalJFTPtr:    ;Ptr to external JFT array (first byte AFTER MCB)
    .jobFileTbl db dfltJFTsize dup (?) ;Main File handle array, JFT
    .envPtr     dq ?  ;Pointer to the environment
    .rspPtr     dq ?  ;Pointer to rsp on entry to Int 21h
    .jftSize    dw ?  ;JFT array size, 20 => PSP JFT in use
    .unixEntry  db 3 dup (?)  ;Must always be CDh 21h CBh, same place as DOS
    .prevPSP    dq ?  ;Pointer to the previous PSP in chain (used by SHARE)
                db ?  ;Reserved byte
;The below are in the same place as in DOS.
    .cmdLineArgPtr: ;Symbol for future use
    .fcb1       db 16 dup (?)  ;First FCB,    argument 1 
    .fcb2       db 20 dup (?)  ;Second FCB,   argument 2
    .dta:   ;Pointer to the default DTA in the PSP
    .parmList   db ?   ;Number of characters in command tail
    .progTail   db 127 dup (?) ;Default DTA/Program tail
endstruc

struc clkStruc
    .dateWord   dw ?
    .minutes    db ?
    .hours      db ?
    .hseconds   db ?  ;Hundreths of seconds
    .seconds    db ?
endstruc


struc extCtryStruc
    .infoIDCode db ?    ;Information ID code (01h)
    .strucSize  dw ?    ;Length of the structure  (42 or less)
    .ctryID     dw ?    ;Country ID number
    .cpNumber   dw ?    ;Code page number
    .dtfmt      dw ?    ;Date time format (2 bytes)
                        ;0 = month day year hh:mm:ss
                        ;1 = day month year hh:mm:ss
                        ;2 = year month day hh:mm:ss
    .curr       db 5 dup (?)   ;ASCIIZ Currency symbol (5 chars)
    .thouSep    db 2 dup (?)   ;ASCIIZ Thousands separator
    .dcmlSep    db 2 dup (?)   ;ASCIIZ Decimal separator
    .dateSep    db 2 dup (?)   ;ASCIIZ Date separator
    .timeSep    db 2 dup (?)   ;ASCIIZ Time separator
    .currFmt    db ?    ;Currency Format
                        ;0 = Symbol leads, without space
                        ;1 = Symbol follows, without space
                        ;2 = Symbol leads, one space
                        ;3 = Symbol follows, one space
                        ;4 = Symbol replace decimal separator
    .digtdpt    db ?    ;Number of digits after the decimal point
    .timefmt    db ?    ;Time format, Bit 0 = 0 => 12 hour, = 1 => 24 hour clock
    .mapptr     db 8 dup (?)  ;Case map address (0 is nulptr), needs fixing up
    .dataSep    db 2 dup (?)  ;Data list separator
    .resv       db 10 dup (?) ;Reserve 10 bytes
endstruc

struc countryStruc
    .dtfmt      dw ?    ;Date time format (2 bytes)
                        ;0 = month day year hh:mm:ss
                        ;1 = day month year hh:mm:ss
                        ;2 = year month day hh:mm:ss
    .curr       db 5 dup (?)   ;ASCIIZ Currency symbol (5 chars)
    .thouSep    db 2 dup (?)   ;ASCIIZ Thousands separator
    .dcmlSep    db 2 dup (?)   ;ASCIIZ Decimal separator
    .dateSep    db 2 dup (?)   ;ASCIIZ Date separator
    .timeSep    db 2 dup (?)   ;ASCIIZ Time separator
    .currFmt    db ?    ;Currency Format
                        ;0 = Symbol leads, without space
                        ;1 = Symbol follows, without space
                        ;2 = Symbol leads, one space
                        ;3 = Symbol follows, one space
                        ;4 = Symbol replace decimal separator
    .digtdpt    db ?    ;Number of digits after the decimal point
    .timefmt    db ?    ;Time format, Bit 0 = 0 => 12 hour, = 1 => 24 hour clock
    .mapptr     db 8 dup (?)  ;Case map address (0 is nulptr), needs fixing up
    .dataSep    db 2 dup (?)  ;Data list separator
    .resv       db 10 dup (?) ;Reserve 10 bytes
endstruc

;Critical Error AH bitfield equates
critRead    equ 0h
critWrite   equ 1h
critDOS     equ 0h
critFAT     equ 2h
critDir     equ 4h
critData    equ 6h
critFailOK  equ 8h
critRetryOK equ 10h
critIgnorOK equ 20h
critCharDev equ 80h

critIgnore  equ 0
critRetry   equ 1
critAbort   equ 2
critFail    equ 3

;MISC ASCII control chars
NUL equ 00h ;^@         | ASCII Null
ETX equ 03h ;^C         | ASCII Break (End of Text) 
ACK equ 06h ;^F         | ASCII Acknowledgement
BEL equ 07h ;^G         | ASCII Bell
BSP equ 08h ;^H         | ASCII Backspace
TAB equ 09h ;^I         | ASCII Horizontal Tab
LF  equ 0Ah ;^J         | ASCII Line Feed
CR  equ 0Dh ;^M         | ASCII Carriage Return
DLE equ 10h ;^P         | ASCII Data Link End (Toggle Print Echo)
DC3 equ 13h ;^S         | ASCII Device Control 3 (Toggle Scroll Lock)
DC4 equ 14h ;^T         | ASCII Device Control 4
NAK equ 15h ;^U         | ASCII Negative Acknowledgement
ETB equ 17h ;^W         | ASCII End of Transmission Block
EOF equ 1Ah ;^Z         | ASCII Substitute character (End of File)
ESC equ 1Bh ;           | ASCII Escape char, used for ANSI.SYS
SPC equ 20h ;Printable  | ASCII Space
;This last one is NEVER printed with a caret as it is a console control char
DEL equ 7Fh ;^?         | ASCII Rubout (Del char and dec cursor)

asciiCaret  equ 20h ;Used to check which chars need to be printed with caret
;Extended Ascii Codes
eF1     equ 3Bh ;F1 second byte
eF2     equ 3Ch ;F2 second byte
eF3     equ 3Dh ;F3 second byte
eF4     equ 3Eh ;F4 second byte
eF5     equ 3Fh ;F5 second byte
eF6     equ 40h ;F6 second byte
eF7     equ 41h ;F7 second byte
eCursL  equ 4Bh ;Cursor Left second byte
eCursR  equ 4Dh ;Cursor Right second byte
eIns    equ 52h ;Insert second byte
eDel    equ 53h ;DEL second byte (not necessarily delete key)

;Default Configuration for the system

buffersDefault  equ 30
filesDefault    equ 20
fcbsDefault     equ 4
safeFcbsDeflt   equ 0
lastDriveDeflt  equ 5   

;ASCII char property table equates. If such a bit is clear in signature in table
; then the char has this property.
badChar     equ 1   ;Bad ASCIIZ filename char
termChar    equ 2   ;Terminator char type
sepChar     equ 4   ;Separator char type
badFCBChar  equ 8   ;FCB filename char