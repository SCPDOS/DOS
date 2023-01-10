
;-------------------------------------------------------;
; This include file contains miscellaneous internal     ;
; SCP/DOS structures.                                   ;
;-------------------------------------------------------;

;------------------------------------------------------------------------;
; Each mass storage device up to LASTDRIVE gets a DPB, describing 
; its file system structure. This is used to locate files and structures
; on the device itself and to translate file requests to cluster chains
; to LBAs to pass to the disk driver..
; The DPBs form a linked list, with each DPB formed after the devices 
; drivers Initialise routine has been called.
;------------------------------------------------------------------------;
struc dpb        ;Drive Parameter Block

    .bDriveNumber            resb 1  ;Drive number (0=A, 1=B ...) x
    .bUnitNumber             resb 1  ;Unit number in device x
    .wBytesPerSector         resw 1  ;min 512, max 4096 x
    .bMaxSectorInCluster     resb 1  ;(Maximum sector in cluster) - 1 x
    .bSectorsPerClusterShift resb 1  ;Sectors per cluster exponent x
    .wFAToffset              resw 1  ;Number of reserved sectors in partition x
    .bNumberOfFATs           resb 1  ;Number of FATs x
    .wNumberRootDirEntries   resw 1  ;In 32 byte units, this is 0 for FAT32 x
    .dClusterHeapOffset      resd 1  ;Start sector of data area x
    .dClusterCount           resd 1  ;Total number of clusters in data area x
    .dFATlength              resd 1  ;FAT length, in sectors x
    .dFirstUnitOfRootDir     resd 1  
    ;Unit=Sector/Cluster of Root Directory (FAT12/16 = Sector, FAT32 = Cluster)x
    .qDriverHeaderPtr        resq 1  ;Pointer to device driver header x
    .bMediaDescriptor        resb 1  ;Media descriptor x
    .bAccessFlag             resb 1  ;Access Flag (0 if accessed, else -1) x
    .qNextDPBPtr             resq 1  ;Pointer to next DPB, -1 if at end
    .dFirstFreeCluster       resd 1  ;Starting cluster of free space search x
    .dNumberOfFreeClusters   resd 1  ;Number of free clusters, -1 unknown x

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
    .sCurrentPath        resb 67    ;Current Directory Path String
    .wFlags              resw 1 ;CDS Flags - 
    .qDPBPtr             resq 1 ;Ptr to the DPB of the dev using this CDS
    .dStartCluster       resd 1 ;Starting cluster of the directory on disk
    .qReserved           resq 1 ;Reserved for future expansions
    .wBackslashOffset    resw 1 ;How many chars to start of current dir in the 
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
    .qNextSFTPtr resq 1    ;Pointer to the next SFT
    .wNumFiles   resw 1    ;Number of files in this table
endstruc

struc sft
;This is the file struc itself
    .wNumHandles resw 1    ;Number of handles pointing to this file
    .wOpenMode   resw 1    ;File access permissions and behvaiours
    .bFileAttrib resb 1    ;Regular File Attributes
    .wDeviceInfo resw 1    ;Includes unit number if a block device
    .qPtr        resq 1    ;IF char, ptr to device driver header, ELSE DPBptr
    .dStartClust resd 1    ;Start cluster of file pointed to by SFT entry
    .wTime       resw 1    ;File Time, updated on open and writes
    .wDate       resw 1    ;File Date, updated on open and writes
    .dFileSize   resd 1    ;File Size, in bytes
    .dCurntOff   resd 1    ;Current Offset pointed to by SFT entry, in bytes
    .dRelClust   resd 1    ;Rel cluster pointed to by SFT entry
    .dAbsClusr   resd 1    ;Absolute cluster pointed to by SFT entry
    .qDirSect    resq 1    ;Abs number of sector containing directory entry 
    .bNumDirEnt  resb 1    ;Directory entry within dir sector (byte offset/32)
    .sFileName   resb 11   ;8.3 Filename
    .qSharePtr   resq 1    ;SHARE pointer to previous SFT sharing same file
    .wMachNum    resw 1    ;SHARE number of network machine opening file
    .qPSPOwner   resq 1    ;Addr of PSP of program which first opened this file
    .wShareRec   resw 1    ;offset within SHARE code segment of sharing record
;                           0 means no record
endstruc
;Open Mode equates
;Access modes   Bits[2:0]
ReadAccess      equ 0h
WriteAccess     equ 1h
RWAccess        equ 2h
;Bit 3 reserved 0
;Sharing modes  Bits[6:4]
CompatShare     equ 0h
denyRWShare     equ 10h
denyWriteShare  equ 20h
denyReadShare   equ 30h
denyNoneShare   equ 40h
netFCBShare     equ 70h
noInherit       equ 80h
;Bits 8-12 reserved 0

diskFullFail    equ 2000h   ;DO NOT USE. IF set, Trigger Int 44h on disk full.
;Buffering on writes
noBufferWrites  equ 4000h   ;If set, dont buffer writes, always update disk
;FCB opened file, shouldn't see this as we are depreciating FCB's completely now
FCBopenedFile   equ 8000h   ;If set, file is opened by FCB, else SFT
;Attribute Byte
readOnlyFile    equ 1h
hiddenFile      equ 2h
systemFile      equ 4h
volumeLabel     equ 8h
directoryFile   equ 10h ;Directory entry
archiveFile     equ 20h
;Device Info equates
;For a block device, bits [5:0] contain the drive number
charDevConIn    equ 1h  ;If set, device is CON input
charDevConOut   equ 2h  ;If set, device is CON output
charDevNulDev   equ 4h  ;If set, device is NULL device
charDevClockDev equ 8h  ;If set, device is CLOCK device
charDevFastOut  equ 10h ;If set, device supports INT 49h
charDevBinary   equ 20h ;If set, r/w char device in Binary mode!!!
charDevNoEOF    equ 40h ;If set, no EOF when reading from device!!
blokFileNoFlush equ 40h ;Set if file has not yet been accessed (sol no flush!)
devCharDev      equ 80h ;If set, it is a char device!
charDevNetSpool equ 800h    ;If set, device is a network spooler
devNoInherit    equ 1000h   ;If set, handle not inherited by child processes
charDevNamePipe equ 2000h   ;If set, device is a named pipe
blokNoDTonClose equ 4000h   ;If set, dont update date and time when closing
devRedirDev     equ 8000h   ;If set, dev/file is redirected!
 
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
    .nextBufPtr  resq  1   ;Pointer to the next buffer in the chain or -1 at end
    .driveNumber resb  1   ;Drive number the buffer is used for (or -1 for free)
    .bufferFlags resb  1   ;Standard Flags
    .bufferLBA   resq  1   ;LBA the buffer contains
    .bufFATcopy  resb  1   ;Number of copies of the FAT to write (1 if non-FAT)
    .bufFATsize  resd  1   ;Number of sectors per FAT (or 0 if non-FAT)
    .driveDPBPtr resq  1   ;Pointer to the device DPB block
    .owningFile  resq  1   ;Ptr to SFT/FCBS which owns buff (-1 if not in chain)
    .reserved    resb  9   ;Alignment bytes
    .dataarea:             ;Points to the start of the data area
endstruc
;Buffer Flag equates
dosBuffer   equ 1h  ;Sector 0 (BPB/VPB) in buffer
fatBuffer   equ 2h  ;FAT sector in buffer
dirBuffer   equ 4h  ;Directory sector in buffer
dataBuffer  equ 8h  ;File Data sector in buffer
refBuffer   equ 20h ;Data in buffer has been referenced by an application
dirtyBuffer equ 40h ;Data in buffer modified by an application
;Note! A buffer is marked as referenced only when the application 
; is done with that buffer OR if a hard error has occured (both read and write)
;------------------------------------------------------------------------;
; Memory control block for each memory arena
;------------------------------------------------------------------------;
struc mcb
    .marker     resb 1  ;M = Member of the MCB chain. Z = Last member
    .owner      resq 1  ;Owning application PSP (para aligned) or 0 for free
    .blockSize  resd 1  ;Number of contiguous paragraphs owned by this block
    .reserved   resb 3  ;Reserved three bytes
    .program:
endstruc
;MCB equates
mcbOwnerHole    equ 7   ;Owner = 7 => Memory hole, always fail free/realloc
mcbOwnerFree    equ 0   ;Owner = 0 => Free and available space
mcbOwnerDOS     equ 8   ;Owner = 8 => Owned by DOS
mcbMarkCtn      equ "M"
mcbMarkEnd      equ "Z"

memFirstFit     equ 0   ;First free block goes to application
memBestFit      equ 1   ;Block whos size closest matches requirement
memLastFit      equ 2   ;Last free block goes to application

;Owner = Para aligned address => Owning application PSP
;...To consider...
;Owner = Non-para aligned address => Error? Should maybe lock up system?
;blockSize counts the number of paragraphs FROM .program TO THE NEXT MCB!
;------------------------------------------------------------------------;
; For Int 41h, the callers registers are saved on their stack in the 
; following order, thus allowing for return values to be placed in the 
; registers by accessing the caller register stack frame.
;------------------------------------------------------------------------;
struc callerFrame
    .rax    resq 1  ;Bottom of frame, pointer to rax saved in oldRSP
    .rbx    resq 1    
    .rcx    resq 1
    .rdx    resq 1
    .rsi    resq 1
    .rdi    resq 1
    .rbp    resq 1
    .r8     resq 1  ;To be removed if truly not needed
    .r9     resq 1
;Pushed by Processor when invoked via Interrupt
    .rip    resq 1
    .cs     resq 1
    .flags  resq 1
endstruc

struc dpl
;DOS parameter list, pointer to this stack is passed to DOS in rdx on
; server calls
    .rax    resq 1
    .rbx    resq 1
    .rcx    resq 1
    .rdx    resq 1
    .rsi    resq 1
    .rdi    resq 1
    .r8     resq 1
    .r9     resq 1
    .align  resq 1
    .compID resq 1  ;Computer id, low word used only
    .procID resq 1  ;Process ID
endstruc

struc cfgFrame  ;Frame used for config.sys parsing
                    resq 1  ;Alignment qword
    .newBuffers     resq 1  ;New buffers value
    .newSFTVal      resq 1
    .newFCBSVal     resq 1
    .newProtFCBSVal resq 1  ;Number of protected FCBS
    .newLastdrive   resq 1
    .newStacks      resq 1  ;Ignored for now
    .newStackSize   resq 1  ;Ignored for now
    .newAllocSize   resq 1  ;Used to keep track of reallocation size
    .cfgHandle      resq 1  ;CONFIG.SYS handle
    .lastLine       resq 1  ;Set if the last line is current
    .endPtr         resq 1  ;Pointer to the end of the allocation space
    .linePtr        resq 1  ;Ptr to line being processed by CONFIG.SYS
endstruc

struc filename
;8.3 File name struct
    .fName  resb 8
    .fExt   resb 3
endstruc

struc ffBlock   ;The structure of the Find First structure at the DTA
    .driveNum   resb 1  ;Drive we are searching on
    .template   resb 11 ;Search template (? meaning anything)
    .attrib     resb 1  ;Search attribute
    .dirOffset  resd 1  ;32 byte entry within dir cluster
    .parDirClus resd 1  ;Parent Directory Cluster number
    .attribFnd  resb 1  ;Attrib of file found
    .fileTime   resw 1  ;File time
    .fileDate   resw 1  ;File date
    .fileSize   resd 1  ;Number of bytes
    .asciizName resb 13 ;ASCIIZ name with dot and terminating nul
endstruc

struc psp
    .return     resb 2  ;Should always be CDh 40h, same place as DOS
    .allocSize  resd 1  ;Number of paras in init alloc or when exiting as TSR.
                resd 1  ;Reserved 4 bytes
    .oldInt42h  resq 1  ;Int 42h pointer on overlay load
    .oldInt43h  resq 1  ;Int 43h pointer on overlay load
    .oldInt44h  resq 1  ;Int 44h pointer on overlay load
    .parentPtr  resq 1  ;Pointer to parent process PSP
    .externalJFTPtr:    ;Ptr to external JFT array (first byte AFTER MCB)
    .jobFileTbl resb 20 ;Main File handle array, JFT
    .envPtr     resq 1  ;Pointer to the environment
    .rspPtr     resq 1  ;Pointer to rsp on entry to Int 41h
    .jftSize    resw 1  ;JFT array size, 20 => PSP JFT in use
    .unixEntry  resb 3  ;Must always be CDh 41h CBh, same place as DOS
    .prevPSP    resq 1  ;Pointer to the previous PSP in chain (used by SHARE)
                resb 1  ;Reserved byte
;If xtraHdlSz > 20, then the QWORD at .jobFileTbl becomes a pointer
; to the actual in use JFT and the other 12 bytes are left undefined.
;If xtraHdlSz < 20 (DOS will never set it so), then it is considered as 20
;If the user tries to reduce handle count to 20 or less, then the PSP JFT is 
; always used and xtraHdlSz.
;Will need to add two SYSCALLS for future expansion:
;       1) Get Program Environment Pointer
;       2) Get Command Line arguments pointer
;The below are in the same place as in DOS.
    .cmdLineArgPtr: ;Symbol for future use
    .fcb1       resb 16  ;First FCB,    argument 1 
    .fcb2       resb 20  ;Second FCB,   argument 2
    .dta:   ;Pointer to the default DTA in the PSP
    .parmList   resb 1   ;Number of characters in command tail
    .progTail   resb 127 ;Default DTA/Program tail
endstruc

dfltJFTsize equ 20  ;Default size of PSP JFT

struc clkStruc
    .dateWord   resw 1
    .minutes    resb 1
    .hours      resb 1
    .hseconds   resb 1  ;Hundreths of seconds
    .seconds    resb 1
endstruc

struc countryStruc
    .dtfmt     resw 1   ;Date time format (2 bytes)
                        ;0 = month day year hh:mm:ss
                        ;1 = day month year hh:mm:ss
                        ;2 = year month day hh:mm:ss
    .curr      resb 5   ;ASCIIZ Currency symbol (5 chars)
    .thouSep   resb 2   ;ASCIIZ Thousands separator
    .dcmlSep   resb 2   ;ASCIIZ Decimal separator
    .dateSep   resb 2   ;ASCIIZ Date separator
    .timeSep   resb 2   ;ASCIIZ Time separator
    .currFmt   resb 1   ;Currency Format
                        ;0 = Symbol leads, without space
                        ;1 = Symbol follows, without space
                        ;2 = Symbol leads, one space
                        ;3 = Symbol follows, one space
                        ;4 = Symbol replace decimal separator
    .digtdpt   resb 1   ;Number of digits after the decimal point
    .timefmt   resb 1   ;Time format, Bit 0 = 0 => 12 hour, = 1 => 24 hour clock
    .mapaddr   resb 8   ;Case map address (0 is nulptr), needs fixing up
    .dataSep   resb 2   ;Data list separator
    .resv      resb 10  ;Reserve 10 bytes
endstruc

;60 4kb pages, giving a total of 264kb of space allocated to paging
;for a space of 64Gb
dosAPTsize  equ 60*4096 ;(APT = Additional Page Tables)
dosLowBase  equ 600h    ;Low load address

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