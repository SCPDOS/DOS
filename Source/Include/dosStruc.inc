
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
    .bBytesPerSectorShift    resb 1  ;min 9 (512 bps), max 12 (4096 bps)x
    .bMaxSectorInCluster     resb 1  ;(Maximum sector in cluster) - 1 x
    .bSectorsPerClusterShift resb 1  ;Sectors per cluster exponent x
    .wFAToffset              resw 1  ;Number of reserved sectors in partition x
    .bNumberOfFATs           resb 1  ;Number of FATs x
    .wNumberRootDirSectors   resw 1  ;In sectors, this is 0 for FAT32 formats x
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
;        Bit 7  set = Redirected, not network, drive (This is how MSCDEX worked)
endstruc
;CDS equates
    cdsNetDrive     equ 8000h
    cdsPhysDrive    equ 4000h
    cdsJoinDrive    equ 2000h
    cdsSubstDrive   equ 1000h
    cdsRedirDrive   equ 0080h
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
; DEFAULT FCBS=5, MINIMUM FCBS=5
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
    .wDeviceInfo resw 1    ;Includes Device number
    .qPtr        resq 1    ;IF char, ptr to device driver, ELSE DPBptr
    .dStartClust resd 1    ;Start cluster of file
    .wTime       resw 1    ;File Time
    .wDate       resw 1    ;File Date
    .dFileSize   resd 1    ;File Size, in bytes
    .dCurntOff   resd 1    ;Current Offset in file, in bytes
    .dRelClust   resd 1    ;Rel cluster in file, the last cluster read
    .qDirSect    resq 1    ;Abs number of sector containing directory entry 
    .bNumDirEnt  resb 1    ;Directory entry within dir sector (byte offset/32)
    .sFileName   resb 11   ;8.3 Filename
    .qPSPOwner   resq 1    ;Addr of PSP of program which first opened this file
    .dAbsClusr   resd 1    ;Absolute cluster of last cluster accessed
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
;Bit 7 reserved 0
;Bits 8-12 reserved 0
;Critical Error handling
    critErrHdl      equ 2000h   ;If set, return error code. If clear, INT 44h
;Buffering on writes
    noBufferWrites  equ 4000h   ;If set, dont buffer writes, always update disk
;FCB opened file?
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
    devBinary       equ 20h ;If set, r/w device in Binary mode!!!
    charDevNoEOF    equ 40h ;If set, no EOF when reading from device!!
    blokDevNotFlush equ 40h ;If set, file not written to disk yet!!!
    devCharDev      equ 80h ;If set, it is a char device!
    charDevNetSpool equ 800h    ;If set, device is a network spooler
    devNoInherit    equ 1000h   ;If set, handle not inherited by child processes
    charDevNamePipe equ 2000h   ;If set, device is a named pipe
    blokDevDTSet    equ 4000h   ;If set, date and time updated for all i/o
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
    .reserved    resb  1   ;Alignment byte
    .dataarea:             ;Points to the start of the data area
endstruc
;Buffer Flag equates
    dosBuffer   equ 1h  ;Sector 0 (BPB/VPB) in buffer
    fatBuffer   equ 2h  ;FAT sector in buffer
    dirBuffer   equ 4h  ;Directory sector in buffer
    dataBuffer  equ 8h  ;File Data sector in buffer
    refBuffer   equ 20h ;Data in buffer referenced by an application
    dirtyBuffer equ 40h ;Data in buffer modified by an application
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

struc fcb
;Similar to a DOS 2.0 FCB. Who even used them in DOS 3.x and higher?
    .driveNum   resb 1
    .filename   resb 8
    .fileext    resb 3
    .curBlock   resb 2  ;Current block number relative to 0
;One block consists of 128 records, of size denoted in the below
    .recordSize resb 2  ;Record size, default 80h
    .fileSize   resb 4  ;File size, in bytes
    .fileDate   resb 2  ;Standard date format
    .fileTime   resb 2  ;Standard time format
    .fcbByte    resb 1  ;Res. byte, bit7 = Logical drive, bit 6 = open file
    .cluster    resb 4  ;Res. dword, Current Cluster number on disk
    .reserved   resb 3  ;Res. 3 alignment bytes
    .recordNum  resb 1  ;Current record in block
    .randRecrd  resb 4  ;Relative record number, relative to start of file
    ;If record size > 64, high byte is ignored.
    ;Used for random read and write operations to find the right record
endstruc

struc ffBlock   ;The structure of the Find First structure at the DTA
    .driveNum   resb 1  ;Drive we are searching on
    .template   resb 11 ;Search template (? meaning anything)
    .attrib     resb 1  ;Search attribute
    .dirOffset  resd 1  ;Entry within dir (made a dword but will never be so)
    .parCluster resd 1  ;Parent cluster number
    .atrribFnd  resb 1  ;Attrib of file found
    .fileTime   resw 1  ;File time
    .fileDate   resw 1  ;File date
    .fileSize   resd 1  ;Number of bytes
    .asciizName resb 13 ;ASCIIZ name with dot and terminating nul
endstruc

struc psp
    .return     resb 2  ;Should always be CDh 40h, same place as DOS
    .startSeg   resq 1  ;Pointer to the start of the PSP
    .oldInt42h  resq 1  ;Int 42h pointer on overlay load
    .oldInt43h  resq 1  ;Int 43h pointer on overlay load
    .oldInt44h  resq 1  ;Int 44h pointer on overlay load
    .parentPtr  resq 1  ;Pointer to parent process PSP
    .jobFileTbl resb 20 ;Main File handle array, JFT
    .envPtr     resq 1  ;Pointer to the environment
    .rspPtr     resq 1  ;Pointer to rsp on entry to Int 41h
    .xtraHdlSz  resw 1  ;Additional File Handle array size, 0 by default
    .unixEntry  resb 3  ;Must always be CDh 41h CBh, same place as DOS
    .prevPSP    resq 1  ;Pointer to the previous PSP in chain
    .xtraHdlNum resb 1  ;Lookup number of extra handle array, -1 = unused
;Extra handle arrays are grouped into a linked list
;The below are in the same place as in DOS
    .fcb1       resb 16  ;First FCB
    .fcb2       resb 20  ;Second FCB
    .dta:   ;Pointer to the default DTA in the PSP
    .parmList   resb 1   ;Number of characters in command tail
    .progTail   resb 127 ;Default DTA/Program tail
endstruc

struc clkStruc
    .dateWord   resw 1
    .minutes    resb 1
    .hours      resb 1
    .hseconds   resb 1  ;Hundreths of seconds
    .seconds    resb 1
endstruc

;60 4kb pages, giving a total of 264kb of space allocated to paging
;for a space of 64Gb
dosAPTsize  equ 60*4096