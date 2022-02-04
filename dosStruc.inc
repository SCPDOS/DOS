
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

    .bDriveNumber            resb 1  ;Drive number (0=A, 1=B ...)
    .bUnitNumber             resb 1  ;Unit number in device
    .bBytesPerSectorShift    resb 1  ;min 9 (512 bps), max 12 (4096 bps)
    .bMaxSectorInCluster     resb 1  ;(Maximum sector in cluster) - 1
;                                       i.e. (2^bSectorsPerClusterShift) - 1
    .bSectorsPerClusterShift resb 1  ;Sectors per cluster exponent
    .dFAToffset              resd 1  ;Vol rel offset of first FAT, in sectors
    .bNumberOfFATs           resb 1  ;Number of FATs
    .wNumberRootDirEntries   resw 1  ;In sectors
    .dClusterHeapOffset      resd 1  ;Start of data area, in sectors
    .dClusterCount           resd 1  ;Total number of clusters (volume size)
    .dFATlength              resd 1  ;FAT length, in sectors
    .dFirstClusterOfRootDir  resd 1  ;First Cluster of Root Directory, min 2
    .qDriverHeaderPtr        resq 1  ;Pointer to device driver header
    .bMediaDescriptor        resb 1  ;Media descriptor
    .bAccessFlag             resb 1  ;Access Flag (0 if accessed, else -1)
    .qNextDPBPtr             resq 1  ;Pointer to next DPB, -1 if at end
    .dFirstFreeCluster       resd 1  ;Starting cluster of free space search
    .dNumberOfFreeClusters   resd 1  ;Number of free clusters, -1 unknown

endstruc

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
; Flags: Bit 15 set = Network drive
;        Bit 14 set = Physical drive
;        Bit 13 set = JOIN'ed drive
;        Bit 12 set = SUBST'ed drive
;        Bit 7  set = Redirected, not network, drive (This is how MSCDEX worked)
endstruc

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
; DEFAULT FILES=15, MINIMUM FILES=5
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
    .wOpenMode   resw 1    ;Bit 15 set if opened via FCB
    .bFileAttrib resb 1    ;Regular File Attributes
    .wDeviceInfo resw 1    ;Includes Device number
    .qPtr        resq 1    ;IF char, ptr to device driver, ELSE DPBptr
    .dStartClust resd 1    ;Start cluster of file
    .wTime       resw 1    ;File Time
    .wDate       resw 1    ;File Date
    .dFileSize   resd 1    ;File Size, in bytes
    .dCurntOff   resd 1    ;Current Offset in file, in bytes
    .wRelClust   resw 1    ;Relative cluster in file of the last cluster accessed
    .dDirSect    resd 1    ;Relative number of sector containing directory entry 
    .bNumDirEnt  resb 1    ;Number of dir entry within sector (byte offset/32)
    .sFCBName    resb 11   ;8.3 Filename
    .qPSPOwner   resq 1    ;Address of PSP of program which first opened this file
    .dAbsClusr   resd 1    ;Absolute cluster of last cluster accessed
endstruc

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
    .driveNumber resb  1   ;Drive number the buffer is used for
    .bufferFlags resb  1   ;Standard Flags
    .bufferLBA   resd  1   ;LBA the buffer contains
    .bufFATcopy  resb  1   ;Number of copies of the FAT to write (1 if non-FAT)
    .bufFAToff   resb  1   ;Sector offset of the backup copy of the FAT on disk
    .driveDPBPtr resq  1   ;Pointer to the device DPB block
endstruc