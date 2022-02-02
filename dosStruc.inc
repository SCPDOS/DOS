
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
dpb STRUC        ;Drive Parameter Block

    bDriveNumber            db ?  ;Drive number (0=A, 1=B ...)
    bUnitNumber             db ?  ;Unit number in device
    bBytesPerSectorShift    db ?  ;min 9 (512 bps), max 12 (4096 bps)
    bMaxSectorInCluster     db ?  ;(Maximum sector in cluster) - 1
;                                       i.e. (2^bSectorsPerClusterShift) - 1
    bSectorsPerClusterShift db ?  ;Sectors per cluster exponent
    dFAToffset              dd ?  ;Vol rel offset of first FAT, in sectors
    bNumberOfFATs           db ?  ;Number of FATs
    wNumberRootDirEntries   dw ?  ;In sectors
    dClusterHeapOffset      dd ?  ;Start of data area, in sectors
    dClusterCount           dd ?  ;Total number of clusters (volume size)
    dFATlength              dd ?  ;FAT length, in sectors
    dFirstClusterOfRootDir  dd ?  ;First Cluster of Root Directory, min 2
    qDriverHeaderPtr        dq ?  ;Pointer to device driver header
    bMediaDescriptor        db ?  ;Media descriptor
    bAccessFlag             db ?  ;Access Flag (0 if accessed, else -1)
    qNextDPBPtr             dq ?  ;Pointer to next DPB, -1 if at end
    dFirstFreeCluster       dd ?  ;Starting cluster of free space search
    dNumberOfFreeClusters   dd ?  ;Number of free clusters, -1 unknown

dpb ENDS

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
cds STRUC       ;Current Directory Structure
    sCurrentPath        db 67 dup(?)    ;Current Directory Path String
    wFlags              dw ? ;CDS Flags - 
    qDPBPtr             dq ? ;Ptr to the DPB of the dev using this CDS
    dStartCluster       dd ? ;Starting cluster of the directory on disk
    qReserved           dq ? ;Reserved for future expansions
    wBackslashOffset    dw ? ;How many chars to the start of current dir in the 
; currentPath (usually 2 to skip C: in C:\, but when SUBSTing a drive letter to 
; another, we save the number of chars to skip to that deep level).
; I.E. if we SUBST A:\DOS G: then this value for the CDS for drive G is 6
; Flags: Bit 15 set = Network drive
;        Bit 14 set = Physical drive
;        Bit 13 set = JOIN'ed drive
;        Bit 12 set = SUBST'ed drive
;        Bit 7  set = Redirected, not network, drive (This is how MSCDEX worked)
cds ENDS

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
sfth STRUC   ;System file table header
; This is the SFT header.
; This structure comes before the start of the first file in the array/table.
    qNextSFTPtr dq ?    ;Pointer to the next SFT
    wNumFiles   dw ?    ;Number of files in this table
sfth ENDS

sft STRUC
;This is the file struc itself
    wNumHandles dw ?    ;Number of handles pointing to this file
    wOpenMode   dw ?    ;Bit 15 set if opened via FCB
    bFileAttrib db ?    ;Regular File Attributes
    wDeviceInfo dw ?    ;Includes Device number
    qPtr        dq ?    ;IF char, ptr to device driver, ELSE DPBptr
    dStartClust dd ?    ;Start cluster of file
    wTime       dw ?    ;File Time
    wDate       dw ?    ;File Date
    dFileSize   dd ?    ;File Size, in bytes
    dCurntOff   dd ?    ;Current Offset in file, in bytes
    wRelClust   dw ?    ;Relative cluster in file of the last cluster accessed
    dDirSect    dd ?    ;Relative number of sector containing directory entry 
    bNumDirEnt  db ?    ;Number of dir entry within sector (byte offset/32)
    sFCBName    db 11 dup(?)    ;8.3 Filename
    qPSPOwner   dq ?    ;Address of PSP of program which first opened this file
    dAbsClusr   dd ?    ;Absolute cluster of last cluster accessed
sft ENDS

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

bufferHdr   STRUC
    nextBufPtr  dq  ?   ;Pointer to the next buffer in the chain or -1 at end
    driveNumber db  ?   ;Drive number the buffer is used for
    bufferFlags db  ?   ;Standard Flags
    bufferLBA   dd  ?   ;LBA the buffer contains
    bufFATcopy  db  ?   ;Number of copies of the FAT to write (1 if non-FAT)
    bufFAToff   db  ?   ;Sector offset of the backup copy of the FAT on disk
    driveDPBPtr dq  ?   ;Pointer to the device DPB block
bufferHdr   ENDS