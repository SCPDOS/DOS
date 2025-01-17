
;-------------------------------------------------------;
; This include file contains miscellaneous FAT driver   ;
; structures. exFAT will have it's own files at a later ;
; stage.                                                ;
;-------------------------------------------------------;
;-------------------------------------------------------;
;-------------------------------------------------------;
;  IMPORTANT!!! IMPORTANT!!! IMPORTANT!!! IMPORTANT!!!  ;
;   REMEMBER, CLUSTER NUMBERS START AT 2!               ;
;   THE FIRST DATA CLUSTER ON A DISK IS CLUSTER 2!!!    ;
;   ON FAT12/16, A CLUSTER NUMBER OF 0 MEANS ROOT DIR!  ;
;   ON FAT32, A CLUSTER NUMBER OF 0 ALIASES THE FIRST   ;
;       CLUSTER OF THE ROOT DIRECTORY!!                 ;
;  IMPORTANT!!! IMPORTANT!!! IMPORTANT!!! IMPORTANT!!!  ;
;-------------------------------------------------------;

struc bpb          ;FAT 12 and 16 BPB
    .bytsPerSec resw 1  ;Bytes per sector
    .secPerClus resb 1  ;Sectors per cluster
    .revdSecCnt resw 1  ;Number of reserved sectors, in volume
    .numFATs    resb 1  ;Number of FATs on media
    .rootEntCnt resw 1  ;Number of 32 byte entries in Root directory
    .totSec16   resw 1  ;Number of sectors on medium
    .media      resb 1  ;Media descriptor byte
    .FATsz16    resw 1  ;Number of sectors per FAT
    .secPerTrk  resw 1  ;Number of sectors per "track"
    .numHeads   resw 1  ;Number of read "heads"
    .hiddSec    resd 1  ;Number of hidden sectors, preceeding volume start
    .totSec32   resd 1  ;32 bit count of sectors
endstruc

struc extBs
;Extended bootsector structure. If present, comes immediately after 
; BPB in bootsector
    .drvNum     resb 1  ;Logical drive number (00h or 80h)
    .reserved1  resb 1  ;Reserved byte
    .bootSig    resb 1  ;Extended boot signature
    .volId      resd 1  ;Volume serial number
    .volLab     resb 11 ;Volume label string
    .filSysType resb 8  ;File system type string
endstruc
extBsSig    equ 29h

struc bpb32       ;FAT 32 BPB
    .bytsPerSec resw 1  ;Bytes per sector
    .secPerClus resb 1  ;Sectors per cluster
    .revdSecCnt resw 1  ;Number of reserved sectors
    .numFATs    resb 1  ;Number of FATs on media
    .rootEntCnt resw 1  ;Number of entries in Root directory
    .totSec16   resw 1  ;Number of sectors on medium
    .media      resb 1  ;Media descriptor byte
    .FATsz16    resw 1  ;Number of sectors per FAT, must be 0 for FAT 32
    .secPerTrk  resw 1  ;Number of sectors per "track"
    .numHeads   resw 1  ;Number of read "heads"
    .hiddSec    resd 1  ;Number of hidden sectors
    .totSec32   resd 1  ;32 bit count of sectors
    .FATsz32    resd 1  ;32 bit count of sectors occupied by one FAT
    .extFlags   resw 1  ;Extended Flags word
    .FSver      resw 1  ;File system version word, must be 0
    .RootClus   resd 1  ;First Cluster of Root Directory
    .FSinfo     resw 1  ;Sector number of FSINFO structure, usually 1
    .BkBootSec  resw 1  ;Backup Boot sector, either 0 or 6
    .reserved   resb 12 ;Reserved 12 bytes
endstruc

struc FSInfo
    .leadSig    resb 4  ;Leading Signature, should be 041615252h
    .reserved1  resb 480
    .strucSig   resb 4  ;Should be 061417272h
    .freeCount  resb 4  ;Contains the last known free cluster on volume
    .nextFree   resb 4  ;Contains the first known free cluster on volume
    .reserved2  resb 12 ;Should be 0
    .trailSig   resb 4  ;Should be 0AA550000h
endstruc

fsInfoSig1  equ 041615252h
fsInfoSig2  equ 061417272h
fsInfoSig3  equ 0AA550000h

struc drvBlk
;--------------------------------------------------------------------
; Drvblk Header information
;--------------------------------------------------------------------
    .pLink      dq ?    ;Pointer to the next drive block
    .bDOSNum    db ?    ;DOS 0 based drive number, setLogicalDev may change it
    .bBIOSNum   db ?    ;BIOS number, identifies physical drive
;--------------------------------------------------------------------
; In-use BPB for current media in here. Always have space for FAT32 
;  volume. If FAT12/16, FAT32 fields are undefined (garbage gets 
;  copied into those fields... not a problem)
;--------------------------------------------------------------------
.bpb:
    .wBpS       dw ?    ;Bytes per sector
    .bSpC       db ?    ;Sectors per cluster
    .wResC      dw ?    ;Number of reserved sectors
    .bNumFAT    db ?    ;Number of FATs on media
    .wRtCntNum  dw ?    ;Number of entries in Root directory
    .wTotSec16  dw ?    ;Number of sectors on medium
    .bMedDesc   db ?    ;Media descriptor byte
    .wFATsz16   dw ?    ;Number of sectors per FAT, must be 0 for FAT 32
    .wSecPerTrk dw ?    ;Number of sectors per "track"
    .wNumHeads  dw ?    ;Number of read "heads"
    .dHiddSec   dd ?    ;Number of hidden sectors
    .dTotSec32  dd ?    ;32 bit count of sectors
;--------------------------------------------------------------------
; The below vars are only accessed on FAT32 volumes
;--------------------------------------------------------------------
    .FATsz32    dd ?    ;32 bit count of sectors occupied by one FAT
    .extFlags   dw ?    ;Extended Flags word
    .FSver      dw ?    ;File system version word, must be 0
    .RootClus   dd ?    ;First Cluster of Root Directory
    .FSinfo     dw ?    ;Sector number of FSINFO structure, usually 1
    .BkBootSec  dw ?    ;Backup Boot sector, either 0 or 6
;--------------------------------------------------------------------
; DrvBlk flags
;--------------------------------------------------------------------
    .bBpbType   db ?    ;BPB Type indicator (FAT12/16/32 or other)
    .wOpenCnt   dw ?    ;Device open count (make dword?)
    .bDevType   db ?    ;Device type byte (21/440Dh type subcode)
    .wDevFlgs   dw ?    ;Flags for this device 
    .sMaxBPB    db bpb32_size dup (?)   ;Max capacity BPB for drive
.dAccTime:          ;Last access time if remdev (0 for now)
    .wPtnType   dd ?    
;--------------------------------------------------------------------
; Volume string and id for the drive described by this drvblk
;--------------------------------------------------------------------
    .volId      dd ?            ;Volume serial number
    .volLab     db 11 dup (?)   ;Volume label string
                db ?            ;Null terminator for string
    .filSysType db 8 dup (?)    ;File system type string
                db ?            ;Null terminator for string
endstruc

;FAT type values
bpbDskOff   equ 80h ;All Disk accesses return Not Ready (Unrecognised FAT)
bpbFat16    equ 40h ;FAT 16 disk
bpbFat32    equ 20h ;FAT 32 disk
bpbFat12    equ 10h ;FAT 12 disk

;Device Flag values
devFixed    equ 1       ;Set if fixed disk 
devChgLine  equ 2       ;Always set for us
devLockBPB  equ 4       ;Make BuildBPB fail
devSameSec  equ 8       ;All sectors in track are same size (always set)
devMulti    equ 10h     ;BIOS Drive has many logical units (A/B share)
devOwnMulti equ 20h     ;Owner for shared physical drive (A/B share)
devSwap     equ 40h     ;Device change detected (If A swapped but B same as A)
devChg      equ 80h     ;H/W Dev params changed (NOT USED, ALWAYS OFF)
devFmt      equ 100h    ;Disk reformatted (BPB changed)
devUnFmt    equ 200h    ;Fixed media only, disables reads/writes if set

;Media byte bits
mbTwoHead   equ 1       ;Dual sided if bit set, single sided if not
mb8Sector   equ 2       ;8 sectors per track if bit set, 9 if not
mbRemDev    equ 4       ;Removable if set, Fixed if not.


struc mbr
    .bsCode     resb 440
    .diskID     resb 4
    .reserved   resb 2
    .mbrEntry1  resb 16
    .mbrEntry2  resb 16
    .mbrEntry3  resb 16
    .mbrEntry4  resb 16
    .mbrSig     resb 2
endstruc

struc mbrEntry
    .ptnAtrib   resb 1  ;80h = active
    .chsStart   resb 3  ;Ignore practically
    .ptnType    resb 1  ;This being 00 means free
    .chsEnd     resb 3  ;Ignore
    .lbaStart   resb 4  ;Start of partition in 32 bit LBA
    .numSectors resb 4  ;Number of sectors in partition
endstruc

struc ebr
    .bsCode     resb 446    ;Basic 16-bit code to error if launched into
    .ebrVolHdr  resb 16 ;Information about the volume itself
    .ebrLinkHdr resb 16 ;Link header information to next logical volume in ebr
    .reserved   resb 32 ;Reserved
    .mbrSig     resb 2
endstruc


struc fatDirEntry

    .name       resb 11 ;8.3 File Name w/o fullstop
    .attribute  resb 1  ;Usual attributes
    .ntRes      resb 1  ;Reserved 0
    .crtTimeT   resb 1  ;Count of tenths of a second of file creation time <=199
    .crtTime    resb 2  ;Creation time, granularity of 2 seconds
    .crtDate    resb 2  ;Creation date
    .lastAccDat resb 2  ;Last Read/Write date, not 100% supported (consider later)
    .fstClusHi  resb 2  ;Hi word of 1st data cluster for file/dir, 0 on FAT12/16
    .wrtTime    resb 2  ;Last modification (write) time
    .wrtDate    resb 2  ;Last modification (write) date
    .fstClusLo  resb 2  ;Lo word of 1st data cluster for file/dir
    .fileSize   resb 4  ;32-bit quantity with size of file described by entry

endstruc
;Directory attribute equates
    dirReadOnly     equ 01h
    dirHidden       equ 02h
    dirSystem       equ 04h
    dirVolumeID     equ 08h
    dirDirectory    equ 10h
    dirArchive      equ 20h
    dirCharDev      equ 40h ;Never written to disk, used to represent a Char Dev
    dirLongName     equ dirReadOnly | dirHidden | dirSystem | dirVolumeID
    ;If any of the three bits are set, then ALL three bits are set
    ; in addition to whatever the user passed to search for.
    dirInclusive    equ dirHidden | dirSystem | dirDirectory
    dirIncFiles     equ dirHidden | dirSystem

struc bpbEx   ;exFAT BPB, SIZE: 120 bytes

    .jmpBoot                resb 3 
    .oemName                resb 8  ;OEM name
    .MustBeZero             resb 53 ;Must be 0, 53 bytes
    .partitionOffset        resq 1  ;in sectors, 0 means ignore this field
    .volumeLength           resq 1  ;Volume Length in sectors
    .FAToffset              resd 1  ;Volume rel offset of first FAT, in sectors
    .FATlength              resd 1  ;FAT length, in sectors
    .clusterHeapOffset      resd 1  ;Start of data area, in sectors
    .clusterCount           resd 1  ;Number of clusters on medium
    .firstClusterOfRootDir  resd 1  ;First Cluster of Root Directory, min 2
    .volumeSerialNum        resd 1  ;Volume Serial Number
    .FSrevision             resw 1  ;Should be 0001 (v1.00)
    .volumeFlags            resw 1  ;Volume Flags, refer to documentation
    .bytesPerSectorShift    resb 1  ;min 9 (512 bps), max 12 (4096 bps)
    .sectorsPerClusterShift resb 1  ;Result of log_2(N) for N=sec per clus
    .numberOfFATs           resb 1  ;Number of FATs, only 1 or 2
    .driveSelect            resb 1  ;Drive Select, 0 or 80h (Int 13h)
    .percentInUse           resb 1  ;Rounded down. FFh means unknown
    .reserved               resb 7  ;Reserved for alignment

endstruc

%macro defaultBPB 0
    db 0E9h
    db 00h
    db 90h
    db 'SCPDOSv1'
    dw 0200h                       
    db 01h                         
    dw 0001h                           
    db 02h                              
    dw 00E0h            
    dw 0B40h   
    db 0F0h                   
    dw 0009h                   
    dw 0012h                     
    dw 0002h                  
    dd 0                 
    dd 0 
    db 00h
    db 00h
    db 29h
    dd 0
    db 'NO NAME    '
    db 'FAT12   '
%endmacro