
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

struc bpb          ;FAT 12 and 16 BPB, SIZE: 62 bytes

    .jmpBoot    resb 3 
    .oemName    resb 8  ;OEM name
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

    .drvNum     resb 1  ;Logical drive number (00h or 80h)
    .reserved1  resb 1  ;Reserved byte
    .bootSig    resb 1  ;Extended boot signature (29h)
    .volID      resd 1  ;Volume serial number
    .volLab     resb 11 ;Volume label string
    .filSysType resb 8  ;File system type string

endstruc

struc bpb32       ;FAT 32 BPB, SIZE: 90 bytes

    .jmpBoot    resb 3
    .oemName    resb 8  ;OEM name
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

    .drvNum     resb 1  ;Logical drive number (00h or 80h)
    .reserved1  resb 1  ;Reserved byte
    .bootSig    resb 1  ;Extended boot signature (29h)
    .volID      resd 1  ;Volume serial number
    .volLab     resb 11 ;Volume label string
    .filSysType resb 8  ;File system type string

endstruc

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
    .ptnType    resb 1
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
    .lastAccDat resb 2  ;Last Read/Write date
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
    dirLongName     equ dirReadOnly | dirHidden | dirSystem | dirVolumeID

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

struc FSInfo

    .leadSig    resb 4  ;Leading Signature, should be 041615252h
    .reserved1  resb 480
    .strucSig   resb 4  ;Should be 061417272h
    .freeCount  resb 4  ;Contains the last known free cluster on volume
    .nextFree   resb 4  ;Contains the first known free cluster on volume
    .reserved2  resb 12 ;Should be 0
    .trailSig   resb 4  ;Should be 0AA550000h

endstruc