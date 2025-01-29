
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
    .bytsPerSec dw ?  ;Bytes per sector
    .secPerClus db ?  ;Sectors per cluster
    .revdSecCnt dw ?  ;Number of reserved sectors, in volume
    .numFATs    db ?  ;Number of FATs on media
    .rootEntCnt dw ?  ;Number of 32 byte entries in Root directory
    .totSec16   dw ?  ;Number of sectors on medium
    .media      db ?  ;Media descriptor byte
    .FATsz16    dw ?  ;Number of sectors per FAT
    .secPerTrk  dw ?  ;Number of sectors per "track"
    .numHeads   dw ?  ;Number of read "heads"
    .hiddSec    dd ?  ;Number of hidden sectors, preceeding volume start
    .totSec32   dd ?  ;32 bit count of sectors
endstruc

struc extBs
;Extended bootsector structure. If present, comes immediately after 
; BPB in bootsector
    .drvNum     db ?  ;Logical drive number (00h or 80h)
    .reserved1  db ?  ;Reserved byte
    .bootSig    db ?  ;Extended boot signature
    .volId      dd ?  ;Volume serial number
    .volLab     db 11 dup (?) ;Volume label string
    .filSysType db 8 dup (?)  ;File system type string
endstruc
extBsSig    equ 29h

struc bpb32       ;FAT 32 BPB
    .bytsPerSec dw ?  ;Bytes per sector
    .secPerClus db ?  ;Sectors per cluster
    .revdSecCnt dw ?  ;Number of reserved sectors
    .numFATs    db ?  ;Number of FATs on media
    .rootEntCnt dw ?  ;Number of entries in Root directory
    .totSec16   dw ?  ;Number of sectors on medium
    .media      db ?  ;Media descriptor byte
    .FATsz16    dw ?  ;Number of sectors per FAT, must be 0 for FAT 32
    .secPerTrk  dw ?  ;Number of sectors per "track"
    .numHeads   dw ?  ;Number of read "heads"
    .hiddSec    dd ?  ;Number of hidden sectors
    .totSec32   dd ?  ;32 bit count of sectors
    .FATsz32    dd ?  ;32 bit count of sectors occupied by one FAT
    .extFlags   dw ?  ;Extended Flags word
    .FSver      dw ?  ;File system version word, must be 0
    .RootClus   dd ?  ;First Cluster of Root Directory
    .FSinfo     dw ?  ;Sector number of FSINFO structure, usually 1
    .BkBootSec  dw ?  ;Backup Boot sector, either 0 or 6
    .reserved   db 12 dup (?) ;Reserved 12 bytes
endstruc

struc FSInfo
    .leadSig    dd ? ;Leading Signature, should be 041615252h
    .reserved1  db 480 dup (?)
    .strucSig   dd ?  ;Should be 061417272h
    .freeCount  dd ?  ;Contains the last known free cluster on volume
    .nextFree   dd ?  ;Contains the first known free cluster on volume
    .reserved2  db 12 dup (?) ;Should be 0
    .trailSig   dd ?  ;Should be 0AA550000h
endstruc

fsInfoSig1  equ 041615252h
fsInfoSig2  equ 061417272h
fsInfoSig3  equ 0AA550000h

struc mbr
    .bsCode     db 440 dup (?)
    .diskID     dd ?
    .reserved   dw ?
    .mbrEntry1  db 16 dup (?)
    .mbrEntry2  db 16 dup (?)
    .mbrEntry3  db 16 dup (?)
    .mbrEntry4  db 16 dup (?)
    .mbrSig     dw ?
endstruc

struc ebr   ;A special type of MBR
    .bsCode     db 446 dup (?)    ;Basic 16-bit code to error if launched into
    .ebrVolHdr  db 16 dup (?) ;Information about the volume itself
    .ebrLinkHdr db 16 dup (?) ;Link ptr to next logical volume in ebr
    .reserved   db 32 dup (?) ;Reserved
    .mbrSig     dw ?
endstruc

struc mbrEntry
    .ptnAtrib   db ?  ;80h = active
    .chsStart   db 3 dup (?)  ;Ignore practically
    .ptnType    db ?  ;This being 00 means free
    .chsEnd     db 3 dup (?)  ;Ignore
    .lbaStart   dd ?  ;Start of partition in 32 bit LBA
    .numSectors dd ?  ;Number of sectors in partition
endstruc

struc fatDirEntry
    .name       db 11 dup (?) ;8.3 File Name w/o fullstop
    .attribute  db ?  ;Usual attributes
    .ntRes      db ?  ;Reserved 0
    .crtTimeT   db ?  ;Count of tenths of a second of file creation time <=199
    .crtTime    dw ?  ;Creation time, granularity of 2 seconds
    .crtDate    dw ?  ;Creation date
    .lastAccDat dw ?  ;Last Read/Write date, not 100% supported (consider later)
    .fstClusHi  dw ?  ;Hi word of 1st data cluster for file/dir, 0 on FAT12/16
    .wrtTime    dw ?  ;Last modification (write) time
    .wrtDate    dw ?  ;Last modification (write) date
    .fstClusLo  dw ?  ;Lo word of 1st data cluster for file/dir
    .fileSize   dd ?  ;32-bit quantity with size of file described by entry
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

    ;.jmpBoot                db 3 dup (?)
    ;.oemName                db 8 dup (?)  ;OEM name
;BPB starts here
    .MustBeZero             db 53 dup (?) ;Must be 0, 53 bytes
    .partitionOffset        dq ?  ;in sectors, 0 means ignore this field
    .volumeLength           dq ?  ;Volume Length in sectors
    .FAToffset              dd ?  ;Volume rel offset of first FAT, in sectors
    .FATlength              dd ?  ;FAT length, in sectors
    .clusterHeapOffset      dd ?  ;Start of data area, in sectors
    .clusterCount           dd ?  ;Number of clusters on medium
    .firstClusterOfRootDir  dd ?  ;First Cluster of Root Directory, min 2
    .volumeSerialNum        dd ?  ;Volume Serial Number
    .FSrevision             dw ?  ;Should be 0001 (v1.00)
    .volumeFlags            dw ?  ;Volume Flags, refer to documentation
    .bytesPerSectorShift    db ?  ;min 9 (512 bps), max 12 (4096 bps)
    .sectorsPerClusterShift db ?  ;Result of log_2(N) for N=sec per clus
    .numberOfFATs           db ?  ;Number of FATs, only 1 or 2
    .driveSelect            db ?  ;Drive Select, 0 or 80h (Int 13h)
    .percentInUse           db ?  ;Rounded down. FFh means unknown
    .reserved               db 7 dup (?)  ;Reserved for alignment

endstruc