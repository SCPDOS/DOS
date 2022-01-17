
;-------------------------------------------------------;
; This include file contains miscellaneous FAT driver   ;
; structures. exFAT will have it's own files at a later ;
; stage.                                                ;
;-------------------------------------------------------;

bpb STRUC          ;FAT 12 and 16 BPB

    jmpBoot    db 3 dup (?)
    oemName    db 8 dup (?)  ;OEM name
    bytsPerSec dw ?  ;Bytes per sector
    secPerClus db ?  ;Sectors per cluster
    revdSecCnt dw ?  ;Number of reserved sectors
    numFATs    db ?  ;Number of FATs on media
    rootEntCnt dw ?  ;Number of entries in Root directory
    totSec16   dw ?  ;Number of sectors on medium
    media      db ?  ;Media descriptor byte
    FATsz16    dw ?  ;Number of sectors per FAT
    secPerTrk  dw ?  ;Number of sectors per "track"
    numHeads   dw ?  ;Number of read "heads"
    hiddSec    dd ?  ;Number of hidden sectors
    totSec32   dd ?  ;32 bit count of sectors

    drvNum     db ?  ;Logical drive number (00h or 80h)
    reserved1  db ?  ;Reserved byte
    bootSig    db ?  ;Extended boot signature (29h)
    volID      dd ?  ;Volume serial number
    volLab     db 11 dup (?) ;Volume label string
    filSysType db 8 dup (?)  ;File system type string

bpb ENDS

bpb32 STRUC       ;FAT 32 BPB

    jmpBoot    db 3 dup (?)
    oemName    db 8 dup (?)  ;OEM name
    bytsPerSec dw ?  ;Bytes per sector
    secPerClus db ?  ;Sectors per cluster
    revdSecCnt dw ?  ;Number of reserved sectors
    numFATs    db ?  ;Number of FATs on media
    rootEntCnt dw ?  ;Number of entries in Root directory
    totSec16   dw ?  ;Number of sectors on medium
    media      db ?  ;Media descriptor byte
    FATsz16    dw ?  ;Number of sectors per FAT, must be 0 for FAT 32
    secPerTrk  dw ?  ;Number of sectors per "track"
    numHeads   dw ?  ;Number of read "heads"
    hiddSec    dd ?  ;Number of hidden sectors
    totSec32   dd ?  ;32 bit count of sectors

    FATsz32    dd ?  ;32 bit count of sectors occupied by one FAT
    extFlags   dw ?  ;Extended Flags word
    FSver      dw ?  ;File system version word, must be 0
    RootClus   dd ?  ;First Cluster of Root Directory
    FSinfo     dw ?  ;Sector number of FSINFO structure, usually 1
    BkBootSec  dw ?  ;Backup Boot sector, either 0 or 6
    reserved   db 12 dup (?) ;Reserved 12 bytes

    drvNum     db ?  ;Logical drive number (00h or 80h)
    reserved1  db ?  ;Reserved byte
    bootSig    db ?  ;Extended boot signature (29h)
    volID      dd ?  ;Volume serial number
    volLab     db 11 dup (?) ;Volume label string
    filSysType db 8 dup (?)  ;File system type string

bpb32 ENDS


bpbEx STRUC   ;exFAT BPB

    jmpBoot                db 3 dup (?) 
    oemName                db 8 dup (?) ;OEM name
    MustBeZero             db 53 dup (?) ;Must be 0, 53 bytes
    partitionOffset        dq ?  ;in sectors, 0 means ignore this field
    volumeLength           dq ?  ;Volume Length in sectors
    FAToffset              dd ?  ;Volume rel offset of first FAT, in sectors
    FATlength              dd ?  ;FAT length, in sectors
    clusterHeapOffset      dd ?  ;Start of data area, in sectors
    clusterCount           dd ?  ;Number of clusters on medium
    firstClusterOfRootDir  dd ?  ;First Cluster of Root Directory, min 2
    volumeSerialNum        dd ?  ;Volume Serial Number
    FSrevision             dw ?  ;Should be 0001 (v1.00)
    volumeFlags            dw ?  ;Volume Flags, refer to documentation
    bytesPerSectorShift    db ?  ;min 9 (512 bps), max 12 (4096 bps)
    sectorsPerClusterShift db ?  ;Result of log_2(N) for N=sec per clus
    numberOfFATs           db ?  ;Number of FATs, only 1 or 2
    driveSelect            db ?  ;Drive Select, 0 or 80h (Int 13h)
    percentInUse           db ?  ;Rounded down. FFh means unknown
    reserved               db 7 dup (?)  ;Reserved for alignment

bpbEx ENDS