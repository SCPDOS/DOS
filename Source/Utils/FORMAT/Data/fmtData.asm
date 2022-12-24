;IOCTL request header
reqTable    db genioctlGetParamsTable_size dup (0)

;Data area here
oldDrive    db -1       ;Old default drive (0 based)
fmtDrive    db -1       ;Drive we are operating on (0 based)
fmtDrvInv   db 0        ;If set to -1, the drive needs to be reactivated
cdsPtr      dq 0        ;CDS ptr here
bufferArea  dq 0        ;Ptr to the buffer area
;Format Data here
remDev      db 0        ;0 = Removable, -1 = Fixed
fatType     db -1       ;0 = FAT12, 1 = FAT16, 2 = FAT32, -1 = No FAT
sectorSize  dw 0        ;Sector size in bytes
numSectors  dq 0        ;Number of sectors in volume
secPerClust db 0        ;Copy the sectors per cluster over
bpbPointer  dq 0        ;Pointer to the BPB we will use
bpbSize     db 0        ;Size of the BPB

;Tables
;Each row is 5 bytes, {DWORD, BYTE} with DWORD = diskSize, BYTE=secPerClusVal
fat16ClusterTable:
    dd 8400 ;Disks up to 4.1MB, must use FAT12 with 0.5 K clusters
    db 1    ;ALL FAT12 uses 1, unless it is a preexisting meddesc type medium
    dd 32680    ; Disk up to 16MB, 1K clusters
    db 2
    dd 262144   ; Disk up to 128MB, 2K clusters
    db 4
    dd 524288   ; Disk up to 256MB, 4K clusters
    db 8
    dd 1048576  ; Disk up to 512Mb, 8K clusters
    db 16

fat32ClusterTable:
    dd 16777216 ; Disk up to 8GB, 4K clusters
    db 8        
    dd 33554432 ; Disk up to 16GB, 8K clusters
    db 16
    dd 67108864 ; Disk up to 32Gb, 16K clusters
    db 32
    dd -1       ; Disk up to 2TB, 32K clusters
    db 64

;Static BPBs here, fields set to -1 must be edited.
;Fields with a preset value should NOT be touched.
genericBPB12:
    istruc bpb
    at bpb.jmpBoot,     db 0E9h, 3Ch, 90h   ;60 bytes, jump forward by that 
    at bpb.oemName,     db 'SCPDOSv1'
    at bpb.bytsPerSec,  dw -1           ;512 bytes per sector, normally
    at bpb.secPerClus,  db -1           ;1 sector per cluster, normally
    at bpb.revdSecCnt,  dw 0001h        ;1 Reserved Sector
    at bpb.numFATs,     db 02h          ;2 FAT tables
    at bpb.rootEntCnt,  dw 00E0h        ;224 root entries
    at bpb.totSec16,    dw -1           ;Total number of sectors on disk
    at bpb.media,       db 0F0h         ;Media byte
    at bpb.FATsz16,     dw -1           ;9 FAT sectors, normally
    at bpb.secPerTrk,   dw 0012h        ;18 Sectors per track
    at bpb.numHeads,    dw 0002h        ;2 Heads
    at bpb.hiddSec,     dd 0            ;No hidden sectors
    at bpb.totSec32,    dd 0            ;Not a FAT32 BPB
    at bpb.drvNum,      db -1           ;Set to 80h if fixed
    at bpb.reserved1,   db 00h
    at bpb.bootSig,     db 29h          ;Extended BPB
    at bpb.volID,       dd -1           ;Set volume ID to time
    at bpb.volLab,      db 'NO NAME    '
    at bpb.filSysType,  db 'FAT12   '
    iend

genericBPB16:
    istruc bpb
    at bpb.jmpBoot,     db 0E9h, 3Ch, 90h   ;Jump forward by 60 bytes
    at bpb.oemName,     db 'SCPDOSv1'
    at bpb.bytsPerSec,  dw -1           ;512 bytes per sector, normally
    at bpb.secPerClus,  db -1           ;Sectors per cluster
    at bpb.revdSecCnt,  dw 0001h        ;1 Reserved Sector
    at bpb.numFATs,     db 02h          ;2 FAT tables
    at bpb.rootEntCnt,  dw 0200h        ;512 root entries
    at bpb.totSec16,    dw -1           ;Total number of sectors on disk
    at bpb.media,       db 0F0h         ;Media byte
    at bpb.FATsz16,     dw -1           ;Number of sectors per FAT
    at bpb.secPerTrk,   dw 0012h        ;18 Sectors per track
    at bpb.numHeads,    dw 0002h        ;2 Heads
    at bpb.hiddSec,     dd 0            ;No hidden sectors
    at bpb.totSec32,    dd 0            ;Not a FAT32 BPB
    at bpb.drvNum,      db -1           ;Set to 80h if fixed
    at bpb.reserved1,   db 00h
    at bpb.bootSig,     db 29h          ;Extended BPB
    at bpb.volID,       dd -1           ;Set volume ID to time
    at bpb.volLab,      db 'NO NAME    '
    at bpb.filSysType,  db 'FAT16   '
    iend

genericBPB32:
    istruc bpb32
    at bpb32.jmpBoot,     db 0E9h, 58h, 90h   ;Jump forward by 88 bytes
    at bpb32.oemName,     db 'SCPDOSv1'
    at bpb32.bytsPerSec,  dw -1           ;512 bytes per sector, normally
    at bpb32.secPerClus,  db -1           ;Sectors per cluster
    at bpb32.revdSecCnt,  dw 0001h        ;1 Reserved Sector
    at bpb32.numFATs,     db 02h          ;2 FAT tables
    at bpb32.rootEntCnt,  dw 0200h        ;512 root entries
    at bpb32.totSec16,    dw 0            ;Not a FAT 12/16 BPB
    at bpb32.media,       db 0F0h         ;Media byte
    at bpb32.FATsz16,     dw 0            ;Not a FAT 12/16 BPB
    at bpb32.secPerTrk,   dw 0012h        ;18 Sectors per track
    at bpb32.numHeads,    dw 0002h        ;2 Heads
    at bpb32.hiddSec,     dd 0            ;No hidden sectors
    at bpb32.totSec32,    dd -1           ;Total number of sectors on disk

    at bpb32.FATsz32,     dd -1  ;Number of sectors per FAT
    at bpb32.extFlags,    dw -1  ;Extended Flags word
    at bpb32.FSver,       dw 0   ;File system version word, must be 0
    at bpb32.RootClus,    dd -1  ;First Cluster of Root Directory
    at bpb32.FSinfo,      dw 1   ;Sector number of FSINFO structure, usually 1
    at bpb32.BkBootSec,   dw 6   ;Backup Boot sector, either 0 or 6
    at bpb32.reserved,    db 12 dup (0) ;Reserved 12 bytes

    at bpb32.drvNum,      db -1           ;Set to 80h if fixed
    at bpb32.reserved1,   db 00h
    at bpb32.bootSig,     db 29h          ;Extended BPB
    at bpb32.volID,       dd -1           ;Set volume ID to time
    at bpb32.volLab,      db 'NO NAME    '
    at bpb32.filSysType,  db 'FAT32   '
    iend    