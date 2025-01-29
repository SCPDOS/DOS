;No externally defined symbols, no internally exposed symbols.

;Driver internal equates
drvBlkTblL      equ 26  ;Space for 26 drive letters!
;IOCTL track table size (max number of sectors in a track is 63)
maxTrackTblSz   equ 63
maxAcc          equ 5   ;Maximum accesses to a remdev w/o timer/changeline

struc drvBlk
;--------------------------------------------------------------------
; Drvblk Header information
;--------------------------------------------------------------------
    .pLink      dq ?    ;Pointer to the next drive block
    .bBIOSNum   db ?    ;BIOS number, identifies physical drive
    .bDOSNum    db ?    ;DOS 0 based drive number, setLogicalDev may change it
;--------------------------------------------------------------------
; In-use BPB for current media in here. Always have space for FAT32 
;  volume. If FAT12/16, FAT32 fields are undefined (garbage gets 
;  left in those fields... not a problem)
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
;The below word is a legacy thing. It is only used in a CHS based IOCTL call.
    .wNumCyl    dw ?
    .sDfltBPB   db bpb32_size dup (?)   ;Default capacity BPB for drive
.dAccTime:          ;Last media check time if remdev
    .wPtnType   dd ?    
;--------------------------------------------------------------------
; Volume string and id for the drive described by this drvblk
;--------------------------------------------------------------------
    .volLab     db 11 dup (?)   ;Volume label string
                db ?            ;Null terminator for string
    .volId      dd ?            ;Volume serial number            
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
devChgLine  equ 2       ;Set if the drive has a changeline
devLockBPB  equ 4       ;Makes BuildBPB not build a BPB if set
devSameSec  equ 8       ;All sectors in track are same size
devMulti    equ 10h     ;BIOS Drive has many logical units (A/B share)
devOwnDrv   equ 20h     ;Owner for possibly shared physical drive (A/B share)
devSwap     equ 40h     ;Device change detected (If A swapped and B same as A)
devChg      equ 80h     ;H/W Dev params changed
devFmt      equ 100h    ;Disk reformatted (BPB changed)
devUnFmt    equ 200h    ;Disables reads/writes if set (rec. fixed only)

;Media byte bits
mbTwoHead   equ 1       ;Dual sided if bit set, single sided if not
mb8Sector   equ 2       ;8 sectors per track if bit set, 9 if not
mbRemDev    equ 4       ;Removable if set, Fixed if not.

;Partition types
ptnPrimary  equ 1       ;A primary MBR partition
ptnLogical  equ 2       ;A logical MBR partition
ptnActive   equ 80h     ;An active partition
ptnGpt      equ 4       ;A primary GPT partition
ptrUnknown  equ 8000h   ;An unknown partition
