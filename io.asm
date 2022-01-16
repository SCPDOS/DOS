    
    .x64p

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

dpb STRUC        ;Drive Parameter Block

    bDriveNumber               db ?  ;Drive number
    bUnitNumber                db ?  ;Unit number in device
    bBytesPerSectorShift       db ?  ;min 9 (512 bps), max 12 (4096 bps)
    bMaxSectorInCluster        db ?  ;(Maximum sector in cluster) - 1
;                                       i.e. (2^bSectorsPerClusterShift) - 1
    bSectorsPerClusterShift    db ?  ;Sectors per cluster exponent
    dFAToffset                 dd ?  ;Vol rel offset of first FAT, in sectors
    bNumberOfFATs              db ?  ;Number of FATs
    wNumberRootDirEntries      dw ?  ;In sectors
    dClusterHeapOffset         dd ?  ;Start of data area, in sectors
    dClusterCount              dd ?  ;Total number of clusters (volume size)
    dFATlength                 dd ?  ;FAT length, in sectors
    dFirstClusterOfRootDir     dd ?  ;First Cluster of Root Directory, min 2
    qDriverHeaderPtr           dq ?  ;Pointer to device driver header
    bMediaDescriptor           db ?  ;Media descriptor
    bAccessFlag                db ?  ;Access Flag (0 if accessed, else -1)
    qNextDPBPtr                dq ?  ;Pointer to next DPB, -1 if at end
    dFirstFreeCluster          dd ?  ;Starting cluster of free space search
    dNumberOfFreeClusters      dd ?  ;Number of free clusters, -1 unknown

dpb ENDS

drvHdr STRUC  ;Device Driver Header for character and block devices

    qPtrnxt dq ?  ;Pointer to the next driver header, -1 if at the end
    wAttrib dw ?  ;Attribute Word
    qStratp dq ?  ;Strategy Entry Pointer
    qInterp dq ?  ;Interrupt Entry Pointer
    vDrvnam db 8 dup (?)  ;Driver name (Character Only)

drvHdr ENDS
    bUntnum equ drvHdr.vDrvnam  ;Unit number byte (Block Only)

drvReqHdr STRUC  ;Driver Request Header

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

drvReqHdr ENDS

initReqPkt STRUC  ;Init Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    numunt db ? ;Number of logical units (Block only, 0 for char)
    endptr dq ?  ;Pointer to first free byte after driver
    optptr dq ?  ;Pointer to the BPB array (block) or optional args (char)
    drvnum db ?  ;Drive number

initReqPkt ENDS

mediaCheckReqPkt STRUC ;Media Check Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    medesc db ?  ;DOS media descriptor
    medret db ?  ;Return byte (Has media been changed?)
    desptr dq ?  ;Pointer to a valid volume id field

mediaCheckReqPkt ENDS

bpbBuildReqPkt STRUC  ;Build BPB Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    medesc db ?  ;DOS media descriptor
    bufptr dq ?  ;Transfer buffer
    bpbptr dq ?  ;Pointer to the BPB

bpbBuildReqPkt ENDS

ioReqPkt STRUC   ;IO Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    medesc db ?  ;DOS media descriptor
    bufptr dq ?  ;Transfer buffer
    tfrlen dw ?  ;Number of Sectors/bytes to transfer
    strtsc dq ?  ;Starting sector for transfer
    desptr dq ?  ;Pointer to a valid volume id field if error

ioReqPkt ENDS

nonDestInNoWaitReqPkt STRUC    ;Nondestructive Input No Wait Request Packet
    
    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    retbyt db ?  ;Byte read non destructively

nonDestInNoWaitReqPkt ENDS

statusReqPkt STRUC   ;Status Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

statusReqPkt ENDS

flushReqPkt STRUC ;Flush Request Packet, terminate all pending requests

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

flushReqPkt ENDS

openReqPkt STRUC ;Open Device Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

openReqPkt ENDS

closeReqPkt STRUC ;Close Device Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

closeReqPkt ENDS

remMediaReqPkt STRUC ;Removeable Media? Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field

remMediaReqPkt ENDS

ioctlReqPkt STRUC    ;Generic IOCTL Request Packet

    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    majfun db ?  ;Major function number
    minfun db ?  ;Minor function number
    rsival dq ?  ;Contents of RSI
    rdival dq ?  ;Contents of RDI
    ctlptr dq ?  ;Pointer to Generic IOCTL Request Packet

ioctlReqPkt ENDS

getDevReqPkt STRUC ;Get Logical Device Request Packet
    
    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    getcmd db ?  ;Command code
    cmdsts dw ?  ;Command status word

getDevReqPkt ENDS

setDevReqPkt STRUC ;Set Logical Device Request Packet
    
    hdrlen db ?  ;Length of the request header
    unitnm db ?  ;Unit number, meaningless for character devs
    cmdcde db ?  ;Command code
    status dw ?  ;Status word
    dosptr dq ?  ;DOS queue pointer field
    devptr dq ?  ;Device queue pointer field
    
    setcmd db ?  ;Command code
    cmdsts dw ?  ;Command status word

setDevReqPkt ENDS


loader SEGMENT USE64
    ORG 7C00h
    ASSUME ds:FLAT, es:FLAT

    dw 0AA55h           ;Initial signature
    mov rbp, OFFSET startmsg
    mov eax, 1304h
    int 30h

    jmp short $

    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
loader ENDS

END