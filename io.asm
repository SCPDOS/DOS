    DEFAULT REL
    [map all io.map]

STRUC   bpb         ;FAT 12 and 16 BPB

    .jmpBoot    resb 3
    .oemName    resb 8  ;OEM name
    .bytsPerSec resw 1  ;Bytes per sector
    .secPerClus resb 1  ;Sectors per cluster
    .revdSecCnt resw 1  ;Number of reserved sectors
    .numFATs    resb 1  ;Number of FATs on media
    .rootEntCnt resw 1  ;Number of entries in Root directory
    .totSec16   resw 1  ;Number of sectors on medium
    .media      resb 1  ;Media descriptor byte
    .FATsz16    resw 1  ;Number of sectors per FAT
    .secPerTrk  resw 1  ;Number of sectors per "track"
    .numHeads   resw 1  ;Number of read "heads"
    .hiddSec    resd 1  ;Number of hidden sectors
    .totSec32   resd 1  ;32 bit count of sectors

    .drvNum     resb 1  ;Logical drive number (00h or 80h)
    .reserved1  resb 1  ;Reserved byte
    .bootSig    resb 1  ;Extended boot signature (29h)
    .volID      resd 1  ;Volume serial number
    .volLab     resb 11 ;Volume label string
    .filSysType resb 8  ;File system type string

ENDSTRUC

STRUC   bpb32       ;FAT 32 BPB

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

ENDSTRUC

STRUC   bpbEx       ;exFAT BPB

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

ENDSTRUC

STRUC   dpb         ;Drive Parameter Block

    .bDriveNumber               resb 1  ;Drive number
    .bUnitNumber                resb 1  ;Unit number in device
    .bBytesPerSectorShift       resb 1  ;min 9 (512 bps), max 12 (4096 bps)
    .bMaxSectorInCluster        resb 1  ;(Maximum sector in cluster) - 1
;                                        i.e. (2^bSectorsPerClusterShift) - 1
    .bSectorsPerClusterShift    resb 1  ;Sectors per cluster exponent
    .dFAToffset                 resd 1  ;Vol rel offset of first FAT, in sectors
    .bNumberOfFATs              resb 1  ;Number of FATs
    .wNumberRootDirEntries      resw 1  ;In sectors
    .dClusterHeapOffset         resd 1  ;Start of data area, in sectors
    .dClusterCount              resd 1  ;Total number of clusters (volume size)
    .dFATlength                 resd 1  ;FAT length, in sectors
    .dFirstClusterOfRootDir     resd 1  ;First Cluster of Root Directory, min 2
    .qDriverHeaderPtr           resq 1  ;Pointer to device driver header
    .bMediaDescriptor           resb 1  ;Media descriptor
    .bAccessFlag                resb 1  ;Access Flag (0 if accessed, else -1)
    .qNextDPBPtr                resq 1  ;Pointer to next DPB, -1 if at end
    .dFirstFreeCluster          resd 1  ;Starting cluster of free space search
    .dNumberOfFreeClusters      resd 1  ;Number of free clusters, -1 unknown

ENDSTRUC

STRUC   drvHdr      ;Device Driver Header for character and block devices

    .qPtrnxt resq 1  ;Pointer to the next driver header, -1 if at the end
    .wAttrib resw 1  ;Attribute Word
    .qStratp resq 1  ;Strategy Entry Pointer
    .qInterp resq 1  ;Interrupt Entry Pointer
    .bUntnum:        ;Unit number byte (Block Only)
    .vDrvnam resb 8  ;Driver name (Character Only)

ENDSTRUC

STRUC   drvReqHdr   ;Driver Request Header

    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC   initReqPkt   ;Init Request Packet

    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .numunt resb 1  ;Number of logical units (Block only, 0 for char)
    .endptr resq 1  ;Pointer to first free byte after driver
    .optptr resq 1  ;Pointer to the BPB array (block) or optional args (char)
    .drvnum resb 1  ;Drive number

ENDSTRUC

STRUC mediaCheckReqPkt  ;Media Check Request Packet

    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .medesc resb 1  ;DOS media descriptor
    .medret resb 1  ;Return byte (Has media been changed?)
    .desptr resq 1  ;Pointer to a valid volume id field

ENDSTRUC

STRUC bpbBuildReqPkt   ;Build BPB Request Packet

    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .medesc resb 1  ;DOS media descriptor
    .bufptr resq 1  ;Transfer buffer
    .bpbptr resq 1  ;Pointer to the BPB

ENDSTRUC


STRUC ioReqPkt      ;IO Request Packet

    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .medesc resb 1  ;DOS media descriptor
    .bufptr resq 1  ;Transfer buffer
    .tfrlen resw 1  ;Number of Sectors/bytes to transfer
    .strtsc resq 1  ;Starting sector for transfer
    .desptr resq 1  ;Pointer to a valid volume id field if error

ENDSTRUC

STRUC nonDestInNoWaitReqPkt   ;Nondestructive Input No Wait Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .retbyt resb 1  ;Byte read non destructively

ENDSTRUC

STRUC statusReqPkt  ;Status Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC flushReqPkt   ;Flush Request Packet, terminate all pending requests
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC openReqPkt    ;Open Device Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC closeReqPkt   ;Close Device Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC remMediaReqPkt    ;Removeable Media? Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

ENDSTRUC

STRUC ioctlReqPkt   ;Generic IOCTL Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .majfun resb 1  ;Major function number
    .minfun resb 1  ;Minor function number
    .rsival resq 1  ;Contents of RSI
    .rdival resq 1  ;Contents of RDI
    .ctlptr resq 1  ;Pointer to Generic IOCTL Request Packet

ENDSTRUC

STRUC getDevReqPkt  ;Get Logical Device Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .getcmd resb 1  ;Command code
    .cmdsts resw 1  ;Command status word

ENDSTRUC

STRUC setDevReqPkt  ;Set Logical Device Request Packet
    
    .length resb 1  ;Length of the request header
    .unitnm resb 1  ;Unit number, meaningless for character devs
    .cmdcde resb 1  ;Command code
    .status resw 1  ;Status word
    .dosptr resq 1  ;DOS queue pointer field
    .devptr resq 1  ;Device queue pointer field

    .setcmd resb 1  ;Command code
    .cmdsts resw 1  ;Command status word

ENDSTRUC

    BITS 64
Section loader vstart=7C00h align=1
    dw 0AA55h           ;Initial signature
    mov rbp, startmsg
    mov eax, 1304h
    int 30h

    jmp short $

Section loaderData vfollows=loader align=4
startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0