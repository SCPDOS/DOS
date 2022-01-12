    DEFAULT REL
    [map all io.map]

STRUC   bpb         ;FAT 12 and 16 BPB

    .btjmp  resb 3
    .osname resb 8  ;OEM name
    .bypsec resw 1  ;Bytes per sector
    .secpcl resb 1  ;Sectors per cluster
    .ressec resw 1  ;Number of reserved sectors
    .numFAT resb 1  ;Number of FATs on media
    .nortdr resw 1  ;Number of entries in Root directory
    .nosect resw 1  ;Number of sectors on medium
    .medesc resb 1  ;Media descriptor byte
    .FATsec resw 1  ;Number of sectors per FAT
    .sectrc resw 1  ;Number of sectors per "track"
    .numhed resw 1  ;Number of read "heads"
    .numhsc resd 1  ;Number of hidden sectors
    .nsec32 resd 1  ;32 bit count of sectors

    .ldrvnu resb 1  ;Logical drive number (00h or 80h)
    .resbyt resb 1  ;Reserved byte
    .extsig resb 1  ;Extended boot signature (29h)
    .sernum resd 1  ;Volume serial number
    .vollbl resb 11 ;Volume label string
    .fstype resb 8  ;File system type string

ENDSTRUC

STRUC   bpb32       ;FAT 32 BPB

    .btjmp  resb 3
    .osname resb 8  ;OEM name
    .bypsec resw 1  ;Bytes per sector
    .secpcl resb 1  ;Sectors per cluster
    .ressec resw 1  ;Number of reserved sectors
    .numFAT resb 1  ;Number of FATs on media
    .nortdr resw 1  ;Number of entries in Root directory
    .nosect resw 1  ;Number of sectors on medium
    .medesc resb 1  ;Media descriptor byte
    .FATsec resw 1  ;Number of sectors per FAT
    .sectrc resw 1  ;Number of sectors per "track"
    .numhed resw 1  ;Number of read "heads"
    .numhsc resd 1  ;Number of hidden sectors
    .nsec32 resd 1  ;32 bit count of sectors

    .nFAT32 resd 1  ;32 bit count of sectors occupied by one FAT
    .extflg resw 1  ;Extended Flags word
    .FSvers resw 1  ;File system version word, must be 0
    .rtclus resd 1  ;First Cluster of Root Directory
    .FSinfo resw 1  ;Sector number of FSINFO structure, usually 1
    .bpbbkp resw 1  ;Backup Boot sector, either 0 or 6
    .res1   resb 12 ;Reserved 12 bytes

    .ldrvnu resb 1  ;Logical drive number (00h or 80h)
    .resbyt resb 1  ;Reserved byte
    .extsig resb 1  ;Extended boot signature (29h)
    .sernum resd 1  ;Volume serial number
    .vollbl resb 11 ;Volume label string
    .fstype resb 8  ;File system type string

ENDSTRUC

STRUC   bpbEx       ;exFAT BPB

    .btjmp  resb 3 
    .osname resb 8  ;OEM name
    .zerobt resb 53 ;Must be 0, 53 bytes
    .ptnoff resq 1  ;Partition offset in sectors, 0 means ignore this field
    .vollen resq 1  ;Volume Length in sectors
    .FAToff resd 1  ;Volume relative offset of first FAT, in sectors
    .FATlen resd 1  ;FAT length, in sectors
    .clsoff resd 1  ;Cluster Heap Offset (start of data area), in sectors
    .clscnt resd 1  ;Cluster count, number of clusters on medium
    .rtdir  resd 1  ;First Cluster of Root Directory, min 2
    .volser resd 1  ;Volume Serial Number
    .fsrev  resw 1  ;File System Revision word, should be 0001 (v1.00)
    .volflg resw 1  ;Volume Flags, refer to documentation
    .bpssft resb 1  ;Byte per sector shift, min 9 (512 bps), max 12 (4096 bps)
    .spcsft resb 1  ;Sector per cluster shift, result of log_2(N) for N=sec/clus
    .numFAT resb 1  ;Number of FATs, only 1 or 2
    .drvsel resb 1  ;Drive Select, 0 or 80h (Int 13h)
    .pctuse resb 1  ;Percent of volume in use, rounded down. FFh means unknown
    .resrvd resb 7  ;Reserved for alignment

ENDSTRUC

STRUC   dpb         ;Drive Parameter Block

    .bDrvnum resb 1  ;Drive number
    .bUntnum resb 1  ;Unit number in device
    .bBypsec resb 1  ;Bytes per sector shift (N in 2^N)
    .bSecpcl resb 1  ;Sectors per cluster - 1
    .bSpclsh resb 1  ;Shift factor for sectors per cluster
    .dFAToff resd 1  ;Volume relative offset of first FAT, in sectors
    .bNumFAT resb 1  ;Number of FATs
    .wNortdr resw 1  ;Number of root directory entries, in sectors
    .wNodtsc resw 1  ;Start of data area, in sectors
    .dMaxclu resd 1  ;Total number of clusters
    .dSecFAT resd 1  ;FAT length, in sectors
    .dRtclus resd 1  ;First Cluster of Root Directory
    .qPtrdrv resq 1  ;Pointer to device driver header
    .bMedesc resb 1  ;Media descriptor
    .bAccflg resb 1  ;Access Flag (0 if accessed, else -1)
    .qPtrnxt resq 1  ;Pointer to next DPB, -1 if at the end of chain
    .dFrecls resd 1  ;Starting cluster of free space search
    .dNfrcls resd 1  ;Number of free clusters, -1 unknown

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