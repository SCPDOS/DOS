
;-------------------------------------------------------;
; This include file contains miscellaneous internal     ;
; SCP/DOS structures.                                   ;
;-------------------------------------------------------;

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