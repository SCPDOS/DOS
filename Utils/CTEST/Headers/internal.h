// This is a file used for specifying DOS internal data structures and 
// prototypes for special functions such as 41h/AH=52h etc.

#ifndef INTERNAL_H
#define INTERNAL_H

#include "basetsd.h"
#include "dosdrive.h"
#include "dosdrvr.h"

typedef QWORD PROCESS_ID;

typedef struct _drive_parameter_block{
    DRIVE_INDEX bDriveIndex;
    BYTE bDriverUnitNumber;
    WORD wBytesPerSector;
    BYTE bMaxSectorInCluster;
    BYTE bSectorPerClusterShift;
    WORD wFATOffset;
    BYTE bNumberOfFATs;
    WORD wNumberOfRootDirectoryEntries;
    DWORD dwClusterHeapOffset;
    DWORD dwFATLength;
    union{
        WORD wSectorOfRootDirectory;
        DWORD dwClusterOfRootDirectory;
    } dwFirstUnitOfRootDirectory;
    LPDEVDRV_HEADER lpDeviceDriverHeader;
    BYTE bMediaDescriptor;
    BYTE bAccessFlag;
    struct _drive_parameter_block *lpNextDPB;
    DWORD dwFirstFreeCluster;
    DWORD dwNumberOfFreeClusters;
} DPB, *PDPB, *LPDPB;


#endif