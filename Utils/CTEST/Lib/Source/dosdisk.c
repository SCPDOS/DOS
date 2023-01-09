#include "../../Headers/scpdos.h"

VOID DiskReset(){
    __asm__ __volatile__(
        "mov eax, 0x0D00\n\t"
        "int 0x41"
    );
}

VOID SetDefaultDrive(DRIVE_INDEX bDiskIndex){
    __asm__ __volatile__(
        "movzx edx, cl\n\t"
        "mov eax, 0x0E00\n\t"
        "int 0x41"
    );
}

DRIVE_INDEX GetDefaultDrive(){
    __asm__ __volatile__(
        "mov eax, 0x1900\n\t"
        "int 0x41"
    );
}

BOOL GetDiskFATInfo(DRIVE_NUMBER bDriveNumber, LPDWORD lpBytesPerSector, \
    LPDWORD lpSectorsPerCluster, LPDWORD lpTotalNumberOfClusters, \
    LPVOID *lpMediaDescriptor){
        return FALSE;
    }

BOOL GetDefaultDiskFATInfo(LPDWORD lpBytesPerSector, \
    LPDWORD lpSectorsPerCluster, LPDWORD lpTotalNumberOfClusters, \
    LPVOID *lpMediaDescriptor){
        return FALSE;
    }
    
BOOL GetDiskFreeSpace(DRIVE_NUMBER bDriveNumber, LPDWORD lpSectorsPerCluster, \
    LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, \
    LPDWORD lpTotalNumberOfFreeClusters){
        return FALSE;
    }

BYTE GetDiskReadVerifyFlag(){
    __asm__ __volatile__(
        "mov eax, 0x5400\n\t"
        "int 0x41\n\t"
    );
}