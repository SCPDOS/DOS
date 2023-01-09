#ifndef DOSDISK_H
#define DOSDISK_H
#include "basetsd.h"

//An ASCII Char in the range A-Z inclusive
typedef CHAR DRIVE_LETTER, *PDRIVE_LETTER, *LPDRIVE_LETTER; 
//A zero based number
typedef BYTE DRIVE_INDEX, *PDRIVE_INDEX, *LPDRIVE_INDEX;
//A one based drive number. Zero means default drive
typedef BYTE DRIVE_NUMBER, *PDRIVE_NUMBER, *LPDRIVE_NUMBER;

typedef struct _fat_time{
    BYTE seconds : 5;   //Seconds divided by 2, values [0, 28] valid
    BYTE minutes : 6;   //Minutes, values [0, 59] valid
    BYTE hours   : 5;   //Hours, values [0, 23] valid
} FAT_TIME, *PFAT_TIME, *LPFAT_TIME;

typedef struct _fat_date{
    BYTE day    : 5;    //Day of the month
    BYTE month  : 4;    //Month of the year, January = 1 ...
    BYTE year   : 7;    //Number of years minus 1980
} FAT_DATE, *PFAT_DATE, *LPFAT_DATE;

#endif

VOID DiskReset();
VOID SetDefaultDrive(DRIVE_INDEX bDiskIndex);
DRIVE_INDEX GetDefaultDrive();
BOOL GetDiskFATInfo(DRIVE_NUMBER bDriveNumber, LPDWORD lpBytesPerSector, \
    LPDWORD lpSectorsPerCluster, LPDWORD lpTotalNumberOfClusters, \
    LPVOID *lpMediaDescriptor);
BOOL GetDefaultDiskFATInfo(LPDWORD lpBytesPerSector, \
    LPDWORD lpSectorsPerCluster, LPDWORD lpTotalNumberOfClusters, \
    LPVOID *lpMediaDescriptor);
BOOL GetDiskFreeSpace(DRIVE_NUMBER bDriveNumber, LPDWORD lpSectorsPerCluster, \
    LPDWORD lpBytesPerSector, LPDWORD lpNumberOfFreeClusters, \
    LPDWORD lpTotalNumberOfFreeClusters);


BYTE GetDiskReadVerifyFlag();