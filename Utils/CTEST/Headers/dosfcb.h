
#include "basetsd.h"
#include "dosfile.h"
#include "dosdisk.h"

#ifndef DOSFCB_H
#define DOSFCB_H

typedef struct _fcb{
    DRIVE_NUMBER bDriveNumber;
    LPCSTR lpFcbName[8];
    LPCSTR lpFcbExtention[3];
    PBYTE reserved[23];
} FCB, *PFCB, *LPFCB;

typedef struct _extended_fcb{
    BYTE bExtendedFlag;  //Must be set to -1
    PBYTE reserved2[5];
    FILE_ATTRIBUTES dwFileAttribute :8;  //Use only low 8 bits of file attribs
    DRIVE_NUMBER bDriveNumber;
    LPCSTR lpFcbName[8];
    LPCSTR lpFcbExtention[3];
    PBYTE reserved[23];
} EXTENDED_FCB, *PEXTENDED_FCB, *LPEXTENDED_FCB;

#endif

VOID __setDTA(LPVOID lpDTA);
LPVOID __getDTA();