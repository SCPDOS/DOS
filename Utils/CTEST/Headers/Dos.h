/*
    The start of a DOS low level library upon which stdlib (newlib) will be 
    built!
    Currently, the .c file uses the SYSV ABI. Must Must switch to MS
*/
#include "DosTypes.h"
#include "DosError.h"

#ifndef DOS_H
#define DOS_H

VOID WriteString(LPCSTR lpString); /*This needs to be modified to accept C strings*/

HANDLE CreateFile(LPCSTR lpFileName, FILE_ATTRIBUTES dwFileAttributes);
HANDLE OpenFile(LPCSTR lpFileName, FILE_OPEN_MODE dwOpenMode, \
    FILE_SHARE_MODE dwShareMode);
BOOL CloseFile(HANDLE hFile);
BOOL ReadFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, \
    LPDWORD lpNumberOfBytesRead);
BOOL WriteFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToWrite, \
    LPDWORD lpNumberOfBytesWritten);

BOOL DeleteFile(LPCSTR lpFileName);

DWORD SetFilePointer(HANDLE hFile, LONG lDistanceToMove, DWORD dwMoveMethod);



VOID Exit(RETURN_CODE returnCode);
RETURN_CODE Wait(PROCESS_ID processId);

#endif