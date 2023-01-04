/*
    The start of a DOS low level library upon which stdlib (newlib) will be 
    built!
    Currently, the .c file uses the SYSV ABI. Must Must switch to MS
*/

//
// Basics
//

#ifndef VOID
#define VOID void
#endif

#ifndef FALSE
#define FALSE               0
#endif

#ifndef TRUE
#define TRUE                1
#endif

#ifndef
#define BAD_HANDLE 0xFFFFFFFFFFFFFFFF
#endif

typedef char CHAR;
typedef short SHORT;
typedef int INT;
typedef long LONG;

typedef unsigned char BYTE;
typedef unsigned short WORD;
typedef unsigned int DWORD;
typedef unsigned long QWORD;

typedef unsigned long       DWORD;
typedef int                 BOOL;
typedef unsigned char       BYTE;
typedef unsigned short      WORD;
typedef float               FLOAT;
typedef FLOAT               *PFLOAT;
typedef BOOL                *PBOOL;
typedef BOOL                *LPBOOL;
typedef BYTE                *PBYTE;
typedef BYTE                *LPBYTE;
typedef int                 *PINT;
typedef int                 *LPINT;
typedef WORD                *PWORD;
typedef WORD                *LPWORD;
typedef long                *LPLONG;
typedef DWORD               *PDWORD;
typedef DWORD               *LPDWORD;
typedef void                *LPVOID;
typedef CONST void          *LPCVOID;

//
// ANSI (Multi-byte Character) types
//
typedef CHAR *PCHAR, *LPCH, *PCH;
typedef CONST CHAR *LPCCH, *PCCH;

typedef  CHAR *NPSTR, *LPSTR, *PSTR;
typedef  PSTR *PZPSTR;
typedef  CONST PSTR *PCZPSTR;
typedef  CONST CHAR *LPCSTR, *PCSTR;
typedef  PCSTR *PZPCSTR;
typedef  CONST PCSTR *PCZPCSTR;

typedef  CHAR *PNZCH;
typedef  CONST CHAR *PCNZCH;

typedef BYTE  BOOLEAN;           
typedef BOOLEAN *PBOOLEAN; 

typedef VOID *HANDLE;
typedef HANDLE *PHANDLE;

typedef struct _SECURITY_ATTRIBUTES {
  DWORD  nLength; /* Effectively null */
  LPVOID lpSecurityDescriptor; /* Effectively null */
  BOOL   bInheritHandle; /* We only check if this is true or false! */
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

typedef WORD RETURN_CODE;
typedef QWORD PROCESS_ID;

VOID WriteString(LPCSTR*);    /*This needs to be modified to accept C strings*/

HANDLE CreateFile(DWORD attributes, LPCSTR lpFileName);

VOID Exit(RETURN_CODE returnCode);
RETURN_CODE Wait(PROCESS_ID processId);