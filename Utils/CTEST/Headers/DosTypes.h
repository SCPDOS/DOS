//
// Basic type definitions
//

#ifndef DOSTYPES_H
#define DOSTYPES_H

#ifndef VOID
#define VOID void
#endif

#ifndef CONST
#define CONST const
#endif

#ifndef FALSE
#define FALSE   0
#endif

#ifndef TRUE
#define TRUE    1
#endif

#ifndef BAD_HANDLE
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

typedef int          BOOL;
typedef float        FLOAT;
typedef FLOAT       *PFLOAT;
typedef BOOL        *PBOOL;
typedef BOOL        *LPBOOL;
typedef BYTE        *PBYTE;
typedef BYTE        *LPBYTE;
typedef int         *PINT;
typedef int         *LPINT;
typedef WORD        *PWORD;
typedef WORD        *LPWORD;
typedef long        *PLONG;
typedef long        *LPLONG;
typedef DWORD       *PDWORD;
typedef DWORD       *LPDWORD;
typedef void        *LPVOID;
typedef CONST void  *LPCVOID;

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

#ifndef STDIN
#define STDIN 0x00
#endif

#ifndef STDOUT
#define STDOUT 0x01
#endif

#ifndef STDERR
#define STDERR 0x02
#endif

#ifndef STDAUX
#define STDAUX 0x03
#endif

#ifndef STDPRN
#define STDPRN 0x04
#endif

/* DOS FUNCTION SPECIFIC TYPES */

typedef WORD RETURN_CODE;
typedef QWORD PROCESS_ID;

typedef struct _SECURITY_ATTRIBUTES {
  DWORD  nLength; /* Effectively null */
  LPVOID lpSecurityDescriptor; /* Effectively null */
  BOOL   bInheritHandle; /* We only check if this is true or false! */
} SECURITY_ATTRIBUTES, *PSECURITY_ATTRIBUTES, *LPSECURITY_ATTRIBUTES;

/*Create Mode/File attribute definitions bit definitions*/
typedef DWORD FILE_ATTRIBUTES, *PFILE_ATTRIBUTES, *LPFILE_ATTRIBUTES;

#ifndef FILE_READ_ONLY
#define FILE_READ_ONLY          0x01
#endif

#ifndef FILE_HIDDEN
#define FILE_HIDDEN             0x02
#endif

#ifndef FILE_SYSTEM
#define FILE_SYSTEM             0x04
#endif

#ifndef FILE_ARCHIVE
#define FILE_ARCHIVE            0x20
#endif

/* The following are defined for file searches but MUST NOT be used for file 
    creations. DOS will return an error if these bits are set */

#ifndef FILE_VOLUME_LABEL
#define FILE_VOLUME_LABEL       0x08
#endif

#ifndef FILE_DIRECTORY
#define FILE_DIRECTORY          0x10
#endif

/*Open Mode bit definitions*/
typedef DWORD FILE_OPEN_MODE, *PFILE_OPEN_MODE, *LPFILE_OPEN_MODE;
typedef DWORD FILE_SHARE_MODE, *PFILE_SHARE_MODE, *LPFILE_SHARE_MODE;

#ifndef OPEN_READ
#define OPEN_READ               0x00
#endif

#ifndef OPEN_WRITE
#define OPEN_WRITE              0x01
#endif

#ifndef OPEN_READ_WRITE
#define OPEN_READ_WRITE         0x02
#endif

#ifndef OPEN_INHERIT        
#define OPEN_INHERIT            0x80
#endif

#ifndef SHARE_DENY_READ_WRITE
#define SHARE_DENY_READ_WRITE   0x10
#endif

#ifndef SHARE_DENY_WRITE
#define SHARE_DENY_WRITE        0x20
#endif

#ifndef SHARE_DENY_READ
#define SHARE_DENY_READ         0x30
#endif

#ifndef SHARE_DENY_NONE
#define SHARE_DENY_NONE         0x40
#endif



/*LSEEK modes*/
#ifndef FILE_BEGIN
#define FILE_BEGIN    0x0
#endif

#ifndef FILE_CURRENT
#define FILE_CURRENT  0x1
#endif

#ifndef FILE_END
#define FILE_END      0x2
#endif

#endif