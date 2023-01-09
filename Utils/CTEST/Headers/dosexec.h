#include "basetsd.h"

#ifndef DOSEXEC_H
#define DOSEXEC_H

typedef BYTE EXIT_CODE, *PEXIT_CODE, *LPEXIT_CODE;

#ifndef EXIT_OK
#define EXIT_OK 0
#endif

#ifndef EXIT_CTRL_BREAK
#define EXIT_CTRL_BREAK 1
#endif

#ifndef EXIT_ABORT
#define EXIT_ABORT  2
#endif

#ifndef EXIT_TSR
#define EXIT_TSR    3
#endif

typedef union _return_code{
    WORD wReturnWord;
    struct{
        EXIT_CODE bErrorLevel;          //Returned exit code
        BYTE bExitType;                 //Type of process termination
    };
} RETURN_CODE, *PRETURN_CODE, *LPRETURN_CODE;

typedef struct _exec_program_block {
    LPCSTR lpEnvironmentBlock;
    LPCSTR lpCommandLine;
    union{
        LPFCB lpFCB;
        LPEXTENDED_FCB lpExtended_FCB;
    }lpFCB1;
        union{
        LPFCB lpFCB;
        LPEXTENDED_FCB lpExtended_FCB;
    }lpFCB2;
} EPB, *PEPB, *LPEPB;

typedef struct _load_program_block {
    LPCSTR lpEnvironmentBlock;
    LPCSTR lpCommandLine;
    union{
        LPFCB lpFCB;
        LPEXTENDED_FCB lpExtended_FCB;
    }lpFCB1;
        union{
        LPFCB lpFCB;
        LPEXTENDED_FCB lpExtended_FCB;
    }lpFCB2;
    LPVOID lpProcessRIP;
    LPVOID lpProcessRSP;
} LPB, *PLPB, *LPLPB;

typedef struct _load_overlay_block {
    LPVOID lpOverlayLoadAddress;
    DWORD dwRelocationFactor;
} LOB, *PLOB, *LPLOB;

#endif

VOID ExitProcessAndStayResidentB(DWORD dwBytesToReserve);
VOID ExitProcessAndStayResidentP(BYTE bExitCode, DWORD dwParagraphsToReserve);
BOOL ExecProcess(LPCSTR lpProcessName, LPEPB lpExecuteParameterBlock);
BOOL LoadProcess(LPCSTR lpProcessName, LPLPB lpLoadParameterBlock);
BOOL LoadOverlay(LPCSTR lpOverlayName, LPLOB lpLoadOverlayBlock);
VOID ExitProcess(BYTE bExitCode);
RETURN_CODE GetExitCodeProcess();
