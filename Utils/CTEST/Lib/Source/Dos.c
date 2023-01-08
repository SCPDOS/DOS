
/* Start of the low level scpdos library which will be used to build stdlib.*/

/*Uses MS x64 ABI
 * Integer arguments only needed for DOS API so we only highlight them here
 *             Arguments passed in: rcx, rdx, r8, r9 then stack
 *       Return value is passed in: rax
 *              Volatile registers: rax, rcx, r8, r9, r10, r11
 *           Nonvolatile registers: rbx, rsp, rbp, r12, r13, r14, r15, rdi
 */

#include "../../Headers/Dos.h"

VOID WriteString(LPCSTR lpString){
    __asm__ __volatile__ (
    "mov rdx, rcx \n\t"
    "mov eax, 0x0900 \n\t"
    "int 0x41 \n\t"
    );
}

HANDLE CreateFile(LPCSTR lpFileName, FILE_ATTRIBUTES dwFileAttributes){
    /*Swap the two parameters to get them in the right registers*/
    __asm__ __volatile__ (
    "xchg rdx, rcx\n\t"
    "mov eax, 0x3C00\n\t"
    "int 0x41\n\t"
    "mov ecx, -1\n\t"
    "cmovc eax, ecx"
    );  
}

HANDLE OpenFile(LPCSTR lpFileName, FILE_OPEN_MODE dwOpenMode, \
    FILE_SHARE_MODE dwShareMode){
    /* rcx -> lpFileName, rdx = dwOpenMode  r8 = dwShareMode*/
    /* First clear bits 2 and 3 on the OpenMode */
    /* Then preserve only share bits in r8b (i.e. bits 4,5,6)*/
    __asm__ __volatile__ (
        "mov eax, 0x3D00\n\t"
        "and dl, 0xF3\n\t"
        "mov al, dl\n\t"
        "and r8b, 0x70\n\t "
        "or al, r8b\n\t"
        "mov rdx, rcx\n\t"
        "int 0x41\n\t"
        "mov ecx, -1\n\t"
        "cmovc eax, ecx"
    );  
}

BOOL CloseFile(HANDLE hFile){
    /*If we return TRUE, then handle was closed*/
    __asm__ __volatile__ (
        "mov eax, 0x3E00\n\t"
        "push rbx\n\t"
        "mov ebx, ecx\n\t"
        "int 0x41\n\t"
        "pop rbx\n\t"
        "mov ecx, 1\n\t"
        "mov eax, 0\n\t"
        "cmovc eax, ecx"
    );
}

BOOL ReadFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToRead, \
    LPDWORD lpNumberOfBytesRead){
        /*rcx = hFile, rdx -> lpBuffer, 
        r8 = nNumberOfBytesToRead, r9 -> NumberOfBytesRead*/
        __asm__ __volatile__(
            "\t push rbx\n"
            "\t mov ebx, ecx\n"
            "\t mov rcx, r8\n"
            "\t mov eax, 0x3F00\n"
            "\t int 0x41\n"
            "\t pop rbx\n"
            "\t jnc readOk\n"
            "\t mov eax, 0\n"
            "\t jmp short readExit\n"
            "readOk:\n"
            "\t mov dword [r9], eax\n"
            "\t mov eax, 1\n"
            "readExit:"
        );
    }

BOOL WriteFile(HANDLE hFile, LPVOID lpBuffer, DWORD nNumberOfBytesToWrite, \
    LPDWORD lpNumberOfBytesWritten){
        /*rcx = hFile, rdx -> lpBuffer, 
        r8 = nNumberOfBytesToWrite, r9 -> NumberOfBytesWritten*/
        __asm__ __volatile__(
            "\t push rbx\n"
            "\t mov ebx, ecx\n"
            "\t mov rcx, r8\n"
            "\t mov eax, 0x4000\n"
            "\t int 0x41\n"
            "\t pop rbx\n"
            "\t jnc writeOk\n"
            "\t mov eax, 0\n"
            "\t jmp short writeExit\n"
            "writeOk:\n"
            "\t mov dword [r9], eax\n"
            "\t mov eax, 1\n"
            "writeExit:"
        );
    }
    
BOOL DeleteFile(LPCSTR lpFileName){
    __asm__ __volatile__(
        "mov rdx, rcx\n\t"
        "mov eax, 0x4100\n\t"
        "int 0x41\n\t"
        "mov ecx, 1\n\t"
        "mov eax, 0\n\t"
        "cmovc eax, ecx"
    );
}

DWORD SetFilePointer(HANDLE hFile, LONG lDistanceToMove, DWORD dwMoveMethod){
        /* This is the 32 bit version of the function*/
        /* rcx = hFile, rdx = lDistanceToMove, r8 = 0, 1, 2 */
        __asm__ __volatile__(
            "push rbx\n\t"
            "mov ebx, ecx\n\t"
            "mov eax, 0x4200\n\t"
            "mov al, r8b\n\t"
            "int 0x41\n\t"
            "pop rbx\n\t"
        );
    }

DWORD SetFilePointerL(HANDLE hFile, LONG lDistanceToMove, \
    PLONG lpDistanceToMoveHigh, DWORD dwMoveMethod){
        /* This is the 64 bit version of the function, not yet to be exported */
        /* rcx = hFile, rdx = lDistanceToMove, r8 = lpDistanceToMoveHigh, 
            r9 = 0, 1, 2 */
        /* If r8=NULL then ignore this*/
        __asm__ __volatile__(
            "push rbx\n\t"
            "mov ebx, ecx\n\t"
            "xor ecx, ecx\n\t"
            "test r8, r8\n\t"
            "cmovnz ecx, dword [r8]\n\t"
            "mov eax, 0x4200\n\t"
            "mov al, r9b\n\t"
            "int 0x41\n\t"
            "pop rbx\n\t"
        );
    }

VOID Exit(RETURN_CODE returnCode){
    /*Since RETURN_CODE is a WORD, any value bigger than 255 is set to -1*/
    __asm__ __volatile__ (
    "mov eax, 0xFF\n\t"
    "cmp ecx, eax\n\t"
    "cmova ecx, eax\n\t"
    "mov eax, 0x4C00\n\t"
    "mov al, cl\n\t"
    "int 0x41"
    );
}
RETURN_CODE Wait(PROCESS_ID processId){
    __asm__ __volatile__ (
    "mov eax, 0x4D00\n\t"
    "int 0x41\n\t"
    );   
}