
#include "../../Headers/dos.h"

//If this returns True, you can get the task return code
BOOL ExecuteProcess(LPCSTR lpProcessName, LPEPB lpExecuteParameterBlock){
    __asm__ __volatile(
        "push rbx\n\t"
        "mov rbx, rdx\n\t"
        "mov rdx, rcx\n\t"
        "mov eax, 0x4B00\n\t"
        "xor ecx, ecx\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx\n\t"
        "pop rbx\n\t"
    );
};

BOOL LoadProcess(LPCSTR lpProcessName, LPLPB lpLoadParameterBlock){
    __asm__ __volatile(
        "push rbx\n\t"
        "mov rbx, rdx\n\t"
        "mov rdx, rcx\n\t"
        "mov eax, 0x4B01\n\t"
        "xor ecx, ecx\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx\n\t"
        "pop rbx\n\t"
    );
};

BOOL LoadOverlay(LPCSTR lpOverlayName, LPLOB lpLoadOverlayBlock){
    __asm__ __volatile(
        "push rbx\n\t"
        "mov rbx, rdx\n\t"
        "mov rdx, rcx\n\t"
        "mov eax, 0x4B03\n\t"
        "xor ecx, ecx\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx\n\t"
        "pop rbx\n\t"
    );
};

VOID ExitProcess(BYTE bExitCode){
    __asm__ __volatile__ (
        "mov eax, 0x4C00\n\t"
        "mov al, cl\n\t"
        "int 0x41"
    );
}

VOID ExitProcessAndStayResidentP(BYTE bExitCode, DWORD dwParagraphsToReserve){
    __asm__ __volatile__(
        "mov eax, 0x3100\n\t"
        "mov al, cl\n\t"
        "int 0x41\n\t"
    );
}

VOID ExitProcessAndStayResidentB(DWORD dwBytesToReserve){
    __asm__ __volatile__(
        "mov edx, ecx\n\t"
        "int 0x47"
    );
}


//Gets the return value of the process that last terminated
RETURN_CODE GetTerminatedProcessReturnCode(){
    __asm__ __volatile__ (
        "mov eax, 0x4D00\n\t"
        "int 0x41\n\t"
    );   
}

//Make two Extended Error Code functions. 
//One for just the ext error code and the other for full info.

//Uses r10 for wExtErrorCode 
VOID __getErrorInformation(LPWORD lpExtErrorCode, LPBYTE lpErrorClass,\
    LPBYTE lpErrorAction, LPBYTE lpErrorLocus){
        __asm__ __volatile__(
            "push rbx\n\t"
            "push r10\n\t"
            "mov r10, rcx\n\t"
            "mov eax, 0x5900\n\t"
            "int 0x41\n\t"
            "mov word ptr [r10], ax\n\t"
            "mov byte ptr [rdx], bh\n\t"
            "mov byte ptr [r8], bl\n\t"
            "mov rdx, r9\n\t"
            "mov byte ptr [rdx], ch\n\t"
            "pop r10\n\t"
            "pop rbx"
        );
    }

VOID GetExtendedErrorInformation(LPEXT_ERROR lpExtendedError){
    LPWORD extErr = &(lpExtendedError->wExtendedError);
    LPBYTE class = &(lpExtendedError->bErrorClass);
    LPBYTE action = &(lpExtendedError->bSuggestedAction);
    LPBYTE locus = &(lpExtendedError->bErrorLocus);
    __getErrorInformation(extErr, class, action, locus);

}

WORD GetExtendedErrorCode(){
    WORD extErr = 0;
    BYTE nulVar = 0; //A dumping ground for the info to lose
    __getErrorInformation(&extErr, &nulVar, &nulVar, &nulVar);
    return extErr;
}