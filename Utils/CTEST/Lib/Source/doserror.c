#include "../../Headers/scpdos.h"

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

VOID GetExtendedLastError(LPEXT_ERROR lpExtendedError){
    LPWORD extErr = &(lpExtendedError->wExtendedError);
    LPBYTE class = &(lpExtendedError->bErrorClass);
    LPBYTE action = &(lpExtendedError->bSuggestedAction);
    LPBYTE locus = &(lpExtendedError->bErrorLocus);
    __getErrorInformation(extErr, class, action, locus);

}

WORD GetLastError(){
    WORD extErr = 0;
    BYTE nulVar = 0; //A dumping ground for the info to lose
    __getErrorInformation(&extErr, &nulVar, &nulVar, &nulVar);
    return extErr;
}