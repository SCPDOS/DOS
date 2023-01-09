#include "../../Headers/dos.h"

VOID WriteString(LPCSTR lpString){
    __asm__ __volatile__ (
    "mov rdx, rcx \n\t"
    "mov eax, 0x0900 \n\t"
    "int 0x41 \n\t"
    );
}