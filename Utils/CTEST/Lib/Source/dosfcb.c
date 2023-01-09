#include "../../Headers/scpdos.h"

VOID __setDTA(LPVOID lpDTA){
    __asm__ __volatile__(
        "mov rdx, rcx\n\t"
        "mov eax, 0x1A00\n\t"
        "int 0x41"
    );
}

LPVOID __getDTA(){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov eax, 0x2F00\n\t"
        "int 0x41\n\t"
        "mov rax, rbx\n\t"
        "pop rbx"
    );
}