#include "../../Headers/dos.h"

//Internal common Allocation function
LPVOID __AllocateMemory(SIZE_T dwNumberOfParagraphs){
        __asm__ __volatile__(
        "push rbx\n\t"
        "mov ebx, ecx\n\t"
        "xor ecx, ecx\n\t"
        "mov eax, 0x4800\n\t"
        "int 0x41\n\t"
        "cmovc eax, ecx\n\t"
        "pop rbx"
    );
}

BOOL __FreeMemory(LPVOID lpAddress){
    __asm__ __volatile__(
        "mov r8, rcx\n\t"
        "mov eax, 0x4900\n\t"
        "xor ecx, ecx\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx"
    );
}

BOOL __ReallocateMemory(LPVOID lpAddress, SIZE_T dwNumberOfParagraphs){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov r8, rcx\n\t"
        "mov ebx, edx\n\t"
        "xor ecx, ecx\n\t"
        "mov eax, 0x4A00\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx"
    );
}

//Pass the number of paragraphs to allocate in dwSize
//Returns a NULL pointer if fails
LPVOID VirtualAllocate(SIZE_T dwNumberOfParagraphs){
    return __AllocateMemory(dwNumberOfParagraphs);
}

BOOL VirtualFree(LPVOID lpAddress){
    return __FreeMemory(lpAddress);
}

BOOL VirtualReallocate(LPVOID lpAddress, SIZE_T dwNumberOfParagraphs){
    return __ReallocateMemory(lpAddress, dwNumberOfParagraphs);
}

//Undocumented but exposed function for now.
LPVOID PhysicalAllocate(SIZE_T dwNumberOfParagraphs){
    return __AllocateMemory(dwNumberOfParagraphs);
}

//Undocumented but exposed function for now.
BOOL PhysicalFree(LPVOID lpAddress){
    return __FreeMemory(lpAddress);
}

//Undocumented but exposed function for now.
BOOL PhysicalReallocate(LPVOID lpAddress, SIZE_T dwNumberOfParagraphs){
    return __ReallocateMemory(lpAddress, dwNumberOfParagraphs);
}

//If this function returns -1, memory chain error detected
BYTE GetMemoryAllocationStrategy(){
    __asm__ __volatile__(
        "mov eax, 0x5800\n\t"
        "int 0x41\n\t"
    );
}

//If this function returns FALSE, memory chain error detected
BOOL SetMemoryAllocationStrategy(BYTE bAllocationStrategy){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov ebx, ecx\n\t"
        "xor ecx, ecx\n\t"
        "mov eax, 0x5801\n\t"
        "int 0x41\n\t"
        "mov eax, 1\n\t"
        "cmovc eax, ecx"
    );
}
