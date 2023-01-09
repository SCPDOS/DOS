#include "../../Headers/scpdos.h"

BOOL GetDPBPointer(DRIVE_NUMBER bNumber, LPDPB lpDPB){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov r8, rdx\n\t"
        "mov dl, cl\n\t"
        "xor ecx, ecx\n\t"
        "mov eax, 0x3200\n\t"
        "int 0x41\n\t"
        "cmp al, -1\n\t"
        "cmove eax, ecx\n\t"
        "je getDPBExit\n\t"
        "mov eax, ecx\n\t"
        "inc eax\n\t"
        "mov qword ptr [r8], rbx\n"
        "getDPBExit:\n\t"
        "pop rbx"
    );
}

VOID SetCurrentProcessId(PROCESS_ID pid){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov rbx, rcx\n\t"
        "mov eax, 0x5000\n\t"
        "int 0x41"
    );
}

PROCESS_ID GetCurrentProcessId(){
    __asm__ __volatile__(
        "push rbx\n\t"
        "mov eax, 0x5100\n\t"
        "int 0x41\n\t"
        "mov rax, rbx\n\t"
        "pop rbx"
    );
}

LPVOID GetPointerToDosSysVars(){
    __asm__ __volatile(
        "push rbx\n\t"
        "mov eax, 0x5200\n\t"
        "int 0x41\n\t"
        "mov rax, rbx\n\t"
        "pop rbx"
    );
}

VOID GenerateDPB(LPVOID lpBPB, LPDPB lpDPB){
    __asm__ __volatile__(
        "push rsi\n\t"
        "push rbp\n\t"
        "mov rsi, rcx\n\t"
        "mov rbp, rdx\n\t"
        "mov eax, 0x5300\n\t"
        "int 0x41\n\t"
        "pop rbp\n\t"
        "pop rsi"
    );
}

VOID GenerateNewPSP(LPVOID lpPSPAddress, DWORD dwSizeOfPSPAllocation){
    __asm__ __volatile__(
        "push rsi\n\t"
        "mov rsi, rdx\n\t"
        "mov rdx, rcx\n\t"
        "mov eax, 0x5500\n\t"
        "int 0x41\n\t"
        "pop rsi"
    );
}