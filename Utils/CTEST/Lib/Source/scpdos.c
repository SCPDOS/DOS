
/* Start of the low level scpdos library which will be used to build stdlib.*/

/*Uses SYS V-64 ABI
 * Arguments passed in:              rdi, rsi, rdx, rcx, r8, r9 then stack
 * Return value is passed in:        rax, rdx. Low qword in rax.
 * Registers that can be clobbered:  rax, rdi, rsi, rdx, rcx, r8, r9, r10, r11
 * Registers that need be preserved: rbx, rsp, rbp, r12, r13, r14, r15 
 */

 /* PLEASE USE THE MS STANDARD PLEASE PLEASE*/

VOID WriteString(LPCSTR* lpString){
    __asm__ __volatile__ (
    "mov rdx, rdi \n\t"
    "mov eax, 0x0900 \n\t"
    "int 0x41 \n\t"
    );
}

VOID Exit(RETURN_CODE returnCode){
    __asm__ __volatile__ (
    "and edi, 0xFF\n\t"
    "mov eax, 0x4C00 \n\t"
    "or eax, edi\n\t"
    "int 0x41"
    );
}
RETURN_CODE Wait(PROCESS_ID processId){
    __asm__ __volatile__ (
    "mov eax, 0x4D00\n\t"
    "int 0x41\n\t"
    );   
}

HANDLE CreateFile(DWORD dwDesiredAccess, LPCSTR lpFileName){
    __asm__ __volatile__ (
    "mov rdx, rdi\n\t"
    "mov ecx, esi \n\t"
    "mov eax, 0x3C00\n\t"
    "int 0x41\n\t"
    "mov ecx, -1\n\t"
    "cmovc eax, ecx"
    );  
}

HANDLE OpenFile(DWORD dwDesiredAccess, DWORD dwShareMode,\
    LPSECURITY_ATTRIBUTES lpSecurityAttributes){
    __asm__ __volatile__ (
    "mov rax, -1\n\t"
    );  
}