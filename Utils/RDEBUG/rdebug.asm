;ROM Debugger activator.
;Tests if address of Int 38h is not equal to Int 39h
;If it is not, it scans the 64Kb block after the ROMBIOS for the string SCPBIOS
;If it finds it, it triggers Int 38h to enter the debugger
[map all ./Utils/RDEBUG/Listings/rdebug.map]
[DEFAULT REL]
BITS 64
    mov eax, 3538h  ;Get the ptr for interrupt 38h in rbx
    int 41h
    mov rdi, rbx
    mov eax, 3539h
    int 41h
    cmp rdi, rbx
    je .badExit
    ;Now we scan 64Kb from rdi
    mov al, "S"
    mov ecx, 10000h
.keepSearching:
    repne scasb
    je .potential
.badExit:
    lea rdx, badMsg
    mov eax, 0900h
    int 41h
    mov eax, 4CFFh
    int 41h
.potential:
    push rax
    push rdi
    dec rdi
    mov rax, "SYSDEBUG"
    cmp qword [rdi], rax
    pop rdi
    pop rax
    jne .keepSearching
    int 38h ;Exits via DOS

badMsg:  db "SCP/BIOS Debugger Not Detected",0Dh,0Ah,"$"