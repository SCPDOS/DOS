    
    .x64p

    INCLUDE fatStruc.inc
    INCLUDE dosStruc.inc
    INCLUDE driverStruc.inc


loader SEGMENT USE64
    ASSUME ds:FLAT, es:FLAT

    dw 0AA55h           ;Initial signature
    xchg bx, bx
    lea rbp, startmsg   ;Get the RIP relative address of message
    mov eax, 1304h
    int 30h
    mov ebp, edx        ;Save the boot drive in bp
    mov ecx, 0C0000100h ;Read MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in
    lea rsi, dataSegPtr
    mov qword ptr [rsi], rdi    ;Save the pointer to the data segment 
    mov rcx, 1000h      ;Move 32Kb high
    push rdi
    rep movsq
    pop rdi
    add rdi, dataSegPtr + SIZEOF dataSegPtr  
    ;Add the size to the offset of the last element in dataSeg
    jmp rdi
    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
loader ENDS

dataSeg SEGMENT BYTE USE64     
    dataSegPtr  dq ?    ;Pointer to the data Segment itself
dataSeg ENDS

resident    SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
    jmp short $
resident    ENDS

END