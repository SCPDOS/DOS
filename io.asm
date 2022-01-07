    DEFAULT REL
    [map all io.map]
    BITS 64
Section loader vstart=7C00h align=1
    dw 0AA55h           ;Initial signature
    xchg bx, bx
    mov ecx, 0C0000100h ;Read fs base into rdx:rax
    rdmsr
    mov rbp, strng
    mov ax, 1304h
    int 30h
    jmp $
strng db 0Ah, 0Dh,"Invalid IO.SYS found...",0
