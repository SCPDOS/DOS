    BITS 64
    ORG 7C00h
    dw 0AA55h ;Initial signature
    mov rbp, strng
    mov ax, 1304h
    int 30h
    jmp $
    
strng db 0Ah, 0Dh,"Invalid IO.SYS found...",0
