    
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
    
    jmp short $

    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
loader ENDS

resident    SEGMENT USE64

resident    ENDS

END