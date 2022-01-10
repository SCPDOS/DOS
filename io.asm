    DEFAULT REL
    [map all io.map]
    BITS 64

Section loader vstart=7C00h align=1
    dw 0AA55h           ;Initial signature
    mov rbp, startmsg
    mov eax, 1304h
    int 30h

    jmp short $

Section loaderData vfollows=loader align=4
startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0