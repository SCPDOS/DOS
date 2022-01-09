    DEFAULT REL
    [map all io.map]
    BITS 64
Section loader vstart=7C00h align=1
    dw 0AA55h           ;Initial signature

    mov eax, 0600h  ;CLS
    xor ecx, ecx
    mov edx, 184Fh
    mov bh, 07h
    int 30h
    mov eax, 0200h  ;Move cursor up top
    xor edx, edx
    xor bh, bh
    int 30h
    mov rbp, startmsg
    mov eax, 1304h
    int 30h
    mov ecx, 0C0000100h
    rdmsr
    mov rbp, rax    ;Save ptr in rbp
    xchg bx, bx
lp:
    xor dl, dl
    mov rbx, rbp
    mov eax, 82FFh  ;LBA read
    mov ecx, 100h
    int 33h
    jc exit

    mov ah, 04h
    int 30h
    mov rbp, msg1
    mov eax, 1304h
    int 30h

    xor dl, dl
    mov rbx, rbp
    mov eax, 83FFh
    mov ecx, 100h
    int 33h
    jc exit   

    mov ah, 04h
    int 30h
    mov rbp, msg2
    mov eax, 1304h
    int 30h

    xor dl, dl
    mov rbx, rbp
    mov eax, 84FFh
    mov ecx, 100h
    int 33h
    jc exit

    mov ah, 04h
    int 30h
    mov rbp, msg3
    mov eax, 1304h
    int 30h
lp1:
    mov rbp, strng
    mov eax, 1304h
    int 30h
    jmp $
exit:
    xchg bx, bx
    jmp short lp1
startmsg db "Starting SCP/DOS...",0Ah,0Dh,0
strng db 0Ah, 0Dh,"Invalid IO.SYS. System Halting.",0
msg1  db "h Sectors Read",0Ah,0Dh,0
msg2  db "h Sectors Written", 0Ah, 0Dh, 0
msg3  db "h Sectors Verified", 0
