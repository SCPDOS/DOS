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
    mov ah, 82h
    mov al, 52  ;Read 52 sectors
    mov ecx, 33 ;Start at sector 33
    mov rbx, 200000h    ;Start at 2Mb range
    int 33h
    jc exit

    xor dl, dl
    mov ah, 83h
    mov al, 52  ;Write 52 sectors
    mov ecx, 100h ;Start at sector 256
    mov rbx, 200000h    ;Start at 2Mb range
    int 33h
    jc exit

    mov ecx, 0D00h ;Number of qwords in 52 sectors
    xor eax, eax
    mov rdi, 200000h
    rep stosq   ;Clear buffer

    xor dl, dl
    mov ah, 82h
    mov al, 52  ;Read 52 sectors
    mov ecx, 33 ;Start at sector 33
    mov rbx, 200000h    ;Start at 2Mb range
    int 33h
    jc exit

    xor dl, dl
    mov ah, 82h
    mov al, 52  ;Read 52 sectors
    mov ecx, 100h ;Start at sector 256
    mov rbx, 300000h    ;Start at 3Mb range
    int 33h
    jc exit

    mov ecx, 0D00h ;Number of qwords in 52 sectors
    mov rdi, 200000h
    mov rsi, 300000h
    repe cmpsq   ;Clear buffer
    test ecx, ecx
    jnz exit1
lp0:
    mov rbp, strngok
    mov eax, 1304h
    int 30h
lp1:
    mov rbp, strng
    mov eax, 1304h
    int 30h
    jmp $
exit1:
    mov rbp, strngok
    mov eax, 1304h
    int 30h
exit:
    xchg bx, bx
    jmp short lp1
startmsg db "Starting SCP/DOS...",0Ah,0Dh,0
strng db 0Ah, 0Dh,"Invalid IO.SYS. System Halting.",0
strngok db 0Ah, 0Dh, "Success!",0
strngnok db 0Ah, 0Dh, "Fail!",0