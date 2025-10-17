;Generic MBR program
BITS 16
ORG 600h    ;We start at 7C00h but relocate ourselves low

ep:
    cli ;Pause interrupts for copy low
    cld
    xor ax, ax
    mov ds, ax
    mov es, ax
    mov ss, ax
    mov sp, 7C00h
    sti
    mov si, sp
    mov di, 600h
    mov cx, 100h    
    rep movsw
    jmp 0:main  ;Set CS to 0 too
main:
    mov si, tbl1
    mov cx, 4
.lp:
    cmp byte [si], 80h  ;Is the first byte an active partition?!
    je activeFound
    add si, 10h ;Goto next table entry
    dec cx
    jnz .lp
    mov si, msg1
badExit:
    call print
    xor ax, ax
    int 16h
    int 18h
activeFound:
    mov bp, si
    xor ax, ax
    mov cx, 8
    mov di, pktptr
    push di ;Put pointer to the xfer packet on stack
    rep stosw   ;Store 8 zero words
    lea di, [si + 8]  ;Get ptr to the lbaStart field in di 
    pop si  ;Pop the xfer packet into si
    mov ch, 03h
.tryAgain:
    mov cl, 03h
.tryAgainLp:
    mov word [si], 0010h    ;Packet size and reserved zero
    mov word [si + 2], 1   ;Number of sectors to transfer
    mov word [si + 4], 07C00h ;Offset of buffer
    mov word [si + 6], 0      ;Segment of buffer
    push si
    push di
    xchg si, di
    add di, 8
    movsw   ;Copy over the DWORD from the MBR entry
    movsw
    pop di
    pop si
    mov ah, 42h
    int 13h
    jnc .readOk
    dec cl
    jnz .tryAgainLp
    dec ch
    jz .badRead
    xor ah, ah
    int 13h
    jmp short .tryAgain
.badRead:
    mov si, msg2
    jmp short badExit
.readOk:
    mov si, msg3
    cmp word [07DFEh], 055AAh
    je .okOS
    cmp word [07DFEh], 0AA55h
    jne badExit
.okOS:
    mov si, bp
    jmp 0:7C00h
    
print:
    lodsb
    test al, al
    jz .exit
    mov ah, 0Eh
    xor bx, bx
    int 10h
    jmp short print
.exit:
    ret

msg1:   db "Invalid partition table.", 0
msg2:   db "Error loading operating system",0
msg3:   db "Missing operating system",0
    times (01BEh - ($-$$)) db 00h    ;Pad the partition table to the right place

tbl1:   db 10h dup (0)
tbl2:   db 10h dup (0)
tbl3:   db 10h dup (0)
tbl4:   db 10h dup (0)

        dw 0AA55h
pktptr: ;Packet Pointer, 16 bytes in size, always past the tail