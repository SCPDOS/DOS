;Dos default char functions live here

stdinReadEcho:     ;ah = 01h
;Return char that has been read and echoed in al
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [conPtr]   ;Get ptr to current con device header
    call goDriver

    cmp byte [singleIObyt], 00h
    jz .stdireexit
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call goDriver   ;rbx has reqheader ptr
.stdireexit:
    mov al, byte [singleIObyt]
    ret

stdoutWrite:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov byte [.stdoutWriteBuffer], dl
    lea rbx, charReqHdr ;Get the address of this request block
    lea rdx, .stdoutWriteBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [conPtr]   ;Get ptr to current con device header
    call goDriver
    ret
.stdoutWriteBuffer db 0
stdauxRead:        ;ah = 03h
stdauxWrite:       ;ah = 04h
stdprnWrite:       ;ah = 05h
directCONIO:       ;ah = 06h
waitDirectInNoEcho:;ah = 07h
;Return char in al
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [conPtr]   ;Get ptr to current con device header
    call goDriver
    mov al, byte [singleIObyt]
    ret
waitStdinNoEcho:   ;ah = 08h
    ret
printString:       ;ah = 09h
    xor ecx, ecx    ;Clear char counter
    mov eax, "$"    ;Terminating char
    mov rdi, rdx    ;Set up for scasb
.ps0:   ;Search for $ to get count of chars
    scasb
    je .ps1
    inc ecx
    jmp short .ps0
.ps1:   ;Use handle 
    lea rbx, charReqHdr ;Get the address of this request block
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    
    mov rsi, qword [conPtr]   ;Get ptr to current con device header
    call goDriver   ;Called with rbx pointing to the request header

    mov rbx, qword [oldRSP]
    mov al, byte [rbx+callerFrame.rax]      ;Gets al to preserve it
    ret
buffStdinInput:    ;ah = 0Ah
checkStdinStatus:  ;ah = 0Bh
clearbuffDoFunc:   ;ah = 0Ch