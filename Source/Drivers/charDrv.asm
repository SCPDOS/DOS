commonStrat:
;DOS calls this function with rbx=Ptr to request header
    mov qword [reqHdrPtr], rbx
    ret
nulStrat:
    mov word [rbx + drvReqHdr.status], drvDonStatus    ;Set done bit directly
nulIntr:
    ret
conDriver:
    push rax
    push rbx
    mov rbx, qword [reqHdrPtr]
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .conWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4
    jz .conRead
    cmp al, 5
    jz .conNondestructiveRead
    cmp al, 6
    jz .conInputStatus
    cmp al, 7
    jz .conFlushInputBuffers
    cmp al, 8
    jz .conWrite
    cmp al, 9
    jz .conWrite
    cmp al, 0Ah
    jz .conOutputStatus
    jmp short .conExit  ;All other valid functions return done
.conWriteErrorCode:     ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.conExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rbx
    pop rax
    ret
.conRead:    ;Function 4
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .conWriteErrorCode

    push rdi
    push rcx
    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cre1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
    cmp byte [.conBuf], 0   ;Does the buffer contain a zero?
    jnz .cre3   ;No, get the buffer value
    xor eax, eax
    int 36h
.cre11:
    stosb
    test al, al ;Was the ascii code stored 0?
    jnz .cre12  ;No, skip storing scancode in buffer
    mov byte [.conBuf], ah  ;Save scancode
.cre12:
    inc ecx ;Inc chars stored in buffer
    jmp short .cre1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rdi
    jmp short .conExit
.cre3:
    mov al, byte [.conBuf]  ;Get the buffer value
    mov byte [.conBuf], 0   ;Reset the buffer value
    jmp short .cre11

.conNondestructiveRead:  ;Function 5
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ndInNoWaitPkt_size
    jne .conWriteErrorCode
    cmp byte [.conBuf], 0
    jnz .cnr2
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr1        ;If zero clear => no key, go forwards
    ;Keystroke available
.cnr0:
    mov byte [rbx + ndInNoWaitPkt.retbyt], al   ;Move char in al
    jmp .conExit
.cnr1: ;No keystroke available
    mov word [rbx + ndInNoWaitPkt.status], 0200h   ;Set busy bit
    jmp .conExit
.cnr2:
    mov al, byte [.conBuf]  ;Copy scancode but dont reset it
    jmp short .cnr0   ;Keystroke is available clearly

.conInputStatus:         ;Function 6
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    jmp .conExit ;Exit, device ready

.conFlushInputBuffers:   ;Function 7
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    mov byte [.conBuf], 0   ;Clear buffer
.cfib0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .conExit     ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .cfib0

.conWrite:   ;Function 8 and 9
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .conWriteErrorCode

    push rsi
    push rcx
    mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    int 49h ;Fast print char
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rsi
    jmp .conExit
.conOutputStatus:   ;Function 0Ah
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .conWriteErrorCode
    jmp .conExit

.conBuf db 0    ;Single byte buffer
fastOutput:         ;This CON driver supports Int 49h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
ctrlBreak:
;CON Int 3Bh handler to detect CTRL+BREAK
    mov byte [conDriver.conBuf], 03h    ;Place a ^C in buffer
    iretq

clkDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
    mov rbx, qword [reqHdrPtr]
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .clkWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 04h
    jz .clkRead
    cmp al, 06h
    jz .clkInputStatus
    cmp al, 07h
    jz .clkFlushInputBuffers
    cmp al, 08h
    jz .clkWrite
    cmp al, 09h
    jz .clkWrite
    jmp short .clkExit  ;All other valid functions return done immediately!
.clkNotFunctioning:
    mov al, 02h ;Device not ready error
.clkWriteErrorCode:
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.clkExit:
    or word [rbx + drvReqHdr.status], 0100h ;Merge done bit
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

.clkRead:           ;Function 4
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .clkWriteErrorCode

    mov rsi, rbx    ;Save rbx temporarily in rsi
    mov rbp, qword [rbx + ioReqPkt.bufptr]    ;Save the clock struc ptr in rbp
    mov ax, word [.clkDate] ;Get the clock date
    mov word [rbp + clkStruc.dateWord], ax
    xor ah, ah
    int 3Ah         ;Read the system timer
    test al, al     ;Check to see if midnight has passed?
    jz .clkr1       ;Nope, now just time 
    xor ah, ah
    ;This works as al should keep count of the # of days passed since last read
    add word [rbp + clkStruc.dateWord], ax
    add word [.clkDate], ax ;Add to internal date counter too
.clkr1:
    mov byte [rbp + clkStruc.hours], cl   ;Save hours
    movzx edx, dx
    mov ebx, edx  ;Save the minutes/seconds/hseconds count
    mov eax, edx
    xor edx, edx
    mov eax, ebx
    mov ecx, 1092   
    div ecx
    mov byte [rbp + clkStruc.minutes], al
    mov eax, edx    ;Get remainder in eax
    lea eax, dword [eax + 4*eax]    ;Multiply by 5
    xor edx, edx
    mov ecx, 91 ;5*18.2
    div ecx
    mov byte [rbp + clkStruc.seconds], al
    mov eax, edx    ;Get remainder in eax
    ;lea eax, dword [eax + 4*eax]
    ;add eax, edx    ;Essentially multiply by 6
    mov byte [rbp + clkStruc.hseconds], al
    mov rbx, rsi    ;Return the packet pointer back to rbx
    jmp .clkExit

.clkInputStatus:    ;Function 6
;Always return ready
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .clkWriteErrorCode
    jmp .clkExit
.clkFlushInputBuffers:  ;Function 7
;Always return done immediately
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], flushReqPkt_size
    jne .clkWriteErrorCode
    jmp .clkExit

.clkWrite:          ;Functions 8 and 9
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .clkWriteErrorCode

    mov rsi, rbx    ;Save rbx temporarily in rsi
    mov rbp, qword [rbx + ioReqPkt.bufptr]    ;Save the clock struc ptr in rbp
    mov ax, word [rbp + clkStruc.dateWord]    ;Get date word
    mov word [.clkDate], ax ;Save date internally

    xor ebx, ebx    ;Clear temporary lo count register
    movzx eax, byte [rbp + clkStruc.hseconds]
    mov cl, 5
    div cl          ;Divide al by 5
    xor ah, ah      ;Remove the remainder
    add ebx, eax    ;Add the hseconds to final value
;Using the decimal part of this formula for the low count
;LoCount = (Minutes * 1092.38) + (Seconds * 18.21) + (Hundreths * .182)
    mov al, byte [rbp + clkStruc.seconds]
    mov ecx, 18
    mul ecx  
    add ebx, eax

    xor edx, edx
    movzx eax, byte [rbp + clkStruc.minutes]
    mov ecx, 1092
    mul ecx
    add ebx, eax
    mov edx, ebx    ;edx now has low count
    movzx ecx, byte [rbp + clkStruc.hours]
    mov ah, 01h     ;Set the system time
    int 3Ah

    mov rbx, rsi
    jmp .clkExit

.clkBCDtoHex:
;Converts a BCD value to a Hex byte
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    mov ecx, eax    ;Save al in ecx
    and eax, 0Fh    ;Get lower nybble
    and ecx, 0F0h   ;Get upper nybble
    shr ecx, 4      ;Shift upper nybble value down
.cbth0:
    add eax, 10
    loop .cbth0
    pop rcx
    ret

.clkHexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    xor ecx, ecx
.chtb0:
    cmp eax, 10
    jb .chtb1
    sub eax, 10
    inc ecx
    jmp short .chtb0
.chtb1:
    shl ecx, 4  ;Move to upper nybble
    or al, cl   ;Move upper nybble into al upper nybble
    pop rcx
    ret
.clkDate    dw 0    ;Number of days since 01/01/1980

;COM Driver headers and main interrupt strat
com1Intr:
    mov byte [comIntr.comDevice], 0
    jmp short comIntr
com2Intr:
    mov byte [comIntr.comDevice], 1
    jmp short comIntr
com3Intr:
    mov byte [comIntr.comDevice], 2
    jmp short comIntr
com4Intr:
    mov byte [comIntr.comDevice], 3
comIntr:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    mov rbx, qword [reqHdrPtr]
    mov al, 03h ;Unknown Command
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    ja .comWriteErrorCode ;If yes, error!

    mov al, byte [rbx + drvReqHdr.cmdcde]
    cmp al, 4   ;Read Character(s)
    jz .comRead
    cmp al, 5   ;Non-destructive read, acts like fast read 1 char if available
    jz .comNondestructiveRead   
    cmp al, 6   ;Read Input Status, always return with Busy bit = 0
    jz .comReadInputStatus
    cmp al, 7   ;Flush read buffers, return done
    jz .comFlushInputBuffers
    cmp al, 8
    jz .comWrite
    cmp al, 9
    jz .comWrite
    cmp al, 0Ah
    jz .comOutputStatus ;Return Clear to send bit inverted for busy bit
    jmp short .comExit  ;All other valid functions should return done
.comErrorNoCount:
    mov al, 02h ;Unknown device
    jmp short .comWriteErrorCode
.comReadError:
    mov edx, 0Bh
.comWriteError:
    mov edx, 0Ah
.comError:
    mov dword [rbx + ioReqPkt.tfrlen], ecx ;Store actual transferred chars
    mov ecx, 02h    ;Unknown device
    cmp al, 0FEh    ;Invalid COM port
    cmove edx, ecx  ;Only move unknown device error code if invalid COM port
    mov al, dl      ;Move dl to al to store error code
.comWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.comExit:
    or word [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret

.comRead:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .comWriteErrorCode

    mov rdi, qword [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
.cr1:
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cre2
.cr11:  ;Blocking wait, could be an infinite loop. Imitate basic DOS driver
    mov eax, 02h    ;Recieve 
    mov dl, byte [.comDevice]    ;Get transacting com device
    cbw     ;Zero extend to upper byte
    int 34h ;Recieve Char
    jc .comError
    cmp ah, 80h ;Did a "timeout" occur? If so, keep waiting
    je .cr11
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short .cr1
.cre2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp short .comExit

.comReadInputStatus:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .comWriteErrorCode
    mov word [rbx + statusReqPkt.status], 0 ;Chars ready to read status
    jmp short .comExit

.comNondestructiveRead:
;Acts like a "read one character if there is one" function
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ndInNoWaitPkt_size
    jne .comWriteErrorCode
.cndr1:
    mov eax, 02h    ;Recieve 
    mov dl, byte [.comDevice]    ;Get transacting com device
    cbw     ;Zero extend to upper byte
    int 34h ;Recieve Char
    jc .comErrorNoCount ;Dont save a char transfer number
    cmp ah, 80h ;Did a "timeout" occur? If so, return with busy = 1
    je .cndr2
    mov byte [rbx + ndInNoWaitPkt.retbyt], al   ;Get next char
    jmp short .comExit
.cndr2:
    mov word [rbx + ndInNoWaitPkt.status], 200h ;Busy bit set
    jmp short .comExit

.comFlushInputBuffers:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], flushReqPkt_size
    jne .comWriteErrorCode
.cfib0:
    mov dl, byte [.comDevice]
    cbw
    mov eax, 02h    ;Recieve
    int 34h
    jc .comErrorNoCount
    cmp ah, 80h ;Keep looping until ah = 80h (no more chars in buffer)
    jne .cfib0
    jmp .comExit

.comWrite:
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .comWriteErrorCode

    mov rsi, qword [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
.cw1: 
    cmp ecx, dword [rbx + ioReqPkt.tfrlen]
    je .cw2
    lodsb   ;Get char into al, and inc rsi
    mov ah, 01h ;Move function number into ah
    mov dl, byte [.comDevice]
    cbw     ;Zero extend to upper byte
    int 34h ;Transmit char
    jc .comError
    inc ecx
    jmp short .cw1 ;keep printing until all chars printed
.cw2:
    mov dword [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp .comExit

.comOutputStatus:
;Read MODEM status
    mov al, 05h ;Bad request structure length?
    cmp byte [rbx + drvReqHdr.hdrlen], statusReqPkt_size
    jne .comWriteErrorCode

    mov dl, byte [.comDevice]
    cbw     ;Zero extend to upper byte
    mov ah, 03h     ;Get status
    int 34h
    jc .comErrorNoCount
    and eax, 10h ;Isolate bit 4 of al, clear to set, and clear all other bits
    shl eax, 5   ;Shift it up to bit 9 (busy bit in status word) 
    not eax      ;Bitwise inversion
    and eax, 200h   ;Isolate bit 9
    mov word [rbx + rbx + drvReqHdr.status], ax  ;Add the busy bit
    jmp .comExit
.comDevice   db 0