;-----------------------------------:
;          Driver routines          :
;-----------------------------------:
drivers:
conHdr:
    dq auxHdr
    dw 0813h
    dq commonStrat
    dq conDriver
    db "CON     "
auxHdr:
    dq prnHdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "AUX     "
prnHdr:
    dq clkHdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "PRN     "
clkHdr:
    dq msdHdr
    dw 08008h
    dq commonStrat
    dq clkDriver
    db "CLOCK$  "
msdHdr:
    dq com1Hdr
    dw 00800h   ;Once Generic IO implemented, change to 00840h
    dq commonStrat
    dq msdDriver
    db 0,0,0,0,0,0,0,0
com1Hdr:
    dq com2Hdr
    dw 08000h
    dq commonStrat
    dq com1Intr
    db "COM1    "
com2Hdr:
    dq com3Hdr
    dw 08000h
    dq commonStrat
    dq com2Intr
    db "COM2    "
com3Hdr:
    dq com4Hdr
    dw 08000h
    dq commonStrat
    dq com3Intr
    db "COM3    "
com4Hdr:
    dq lpt1Hdr
    dw 08000h
    dq commonStrat
    dq com4Intr
    db "COM4    "
lpt1Hdr:
    dq lpt2Hdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "LPT1    "
lpt2Hdr:
    dq lpt3Hdr
    dw 0A040h
    dq nulStrat
    dq nulIntr
    db "LPT2    "
lpt3Hdr:
    dq -1
    dw 0A040h
    dq nulStrat
    dq nulIntr
    dq "LPT3    "

commonStrat:
;DOS calls this function with rbx=Ptr to request header
    mov qword [reqHdrPtr], rbx
    ret
reqHdrPtr  dq 0    ;Where the default device drivers store the ReqPtr

nulStrat:
    mov word [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
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
    test al, al ;Was the ascii code 0?
    jnz .cre12  ;No, skip storing scancode
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
    cmp byte [rbx + drvReqHdr.hdrlen], nonDestInNoWaitReqPkt_size
    jne .conWriteErrorCode
    cmp byte [.conBuf], 0
    jnz .cnr2
    mov ah, 01h     ;Get key if exists
    int 36h
    jz .cnr1        ;If zero clear => no key, go forwards
    ;Keystroke available
.cnr0:
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Move char in al
    jmp .conExit
.cnr1: ;No keystroke available
    mov word [rbx + nonDestInNoWaitReqPkt.status], 0200h   ;Set busy bit
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
;When counting the number of days, first compute the number of years since
; 1980 and your year. 
;Then, using the table below, find the number of leap years between 1980
; and (YourYear - 1). 
;Then do (YourYear - 1980) * 365 + numberOfLeapYears to get the number of 
; days since 01/01/1980 and 01/01/YourYear.
;Use the months table to get the number of days in a normal month as leap 
; years are added using the previous comment.
;Finally check if the date is after 28th Feb. If it is, check if your year is 
; a leap year using the table. If it is, add an extra day.
.clkLeapYears:
    db 00, 04, 08, 12, 16, 20, 24, 28, 32, 36, 40, 44, 48, 
    db 52, 56, 60, 64, 68, 72, 76, 80, 84, 88, 92, 96
.clkMonths:
    db 31, 28, 31, 30, 31, 30, 31, 31, 30, 31, 30, 31
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
    cmp byte [rbx + drvReqHdr.hdrlen], nonDestInNoWaitReqPkt_size
    jne .comWriteErrorCode
.cndr1:
    mov eax, 02h    ;Recieve 
    mov dl, byte [.comDevice]    ;Get transacting com device
    cbw     ;Zero extend to upper byte
    int 34h ;Recieve Char
    jc .comErrorNoCount ;Dont save a char transfer number
    cmp ah, 80h ;Did a "timeout" occur? If so, return with busy = 1
    je .cndr2
    mov byte [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Get next char
    jmp short .comExit
.cndr2:
    mov word [rbx + nonDestInNoWaitReqPkt.status], 200h ;Busy bit set
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

msdDriver:
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    mov rbx, qword [reqHdrPtr]  ;Get the ptr to the req header in rbx
    cmp byte [rbx + drvReqHdr.cmdcde], 24 ; Command code bigger than 24?
    mov al, 03h
    ja .msdWriteErrorCode ;If yes, error!
    mov al, 01h ;Unknown Unit Error
    cmp byte [rbx + drvReqHdr.unitnm], 05h  ;Unit greater than 5 is invalid
    ja .msdWriteErrorCode ;If yes, error!
    mov al, byte [rbx + drvReqHdr.cmdcde]   ;Get command code in al
    test al, al
    jz .msdInit
    cmp al, 01
    jz .msdMedChk
    cmp al, 02
    jz .msdBuildBPB
    cmp al, 03
    jz .msdIOCTLRead
    cmp al, 04
    jz .msdRead
    cmp al, 08
    jz .msdWrite
    cmp al, 09
    jz .msdWriteVerify
    cmp al, 12
    jz .msdIOCTLWrite
    cmp al, 13
    jz .msdDevOpen
    cmp al, 14
    jz .msdDevClose
    cmp al, 15
    jz .msdRemovableMedia
    cmp al, 19
    jz .msdGenericIOCTL
    cmp al, 23
    jz .msdGetLogicalDev
    cmp al, 24
    jz .msdSetLogicalDev
    jmp short .msdDriverExit    ;All other valid functions exit done
.msdIOError:  ;In Read and Write errors, rbp points to the dev struc
    mov rbx, rbp
    movzx eax, al   ;Number of IO-ed sectors in last request
    add esi, eax    ;esi Keeps sector count across transfers
    mov dword [rbx + ioReqPkt.tfrlen], esi ;Save number of IO-ed sectors
;Now fall through to general error
.msdGenDiskError:
    mov ah, 01h
    xor dl, dl  ;Work around bug that fails request if dl > 7Fh
    int 33h ;Read status of last operation
    cmp ah, 06h ;Mock Seek response (device not present)
    mov al, 02h ;Give device not ready error (sensibly I think)
    je .msdWriteErrorCode 
    mov al, 0Ch ;Preliminary General Error Faults
    cmp ah, -1  ;Sense operation failed
    je .msdWriteErrorCode 
    cmp ah, 20h ;Gen. ctrlr. failure. Consider new error code to halt system.
    je .msdWriteErrorCode
;Device Not Ready
    mov al, 02h  ;Device not ready code
    cmp r8b, al  ;SCSI Not ready commands start with 2
    je .msdWriteErrorCode
    shr r8, 8       ;Remove Sense Key
    movzx ecx, r8w  ;Get ASC and ASCQ in cl and ch bzw.
;Write Protected
    xor al, al
    cmp cx, 0027h   ;Write protected error
    je .msdWriteErrorCode
;CRC Error
    mov al, 04h     ;CRC error code
    cmp cx, 0308h   ;LU comms CRC error (UDMA/32)
    je .msdWriteErrorCode
    cmp cx, 0010h   ;ID CRC or ECC error
    je .msdWriteErrorCode
    cmp cx, 0147h   ;Data phase CRC error detected
    je .msdWriteErrorCode
;Seek Error
    mov al, 06h     ;Seek error code
    cmp cl, 02h     ;No Seek Complete
    je .msdWriteErrorCode
;Unknown Hardware Media (Shouldn't happen with Flash Drives)
;This error should only be called if BPB not recognised for Flash Drives
    mov al, 07h
    cmp cl, 30h   ;All issues with media returns unknown media
    je .msdWriteErrorCode
;Sector Not Found
    mov al, 08h     ;Sector not found code
    cmp cl, 21h     ;Illegal Request - Invalid LBA
    je .msdWriteErrorCode
;Write faults
    mov al, 0Ah     ;Write fault
    cmp cl, 0Ch     ;Write Error ASC code
    je .msdWriteErrorCode
;Read faults
    mov al, 0Bh     ;Read fault
    cmp cl, 11h     ;Read error
    je .msdWriteErrorCode
;General Errors
    mov al, 0Ch     ;Everything else is general error
.msdWriteErrorCode:    ;Jump to with al=Standard Error code
    mov ah, 80h ;Set error bit
    mov word [rbx + drvReqHdr.status], ax
.msdDriverExit:
    or word [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.msdInit:            ;Function 0
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], initReqPkt_size
    jne .msdWriteErrorCode

    lea rbp, endptr
    mov qword [rbx + initReqPkt.endptr], rbp    ;Where the end is gonna be
    lea rbp, .msdBPBTbl
    mov qword [rbx + initReqPkt.optptr], rbp    ;Where bpb tbl is gonna be

    mov rbp, rbx ;Save the req block ptr in rbp
    xor edx, edx  ;Start from device zero
    mov byte [rbp + initReqPkt.numunt], dl   ;Zero this field, max 5
.mi0:   ;Now check each device for partitions
    cmp byte [rbp + initReqPkt.numunt], 5
    je .msdExit ;IF we are at 5 now, we exit
    mov ah, 82h ;LBA read
    mov al, 1   ;1 sector
    mov ecx, 0  ;Read sector 0
    lea rbx, msdTempBuffer  ;Get address of this space
    int 33h
    jc .msdInitError
;Now we verify if this is a BPB. Removable devices can't be partitioned (yet)
;1) Check byte 0 for EBh (short jmp) and byte 2 for a 90h (nop).
    mov al, byte [rbx]
    mov ah, byte [rbx + 2]
    cmp ax, 090EBh
    jne .mimbr
;Valid BPB found! Copy to internal table and inc lastdrive
    mov rsi, rbx
    mov eax, bpbEx_size
    mov ecx, edx    ;Temporarily save dl in ecx
    mul edx
    mov edx, ecx
    lea rdi, .msdBPBblks
    add rdi, rax
    mov ecx, bpbEx_size
    mov rax, rdi    ;Save the entry address in rax
    rep movsb   ;Copy the bpb into the bpb table
    lea rdi, .msdBPBTbl
    lea rdi, qword [rdi + 8*rdx]
    mov qword [rdi], rax
    lea rdi, .msdBIOSmap
    add rdi, rdx    ;rdx contains a number, table is a list of bytes
    mov byte [rdi], dl
    inc byte [rbp + initReqPkt.numunt]
    inc dl
    cmp dl, byte [numMSDdrv] ;Once these are equal, we have processed last dev
    jne .mi0
.msdExit:
;If one device only, copy its BPB pointer and drive number
;When HDD support implemented, this will check the number of remdevs not lastdrv
    cmp byte [rbp + initReqPkt.numunt], 1
    jne .msdexit1
;Here ONLY if one device found
    lea rsi, .msdBPBTbl
    lea rdi, qword [rsi + 8]    ;Point to next entry
    movsq   ;Copy pointer
    lea rsi, .msdBIOSmap
    lea rdi, qword [rsi + 1]
    movsb   ;Copy byte
    inc byte [rbp + initReqPkt.numunt]
.msdexit1:
    mov rbx, rbp
    jmp .msdDriverExit
.mimbr:
;Goto next device without incrementing LASTDRIVE
    inc dl
    cmp dl, byte [numMSDdrv] ;Once these are equal, we have processed last dev
    jne .mi0
    jmp short .msdExit
.msdInitError:
    mov rbx, rbp
    jmp .msdGenDiskError
.msdMedChk:          ;Function 1
;Once the BIOS function is implmented that reads the changeline, use that!
;For BIOSes that dont support the changeline, the following procedure will 
; suffice.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], mediaCheckReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + mediaCheckReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]    ;Translate unitnum to BIOS num
    test dl, 80h    ;If it is a fixed disk, no change!
    jnz .mmcNoChange
;Now we do a BIOS changeline check. If it returns 80h or 86h then check med desc
    mov ah, 16h 
    int 33h
    jc .msdGenDiskError
    cmp ah, 80h
    je .mmcNoChangeLine
    cmp ah, 86h
    je .mmcNoChangeLine
    test ah, ah ;No change?
    jz .mmcNoChange
    test ah, 1  ;Neither 80h or 86h have bit 0 set
    jnz .mmcChange
;If nothing, fall through and test manually, should never happen though
.mmcNoChangeLine:
;Now we test Media Descriptor
    mov dl, byte [rbx + mediaCheckReqPkt.medesc]    ;Media descriptor
    mov rdi, qword [.msdBPBTbl + 8*rax]
    mov rdi, qword [rdi]    ;Dereference rdi
    cmp byte [rdi + bpb32.media], dl    ;Compare media descriptor bytes
    je .mmcUnsure
.mmcChange:
    mov byte [rbx + mediaCheckReqPkt.medret], -1
    lea rax, qword [.msdDefLabel]           ;Temp, ret def label
    mov qword [rbx + mediaCheckReqPkt.desptr], rax 
    jmp .msdDriverExit
.mmcUnsure:
    mov byte [rbx + mediaCheckReqPkt.medret], 0
    jmp .msdDriverExit
.mmcNoChange:
    mov byte [rbx + mediaCheckReqPkt.medret], 1
    jmp .msdDriverExit

.msdBuildBPB:        ;Function 2
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], bpbBuildReqPkt_size
    jne .msdWriteErrorCode

    mov rsi, rbx
    movzx rax, byte [rsi + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req
    mov rbx, qword [rsi + bpbBuildReqPkt.bufptr]    ;Transfer buffer
    xor ecx, ecx    ;Read Sector 0
    mov eax, 8201h  ;LBA Read 1 sector
    int 33h
    jc .msdGenDiskError
;Check Media Descriptor, must be F0h or F8h-FFh or unknown media
    cmp byte [rbx + bpb.media], 0F0h    ;3.5" FDD standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F8h    ;FDD/Large Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0F9h    ;5.25" & 720K 3.5" Media Standard
    je .mbbpb0
    cmp byte [rbx + bpb.media], 0FCh    ;Very Obsolete Media Standards
    mov al, 07h ;Unknown media error code
    jb .msdWriteErrorCode
.mbbpb0:
    xchg rbx, rsi    ;Transf Buf(rbx) <-> ReqHdr(rsi)
    movzx rax, byte [rbx + bpbBuildReqPkt.unitnm]  ;Get unit number into rax
    mov rdi, qword [.msdBPBTbl + 8*rax] ;Get pointer to pointer to buffer
    mov rdi, qword [rdi] ;Dereference to get pointer to buffer 
    mov qword [rbx + bpbBuildReqPkt.bpbptr], rdi ;rdi -> final bpb resting place
    mov ecx, bpbEx_size/8
    rep movsq   ;Move the BPB data into the right space
    jmp .msdDriverExit
.msdIOCTLRead:       ;Function 3, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdRead:            ;Function 4
;Will read one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set sector read counter to zero
.msdr0:
    mov dh, 82h ;LBA Read Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdr0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWrite:           ;Function 8
;Will write one sector at a time.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdw0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdw0
    mov rbx, rbp
    jmp .msdDriverExit
.msdWriteVerify:     ;Function 9, writes sectors then verifies them
;Will write one sector at a time and then verify it.
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    mov rbp, rbx
    xor esi, esi  ;Set counter to zero
.msdwv0:
    mov dh, 83h ;LBA Write Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    mov dh, 84h ;LBA Verify Sectors
    call .msdBlkIOCommon
    jc .msdIOError    ;Error handler needs to add to esi the value in al
    add qword [rbp + ioReqPkt.strtsc], 200h  ;Add one sector
    add qword [rbp + ioReqPkt.bufptr], 200h  ;Add one sector
    inc esi
    cmp esi, dword [rbp + ioReqPkt.tfrlen]
    jne .msdwv0
    mov rbx, rbp
    jmp .msdDriverExit
.msdIOCTLWrite:      ;Function 12, returns done
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdDevOpen:         ;Function 13
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], openReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + openReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    inc byte [rcx + rax]  ;Inc handle cnt for given unit
    jmp .msdDriverExit
.msdDevClose:        ;Function 14
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], closeReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + closeReqPkt.unitnm]
    lea rcx, .msdHdlCnt
    dec byte [rcx + rax]  ;Dec handle cnt for given unit
    jmp .msdDriverExit
.msdRemovableMedia:  ;Function 15
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], remMediaReqPkt_size
    jne .msdWriteErrorCode

    movzx rax, byte [rbx + remMediaReqPkt.unitnm]
    lea rcx, .msdBIOSmap
    mov al, byte [rcx + rax]    ;Get BIOS number
    test al, 80h
    jz .msdDriverExit   ;If removable, busy bit is clear
    mov word [rbx + remMediaReqPkt.status], 0200h ;Set Busy bit
    jmp .msdDriverExit
.msdGenericIOCTL:    ;Function 19
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], ioctlReqPkt_size
    jne .msdWriteErrorCode

    jmp .msdDriverExit
.msdGetLogicalDev:   ;Function 23
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], getDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [.msdCurDev]
    mov byte [rbx + getDevReqPkt.unitnm], al
    jmp .msdDriverExit
.msdSetLogicalDev:   ;Function 24
    mov al, 05h ;Bad request structure length
    cmp byte [rbx + drvReqHdr.hdrlen], setDevReqPkt_size
    jne .msdWriteErrorCode

    mov al, byte [rbx + getDevReqPkt.unitnm]
    mov byte [.msdCurDev], al
    jmp .msdDriverExit

.msdBlkIOCommon:  ;Does block IO
;Called with rbp containing old rbx value and ah with function number
;Error handled by caller
;Sector count handled by caller
;Called with dh = BIOS function number
    movzx rax, byte [rbp + ioReqPkt.unitnm]
    mov dl, byte [.msdBIOSmap + rax]  ;Get translated BIOS number for req in dl
    mov rcx, qword [rbp + ioReqPkt.strtsc]  ;Get start sector
    mov rbx, qword [rbp + ioReqPkt.bufptr]  ;Get Memory Buffer
    mov ah, dh
    mov al, 01h ;Do one sector at a time 
    int 33h
    ret

.msdDefLabel db "NO NAME ",0 ;Default volume label
;LASTDRIVE default is 5
.msdCurDev   db 0  ;Dev to be used by the driver saved here! (usually 1-1)
; Except when single drive in use, in which case Drive A and B refer to device 0
.msdBIOSmap  db 5 dup (0FFh) ;Translates DOS drive number to BIOS number
.msdHdlCnt   db 5 dup (0)    ;Keeps a count of open handles to drive N
.msdBPBTbl   dq 5 dup (0)    ;BPB pointer table to be returned
.msdBPBblks  db 5*bpbEx_size dup (0) ;Max 5 bpb records of exFAT bpb size

endptr equ $