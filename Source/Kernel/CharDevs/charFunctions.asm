;Dos default char functions live here

stdinReadEcho:     ;ah = 01h
;Return char that has been read and echoed in al
    lea rbx, secdReqHdr ;Get the address of this request block
    lea rax, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver

    cmp byte [singleIObyt], 00h
    jz .stdireexit
    lea rbx, secdReqHdr ;Get the address of this request block
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
    mov byte [singleIObyt], dl
    lea rbx, secdReqHdr ;Get the address of this request block
    lea rdx, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver
    ret
stdauxRead:        ;ah = 03h
stdauxWrite:       ;ah = 04h
stdprnWrite:       ;ah = 05h
directCONIO:       ;ah = 06h
waitDirectInNoEcho:;ah = 07h
;Return char in al
    lea rbx, secdReqHdr ;Get the address of this request block
    lea rax, singleIObyt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01

    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
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
    lea rbx, secdReqHdr ;Get the address of this request block
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver   ;Called with rbx pointing to the request header

    mov rbx, qword [oldRSP]
    mov al, byte [rbx+callerFrame.rax]      ;Gets al to preserve it
    ret
buffStdinInput:    ;ah = 0Ah
checkStdinStatus:  ;ah = 0Bh
clearbuffDoFunc:   ;ah = 0Ch
;------------------------
;  Primitive functions  :
;------------------------

;------------------------
;   Utility functions   :
;------------------------
checkBreakOnCon:
;Reads bytes from CON if there are any bytes to read and 
; if it is a ^C or CTRL+BREAK, then exit via INT 43h
    cmp byte [inDOS], 1
    je checkBreak  ;Only check ^C on first entry to DOS
    ret
checkBreak:
;Returns in al the keystroke that is available IF one is available
; or al=0 if no keystroke available
    push rbx
    push rsi
    mov rsi, qword [vConPtr] ;Get pointer to Console device driver
    xor eax, eax
    ;Place command code and a zero status word at the same time
    mov al, drvNONDESTREAD
    mov dword [secdReqHdr + nonDestInNoWaitReqPkt.cmdcde], eax
    ;Place the packet size in the hdrlen field
    mov al, nonDestInNoWaitReqPkt_size
    mov byte [secdReqHdr + nonDestInNoWaitReqPkt.hdrlen], al
    lea rbx, secdReqHdr
    call goDriver   ;Called with rsi and rbx with appropriate pointers
    ;Check if the busy bit is set (No keystroke available)
    test word [secdReqHdr + nonDestInNoWaitReqPkt.status], drvBsyStatus
    jz .charFound
.exit:
    pop rsi
    pop rbx
    ret
.charFound:
;Keystroke available, proceed
    mov al, byte [secdReqHdr + nonDestInNoWaitReqPkt.retbyt]    ;Get char
    cmp al, ETX ;BREAK/^C =ASCII 03h
    jne .exit   ;If not equal exit
;Now we pull the char out of the buffer
    xor eax, eax
    mov al, drvREAD ;Read command
    mov dword [secdReqHdr + ioReqPkt.cmdcde], eax
    ;Place packet size
    mov byte [secdReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    ;Place pointers and number of chars
    mov dword [secdReqHdr + ioReqPkt.tfrlen], 1 ;One char to be read
    lea rax, singleIObyt    ;IO Byte buffer
    mov qword [secdReqHdr + ioReqPkt.bufptr], rax
    call goDriver   ;RSI and RBX as before
    ret ;Stopgap right now, do nothing


swapVConDriver:
;Sets up the vCon to use the alternative device driver 
    push rdi
    call vConUseAlt
    mov rdi, qword [currentSFT] ;Get current SFT pointer
    mov qword [vConOldSFT], rdi ;Save the SFT ptr in var
    pop rdi
    ret
;These functions set/clear whether vCon should use vConOldSFT or vConPtr
;If vConDrvFlg = 1 => Use vConOldSFT
;If vConDrvFlg = 0 => Use vConPtr
vConUseAlt:
    mov byte [vConDrvFlg], 1    ;Set to use alternative driver
    ret
vConUseDef:
    mov byte [vConDrvFlg], 0    ;Clear to use default driver
    ret

getVConDriverPtr:
;Return: rdi = vCon Device Driver pointer
    mov rdi, qword [vConPtr]  ;Get the usual vCon Ptr
    test byte [vConDrvFlg], 1   ;If set, use alternative driver
    jz .exit
    mov rdi, qword [vConOldSFT] ;Get the alt. vCon Ptr
    mov rdi, qword [rdi + sft.qPtr] ;Get dev drv from SFT
.exit:
    ret
