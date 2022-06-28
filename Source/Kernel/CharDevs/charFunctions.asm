;Dos default char functions live here

stdinReadEcho:     ;ah = 01h
;Return char that has been read and echoed in al
    call waitStdinNoEcho
    test al, al
    jz .stdireexit
    call stdoutWrite.inEP    ;Output it to screen
.stdireexit:
    ret

stdoutWrite:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov al, dl
.inEP:  ;Internal function Entry Point, with char in al
    push rsi
    mov byte [singleIObyt], al
    call wByteSetup ;Puts in rbx the request block
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver
    pop rsi
    ret
stdauxRead:        ;ah = 03h
stdauxWrite:       ;ah = 04h
stdprnWrite:       ;ah = 05h
directCONIO:       ;ah = 06h
waitDirectInNoEcho:;ah = 07h
waitStdinNoEcho:   ;ah = 08h
;Return char in al
    call rByteSetup
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver
    mov al, byte [singleIObyt]  ;Get byte in al to return as return value
    ret
printString:       ;ah = 09h
    mov rsi, rdx    ;Set up for scasb
.ps0:
    lodsb   ;Get char in al and inc rsi
    cmp al, "$" ;End of string char?
    je .ps1
    call stdoutWrite.inEP
    jmp short .ps0
.ps1:
    ret
buffStdinInput:    ;ah = 0Ah
checkStdinStatus:  ;ah = 0Bh
clearbuffDoFunc:   ;ah = 0Ch
;------------------------
;  Primitive functions  :
;------------------------
wByteSetup:
;Preserve all registers EXCEPT RBX= Request header pointer
    push rax
    mov ah, drvWRITE
    jmp short rByteSetup.ep
rByteSetup:
;Preserve all registers EXCEPT RBX= Request header pointer
    push rax
    mov ah, drvREAD
.ep:
    push rcx
    push rdi
    mov ecx, 1
    lea rdi, singleIObyt    ;Get address of symbol
    call secdReqCharIOReq   ;Make request, return rbx = Request header
    pop rdi
    pop rcx
    pop rax
    ret
getCharFunHandle:
;Gets the handle pointer for a device. 
; If the handle is 0,1,2, if the handle is closed, then return vConPtr.
; If the handle is 3,4, if the handle is closed, then return nullDevPtr
; Else find SFT entry, check it is char device.
; If it is disk device, transfer control to readHandle function.
; Else, return device driver pointer for device.

;Input: bx = File handle (zero extended to rbx)
;Output: CF=NC -> rdi = SFT entry 
;        CF=CY -> SFT closed, get default driver ptr

    call getSFTNdxFromHandle    ;Get a ptr to the SFT entry in rdi
    cmp byte [rdi], -1  ;SFT entry closed?
    jne .validDevice
    stc ;Set carry flag
    ret ;Return with al destroyed
.validDevice:
    call derefSFTPtr.ok    ;bx has file handle, now get sft ptr in rdi
    ret
testDeviceCharBlock:
;Input: rdi = SFT pointer
;Output: ZF=ZE => Block device, ZF=NZ => Char device
    test word [rdi + sft.wDeviceInfo], devCharDev
    ret
getDriverFromSFT:

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
    mov rdi, qword [vConCurSFT] ;Get the alt. vCon Ptr
    mov rdi, qword [rdi + sft.qPtr] ;Get dev drv from SFT
.exit:
    ret
