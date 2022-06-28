;-----------------------------------------------------------------------:
;                  DOS default char functions live here                 :
;                                                                       :
; All input Char functions wait for input. Only directConIO doesnt wait :
;                                                                       :
;Rules for the naming of the DOS functions                              :
;If the name is <name>     => Has NO break checking and no echo         :
;If the name is <name>_B   => Has Break checking and no echo            :
;If the name is <name>_E   => Has No Break checking AND Echo to STDOUT  :
;If the name is <name>_BE  => Has Break checking AND Echo to STDOUT     :
;                                                                       :
;-----------------------------------------------------------------------:

charIn_BE:     ;ah = 01h
;Return char that has been read and echoed in al
    call charIn_B
    test al, al
    jz .stdireexit
    call charOut_B.skipEP    ;Output it to screen
.stdireexit:
    ret

charOut_B:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov al, dl
.skipEP:  ;Internal function Entry Point, with char in al
    push rsi
    mov byte [singleIObyt], al
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call wByteSetup ;Puts in rbx the request block
    call goDriver
    pop rsi
    ret
auxIn_B:        ;ah = 03h
auxOut_B:       ;ah = 04h
prnOut_B:       ;ah = 05h
directConIO:    ;ah = 06h
;Only special thing about this function is that it doesn't wait for input.
charIn:         ;ah = 07h
;Return char in al from STDIN
charIn_B:       ;ah = 08h
;Return char in al from STDIN
    call rByteSetup
    mov rsi, qword [vConPtr]   ;Get ptr to current con device header
    call goDriver
    mov al, byte [singleIObyt]  ;Get byte in al to return as return value
    ret
printString_B:      ;ah = 09h
    mov rsi, rdx    ;Set up for scasb
.ps0:
    lodsb   ;Get char in al and inc rsi
    cmp al, "$" ;End of string char?
    je .ps1
    call charOut_B.skipEP
    jmp short .ps0
.ps1:
    ret
buffCharInput_BE:  ;ah = 0Ah
;Works as the main input function for the vCon keyboard buffer
checkStdinStatus:  ;ah = 0Bh
;Returns the status of the driver controlling vCon
clearbuffDoFunc:   ;ah = 0Ch
;Clears any buffers and issues a console command
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
