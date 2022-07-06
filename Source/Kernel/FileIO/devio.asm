;This file contains the primitive dev io function, which will be called 
; from charFunc.asm, hdlFunc.asm and fcbFunc.asm as all three 
; make requests for device IO.

mainCharIO:
;This is the main IO clearing function for Char IO.
; Uses two tables, as per DOS 3.3 to store parts of the request header
; Uses only the secdReqHdr and the singleIObyt as the transfer buffer
;
;Input: rsi = SFT for the file the IO is being enacted on
;       ah = {0,..., 5}, a subfunction number
;       Function 0: Read Char (drvREAD)
;       Function 1: Non Destructive Read Char (drvNONDESTREAD)
;       Function 2: Write Char (drvWrite)
; In the case the below functions are called for Disk/Net files
;   this function will return with ZF=NZ, as if the device is ready (not busy).
;       Function 3: Output Status of Device (drvOUTSTATUS)
;       Function 4: Flush Input Buffers (drvFLUSHINBUF)
;       Function 5: Non Destructive Read Char, Busy Bit forcefully set.
;
; If and only if ah = 02h, al = Char to write to device
;
;
;Output:    If Read:                        AL = Char Read (ZF = NZ)
;           If ND Read or Output Status:    ZF = ZE => Device NOT ready (busy)
;                                           ZF = NZ => Device ready (not busy)
;                                               If ND read, AL = Next Char
; For Files/Net:
;           If ND Read, get char without advancing file pointer (sft.dCurntOff).
;           If Read, get char while advancing file pointer (sft.dCurntOff).
;           For Read and ND Read:
;               ZF = ZE when file pointer hits EOF. Else ZF = NZ.
;               Read returns ^Z on EOF.
;           If Write, char places at sft.dCurntOff and sft.dCurntOff advanced.
;   All other calls will return with ZF = NZ => Device ready (not busy)
;
;All registers preserved EXCEPT for ax and flags

;Reference Request Header
;Common Section:
;    .hdrlen resb 1  ;Length of the request header
;    .unitnm resb 1  ;Unit number, meaningless for character devs
;    .cmdcde resb 1  ;Command code
;    .status resw 1  ;Status word
;    .devptr resq 1  ;Reserved. Unused
;Below used by calls 0,1,2,5
;    .medesc/.retbyt resb 1  ;ioReqPkt / ndInNoWaitPkt
;Below used by calls 0,2
;    .bufptr resq 1
;    .tfrlen resd 1
;Below never used, can reduce buffer size in SDA by 10h
;    .strtsc resq 1
;    .desptr resq 1

    ;Setup parts of the req ptr AS IF IT WERE FOR IO
    push rbx    ; PRESERVE THE STATE OF RBX OR GOD HELP YOU ALL!!!!!
    lea rbx, singleIObyt   ;Get lea of return byte pointer
    mov qword [secdReqHdr + ioReqPkt.bufptr], rbx
    pop rbx
    mov dword [secdReqHdr + ioReqPkt.tfrlen], 1 ;Request 1 byte if read/write
    mov word [singleIObyt], ax  ;Save al for transfer and ah for preservation
    test word [rsi + sft.wDeviceInfo], devRedirDev
    jnz .notChar
    test word [rsi + sft.wDeviceInfo], devCharDev
    jz .notChar
;Char devices only here
    call dosPushRegs    ;Save context here
    lea rbx, ioRqCmdErrTbl  ;Get the table pointer in rbx
    movzx ecx, ah   ;Get the command code into ecx (zero xtnd rcx)
    shl ecx, 2  ;Multiply by four since DWORD entries
    mov ecx, dword [rbx + rcx]  ;Get entry
    mov dword [secdReqHdr], ecx ;First three entries map to first three entries
    ;Now set up cx for status word
    xor ecx, ecx
    cmp ah, 05h ;Did the caller request command 5?
    jne .skipBsySet
    or ecx, drvBsyStatus
.skipBsySet:
    xchg cx, word [secdReqHdr + drvReqHdr.status] ;Swap error flags with status
    ;cl has flags, ch has garbage (status is zeroed by xchg)
    lea rbx, secdReqHdr
    call goDriverChar   ;GoDriver with an SFT in rsi
    mov di, word [secdReqHdr + drvReqHdr.status]    ;Get status
    test edi, drvErrStatus
    jnz .error
.ignoreRet:
    cmp byte [secdReqHdr + drvReqHdr.cmdcde], drvNONDESTREAD
    jne .notNDRead
    mov al, byte [secdReqHdr + ndInNoWaitPkt.retbyt]    ;Get request byte
    mov byte [singleIObyt], al  ;Store it here to make algorithm streamlined
.notNDRead:
    mov ah, byte [secdReqHdr + drvReqHdr.status + 1]  ;Get hibyte of status word
    not ah
    and ah, (drvBsyStatus >> 8) ;Set ZF=ZE if BSY set on for NDRead commands
    call dosPopRegs ;Get back the context
    mov ax, word [singleIObyt]  ;Get back OG high byte and return char in al
    return ;Return to caller
.error:
    ;cl has flags
    mov ah, cl
    call charDevErr
    cmp al, critRetry   ;Did the user request a retry
    jne .errorIgnore
    ;Fallthrough => Retry
    call dosPopRegs ;Pop context (return the regs as on entry)
    jmp mainCharIO  ;Retry operation
.errorIgnore:
    ;Clear the busy bit in the status word
    and byte [secdReqHdr + drvReqHdr.status + 1], ~(drvBsyStatus >> 8)
    jmp short .ignoreRet
.notChar:
;Just return with ZF=NZ for now
    push rax
    xor eax, eax
    inc al
    pop rax
    return

openSFT:
;Signals an open to a file (e.g. when printer echo is to begin)
;Input: rdi = SFT pointer
    call dosPushRegs
    mov al, drvOPEN
    jmp short openCloseCommon
closeSFT:
;Signals a close to a file (e.g. when printer echo is to end)
    call dosPushRegs
    mov al, drvCLOSE
openCloseCommon:
;Only signals an open/close to a block device if SHARE is loaded
    test word [rdi + sft.wDeviceInfo], devRedirDev  ;We a network device?
    jz .exit    ;Exit if so
    test byte [rdi + sft.wDeviceInfo], devCharDev
    mov rdi, qword [rdi + sft.qPtr] ;Get DPB or Device Driver header
    jnz .charDev
    ;Here a disk drive, rdi is a DPB ptr
    cmp byte [shareFlag], 00    ;Is SHARE loaded?
    je .exit   ;Exit if it is zero (Share not loaded)
    mov ah, byte [rdi + dpb.bUnitNumber]    ;Get to populate request header
    mov cl, byte [rdi + dpb.bDriveNumber]   ;Get for error if an error occurs
    mov rdi, qword [rdi + dpb.qDriverHeaderPtr]
.charDev:
    test word [rdi + drvHdr.attrib], devDrvHdlCTL   ;Can we open/close?
    jz .exit    ;No, exit!
    mov rsi, rdi    ;Save driver header in rsi for the request
    lea rbx, primReqHdr ;Get the primary request header space in rbx
    movzx eax, ax   ;Zero extend (al = unit num if block, ah = cmdcde)
.retryEP:
    mov dword [rbx + 1], eax    ;Store unitnm (if block), cmdcde and 0 status
    mov byte [rbx], openReqPkt_size ;Same length as closeReqPkt
    push rax    ;Save the dword
    call goDriver
    movzx edi, word [primReqHdr + drvReqHdr.status] ;Get the status
    test edi, drvErrStatus
    jz .exitPop
    ;Error here, check if char or block drive
    test word [rsi + drvHdr.attrib], devDrvChar
    jz .blockError
    ;Char error here
    mov ah, critCharDev | critData  ;Data on char dev error
    jmp short .errorCmn
.blockError:
    mov al, cl  ;Move drive number into al
    mov ah, critData    ;Data on block dev error
.errorCmn:
;Permit only Abort, Retry or Ignore. Abort doesn't come through.
    call charDevErr ;Call temperror handler (handler due to change, not ep)
    mov al, critRetry   ;al returns user response
    jne .exitPop    ;Ignore, proceed as if nothing happened
    pop rax ;Get back zero extended eax into eax to store
    jmp short .retryEP  ;Reset
.exitPop:
    pop rax    
.exit:
    call dosPopRegs
    return