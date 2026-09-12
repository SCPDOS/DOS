charDevErr:
;Hard character device errors come here
;Input:
; ah = Additional Int 24h flags. Top bit should be set!
;edi = error code in low byte
;rbp -> Not accessed but preserved
    or ah, critIgnorOK | critRetryOK | critFailOK   ;Set the always bits
    mov byte [bI24OkBtfld], ah
    mov qword [qErrRbp], rbp
    push rsi
    movzx edi, dil    ;Zero extend the error code up
    call hardErrorCommon
    pop rsi
    return

diskIOError:
;Called in Binary Disk Read/Write if getting access to shared resource fails
;Input: bRwFlag = 0 or 1 for read/write
;       eax = Status word
;       rbp -> DPB ptr
;       cl = Data type (buffer type, i.e. DOS, FAT, Dir, Data)
; [bI24OkBtfld] = Set with OK responses
; [bRwFlag] = Set or cleared as required.
;Output: al = Action Code
;   Preserves rbx-rbp
    cmp al, drvBadDskChnge
    jne .doReq
    push rax    ;If a bad disk change, drop the volume label ptr here
    mov rax, qword [primReqPkt + ioReqPkt.desptr]   ;Get volume label ptr
    mov qword [errorVolLbl], rax    ;and save it!
    ;Later versions will include a serial number after the lbl too
    pop rax
.doReq:
    call diskDevErr ;Preserves rdi and rbp (in qErrRbp)
    return

diskDevErr:
;Called, NOT Jumped to. 
;Input: eax = Driver status word (Error code in al)
;       ecx = byte: Buffer flag; error on DOS, FAT, DIR or DATA xaction?
;       rbp = Disk DPB pointer
; [bI24OkBtfld] = Set with OK responses
; [bRwFlag] = Set or cleared as required.
;Output: al = Int 24h response (0-3)
; Preserves rbx, rcx, rdx, rsi, rdi, rbp. 
;NOTE: DOS requires bx-si be preserved by i24h for ignore/retry. 
;   We don't trust the user handler.
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    movzx edi, al   ;Store status code in dil, zero extend
    cmp edi, drvWPErr
    jne .notReset
;Reset the error drive to report dpb drive if a write protect error!
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov byte [errorDrv], al ;Store this value
.notReset:
;Now we build the reported bitfield. We build it in al and then shift it up.
;Assertain correct buffer type
    xor eax, eax    ;Set for critDOS (0h -> 0h)
    test ecx, dosBuffer
    jnz .goXlat
    inc eax         ;Set for critFAT (1h -> 200h)
    test ecx, fatBuffer
    jz .getNonFatType
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;Invalidate the count for FAT err!
    jmp short .goXlat
.getNonFatType:
    inc eax         ;Set for critDir (2h -> 400h)
    test ecx, dirBuffer
    jnz .goXlat
    inc eax         ;Set for critData (3h -> 600h)
.goXlat:
;Now al has or 0h, 1h, 2h or 3h
    shl eax, 1  ;Turn into 0h, 2h, 4h or 6h
;Now set the rw bit and the ok response bits
    or al, byte [bI24OkBtfld]       ;Set the OK bits now
    and byte [bRwFlag], 1           ;Save only the bottom bit
    or al, byte [bRwFlag]           ;And get the read/write bit
    shl eax, 8                      ;And put this byte into eax
;Now get the drive we failed on from the dpb 
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get the drive number
    mov qword [qErrRbp], rbp  ;Save the DPB 
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;And get the driver ptr in rsi
    call hardErrorCommon
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    return   
hardErrorCommon:
;The common fields, with the vars set up. 
;Ensure we dont have a crazy error code.
;Entered with: ah = bitfield, al = Fail drive (0 based) if not char
; dil = Driver error code, rsi -> Driver header
;qErrRbp = rbp copied in, bRwFlag set/clear
    call xlatHardError
    push rax
    mov eax, errGF - drvErrShft
    cmp edi, eax    ; If the returned error code is above largest driver code
    cmova edi, eax  ; return the driver largest code
    pop rax
criticalDOSError:   ;Int 2Fh, AX=1206h, Invoke Critical Error Function 
;Will swap stacks and enter int 24h safely and handle passing the right data 
; to the critical error handler.
; Called with rsi set as required by Int 24h (caller decides), ax, di
; and with Int24Bitfield set
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;               XXX= 2 - Terminate the Program  (Abort)XXX
;                  = 3 - Fail the DOS call      (Fail)
; Return response from int 24h in al
; Caller must preserve rsp, rbx, rcx, rdx if they wish to return to DOS
; This function will terminate the program if an abort was requested!
; This function also destroys RBP
    test byte [critErrFlag], -1   ;If not zero, already in error. Auto FAIL
    jnz .setFail
    mov qword [xInt24hRSP], rsp ;Save our critical error stack pointer
    cmp word  [currentNdx], -1  ;If this is -1, we are not opening a file
    je .notOpeningFile
    push rdi
    mov rdi, qword [curHdlPtr]  ;Get the pointer to the current handle entry
    mov byte [rdi], -1          ;Free this handle
    pop rdi
.notOpeningFile:
    test word [wEOFlags], eoFailI24 ;If we should skip I24, return fail!
    jnz .setFail
    cli                         
    inc byte [critErrFlag]      ;Set flag for critical error
    dec byte [inDOS]            ;Exiting DOS
    mov rsp, qword [oldRSP]     ;Get the stack ptr after regs were pushed
    xor ebp, ebp                ;Always zeroed for DOS portability!
    int 24h                     ;Call crit. err. hdlr. Ints reset on
;Now we do self-parent stablity handling. Do it here as interrupts are off
; and rsp is guaranteed to have the right value, thanks to int preserving 
; the stack pointer too. 
;If the process isn't its own parent we ignore it.
;Else, we have a self-parent process. This is undocumented and thus we can
; assume it is one of our DOS kernel extensions or something. 
; If the Int 24h handler didnt access int 21h then this is functionally a nop.
; Else, we modified the stack pointer in the psp and as a result, since we are 
;  our own parent, modified our parent's stack pointer in the psp. 
;  Consequently, our parent's psp stack pointer is no longer are pointing 
;  at a correct stack frame. This WILL lead to a GP fault if attempting to 
;  ABORT a self-parent process. 
;What we do here points the parent's psp stack pointer back to the stack 
; frame that was originally set upon entering the Int 21h call that caused the
; hard error. Since this code is within the main parent process space, which
; won't get deallocated, then the stack frame points safely to a known good 
; stack previously used by the parent process, within a memory block owned by
; the parent process.
;Since this code is not hit once inside Int 24h, it is safe wrt reentrancy 
; issues. Thus, the PSP is reset after all the Int 21h calls the Int 24h 
; handler may have made.
    mov rbp, qword [currentPSP]
    cmp rbp, qword [rbp + psp.parentPtr]    ;Are we our own parent?
    jne .notOwnParent
;Now we ensure that any self-parent applications that have gone through Int 24h
; have their PSP rsp returned to the rsp that they entered Int 21h with.
    mov qword [rbp + psp.rspPtr], rsp
.notOwnParent:
    mov qword [oldRSP], rsp     ;Ensure OG stack is put back in place
    mov rsp, qword [xInt24hRSP] ;Ret to DOS stack for failing device
    mov byte [critErrFlag], 0   ;Clear critical error flag
    inc byte [inDOS]            ;Reenter DOS
    mov rbp, qword [qErrRbp]
    sti                         
    ;Now we check that the response given was allowed, and translate if needed
    cmp al, critIgnore
    je .checkIgnore
    cmp al, critRetry
    je .checkRetry
    cmp al, critFail
    jne .abort   ;Must be abort
.setFail:   ;Here is for fail
    mov al, critFail    ;Reset al to contain fail (even if Int24 responded Fail)
    test word [wEOFlags], eoFailI24   ;If hdl tells us to fail, skip fail ctr!
    jnz .skipFailInc
    inc byte [Int24Fail]        ;Inc the fail counter!
.skipFailInc:
    test byte [bI24OkBtfld], critFailOK
    jz .abort  ;If bit not set, fail not permitted, abort
.exit:
    mov byte [errorDrv], -1 ;Unknown drive (to be set)
    cmp byte [currentNdx], -1   ;Is there a file that needs handling?
    rete    ;Only if this is not equal
    ;In that case, we set the jft entry to its initial value, whatever it was
    push rax
    push rdi
    movzx eax, word [currentNdx]
    mov rdi, qword [curHdlPtr]
    mov byte [rdi], al
    pop rdi
    pop rax
    return
.checkIgnore:
    test byte [bI24OkBtfld], critIgnorOK
    jnz .exit
    jmp short .setFail  ;If ignore not permitted, return Fail
.checkRetry:
    test byte [bI24OkBtfld], critRetryOK
    jnz .exit   
    jmp short .setFail  ;If retry not permitted, return Fail
.abort:
;Prepare to abort. We abort from within!
    call vConRetDriver  ;Always reset the driver flag on abort
;If a network request requests abort, translate to fail
    cmp byte [dosInvoke], -1
    je .exit
;If already terminating, dont start terminating again!
    test byte [procExiting], -1
    jnz .exit
    xor eax, eax    ;Default return code to 0. Abort flag will be set later
    mov byte [exitType], 2      ;We are returning from Abort, ret type 2!
    mov byte [volIdFlag], al     ;Clear special vol search byte if set
    jmp terminateClean.altEP
