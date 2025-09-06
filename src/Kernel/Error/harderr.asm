
diskIOError:
;Called in Binary Disk Read/Write if getting access to shared resource fails
;Input: rwFlag = 0 or 1 for read/write
;       eax = Status word
;       rdi -> buffer pointer
;       rbp -> DPB ptr
    cmp al, drvBadDskChnge
    jne .doReq
    push rax    ;If a bad disk change, drop the volume label ptr here
    mov rax, qword [primReqPkt + ioReqPkt.desptr]   ;Get volume label ptr
    mov qword [errorVolLbl], rax    ;and save it!
    ;Later versions will include a serial number after the lbl too
    pop rax
.doReq:
    call diskDevErr ;Preserves rdi on stack and rbp in tmpDPBPtr
    return
xlatHardError:
;Translates a hard error code to a generic DOS error
;Input: edi = Hard Error Code
;       ah = Bitfield
;       al = Failing drive number
    push rax    ;Wanna preserve ax
    cmp di, hardXlatTblL    ;If errorcode > 15, do not adjust!!
    movzx eax, di  ;Clears 64 bits and moves error code into ax
    jae .skipXlat   ;Skip xlat if above 15, for IOCTL return errors
    push rbx
    lea rbx, hardXlatTbl
    xlatb    ;Get translated byte from the table in al
    pop rbx
.skipXlat:
    mov word [errorExCde], ax   ;Store this error code here
    pop rax
    push rsi
    lea rsi, extErrTbl
    call setErrorVars
    pop rsi
    return

charDevErr:
;Hard character device errors come here
;Input:
; ah = Additional Int 24h flags. Top bit should be set!
;edi = error code in low byte
;rbp -> Not accessed but preserved
    or ah, critIgnorOK | critRetryOK | critFailOK   ;Set the always bits
    mov byte [Int24bitfld], ah
    mov qword [tmpDPBPtr], rbp
    push rsi
    movzx edi, dil    ;Zero extend the error code up
    call hardErrorCommon
    pop rsi
    return
diskDevErr:
;Called, NOT Jumped to. 
;Input: rdi = Disk Buffer pointer (or 0 to mean share)
;       eax = Status word (error code in al)
;       rbp = Disk DPB pointer
; [Int24hbitfld] = Specific bitflags (r/w AND potential extra ok responses)
;Output: al = Int 24h response (0-3)
; All other registers preserved
    mov bl, dataBuffer  ;Set dflt flags for invoke
    test rdi, rdi       ;Is this a share invokation?
    je .skipbufferread  ;Jump if so, since share lock issues occur on data io
    mov bl, byte [rdi + bufferHdr.bufferFlags]  ;Else get the buffer data type
.skipbufferread:
    push rdi        ;Save the disk buffer pointer
    movzx edi, al   ;Store status code in dil, zero extend
    cmp edi, drvWPErr
    jne .notReset
    ;Reset the error drive to report dpb drive if a write protect error!
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov byte [errorDrv], al ;Store this value
.notReset:
    mov ah, byte [Int24bitfld]  ;Get the permissions in var
    or ah, critFailOK | critRetryOK ;Set the always bits
    ;Test for correct buffer data type
    test bl, dosBuffer
    jnz .df0
    or ah, critDOS  ;Add DOS data type bit
    jmp short .df3
.df0:
    test bl, fatBuffer
    jnz .df1
    or ah, critFAT  ;Add FAT data type bit
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;Invalidate the count!
    jmp short .df3
.df1:
    test bl, dirBuffer
    jnz .df2
    or ah, critDir  ;Add Directory data type bit
    jmp short .df3
.df2:
    or ah, critData ;Here it must be a data buffer
.df3:
    and byte [rwFlag], 1    ;Save only the bottom bit
    or ah, byte [rwFlag]    ;And set the low bit here
    or ah, byte [Int24bitfld]
    ;Continue down with failing disk buffer pointer on stack
    call diskDevErrBitfield
    pop rdi ;Pop back the disk buffer pointer
    return   
diskDevErrBitfield:
;Called with Int24Bitfield constructed and in ah and error code in dil
;This is to avoid rebuilding the bitfield.
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get the drive number
    mov qword [tmpDPBPtr], rbp  ;Save the DPB 
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;And get the driver ptr in rsi
    xor ebp, ebp    ;Finally, set ebp to 0 to simulate the segment
hardErrorCommon:
;The common fields, with the vars set up. 
;Ensure we dont have a crazy error code.
;Entered with: ah = bitfield, al = Fail drive (0 based) if not char
; dil = Driver error code, rsi -> Driver header
;tmpDBPPtr = Fail DPB if not char, rwFlag set/clear
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
    ;call checkDoInt24OnHandle   ;IF returns ZF=NZ, we just fail!
    ;jnz .setFail
    cli                         
    inc byte [critErrFlag]      ;Set flag for critical error
    dec byte [inDOS]            ;Exiting DOS
    mov rsp, qword [oldRSP]     ;Get the stack ptr after regs were pushed
    xor ebp, ebp                ;Always zeroed for DOS portability!
    int 24h                     ;Call crit. err. hdlr. Ints reset on
    mov qword [oldRSP], rsp     ;Allows user to change reg vals on fail!
    mov rsp, qword [xInt24hRSP] ;Ret to DOS stack for failing device
    mov byte [critErrFlag], 0   ;Clear critical error flag
    inc byte [inDOS]            ;Reenter DOS
    mov rbp, qword [tmpDPBPtr]
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
    ;call checkDoInt24OnHandle   ;If we fail because of handle, skip fail counter!
    ;jnz .skipFailInc
    inc byte [Int24Fail]        ;Inc the fail counter!
.skipFailInc:
    test byte [Int24bitfld], critFailOK
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
    test byte [Int24bitfld], critIgnorOK
    jnz .exit
    jmp short .setFail  ;If ignore not permitted, return Fail
.checkRetry:
    test byte [Int24bitfld], critRetryOK
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
    ;Before returning, we need to set the aborting psp.rspPtr back to 
    ; the oldRSP as a syscall during Int 24h would change this value.
    ;This only affects programs which are their own parents as when aborting
    ; we swap to the parentPSP. This prevents a bug from arising as the 
    ; stack ptr in psp.rspPtr may have changed since initially entering DOS
    ; as the Int 24h handler may have made an Int 21h call, meaning if 
    ; the Int 24h handler plays with the stack too much, the value in 
    ; psp.rspPtr is no longer pointing at a "valid" stack frame (i.e. 
    ; with valid SS:RSP). The only sane thing to do is to reset this 
    ; pointer to the value it had on entry to the initial DOS call which
    ; triggered the Int 24h (or the equivalent stack frame that was 
    ; replaced by the Int 24h handler). If the task being aborted is not 
    ; its own parent the following is a NOP. If it is its own parent, we
    ; the following prevents a GP. Fault.
    mov rdi, qword [currentPSP]
    mov rbx, qword [oldRSP]
    mov qword [rdi + psp.rspPtr], rbx
    jmp terminateClean.altEP

;checkDoInt24OnHandle:
;Checks if currentSFT is a null pointer. Return ZF=ZE if so.
;Else, take the SFT pointer and check its open mode. 
;   If openFailOnI24 set, return ZF=NZ
;   Else, return ZF=ZE.
;    push rdi
;    call getCurrentSFT
;    test rdi, rdi   ;If this is a null pointer, no
;    jz .exit
;    test word [rdi + sft.wOpenMode], openFailOnI24
;.exit:
;    pop rdi
;    return