;Kernel functions to be used by SHARE on SHARE'd files.

;Generic Share Hooks and Wrappers
shareBadFunction:
    mov eax, errInvFnc
    mov word [errorExCde], ax
    stc
    return
badDfltShareHook:
;Return CF=CY
    call shareBadFunction
    return
goodDfltShareHook:
;Return CF=NC
    clc
    return
openShareCallWrapper:
    call qword [openShare]
    return
closeShareCallWrapper:
    call qword [closeShare]
    return

retryShareIODelay:
;Checks to see if the region of the file we are IOing with is locked.
;Called only in readDiskFile and writeDiskFile
;Input: rdi -> SFT for current file
;       ecx = Length of region from current position in file
;Output: If CF=NC, not locked
;        If CF=CY if ANY portion of the region is locked and EAX=errLokVio
    movzx ebx, word [shareCount]
.mainLoop:      
    push rbx
    call qword [checkFileLockShare]
    pop rbx
    retnc   ;Return if CF=NC (i.e. region not locked [anymore])
    call shareRetryCountdown
    dec ebx
    jnz .mainLoop
    stc
    return

shareRetryCountdown:
;Used in: OpenMain, retryShareIODelay, sharingLoop
;Does the retry wait IFF the request was not a server request.
    test byte [dosInvoke], -1   ;Server call?
    retnz   ;Exit by default if it is a server call
    push rcx
    movzx ecx, word [shareDelay]    ;This many multiples of counts to 65536 
    jecxz .exit
.loopBody:
    push rcx
    xor ecx, ecx
.mainLoop:
    dec cx
    jnz .mainLoop
    pop rcx
    dec ecx
    jnz .loopBody
.exit:
    pop rcx
    return

shareCheckOpenViolation:
;Input: rdi -> Locally complete SFT we are opening
    test word [rdi + sft.wOpenMode], openSFTFCB
    jnz .fcbQuirk   ;FCB?? opened files immediately will cause an error here
    push rax
    movzx eax, word [rdi + sft.wOpenMode]
    and eax, 0F0h   ;Save second nybble only (sharing modes)
    pop rax
    jnz .notInCompatMode    ;Jump if not zero only!
.fcbQuirk:
    call shareLockViolationCriticalError
    retnc
.notInCompatMode:
    mov eax, errShrVio
    stc ;Set the flag for error
    return



shareFile:
;Once the SFT has been made, here we allocate resources within share to
; share the file, if it is possible to do so!
; This is done by calling the open wrapper.
;Preserves ecx
;Output: rdi -> Current SFT!!
    push rcx
.reloadCounter:
    movzx ecx, word [shareCount] ;Try to allocate resources, this many times
.keepLooping:
    call getCurrentSFT
    xor eax, eax
    mov qword [rdi + sft.pMFT], rax     ;Init to no record
    push rcx
    call openShareCallWrapper
    pop rcx
    jnc .exit   ;If all good, exit! (a record has now been made by SHARE.EXE)
    call shareRetryCountdown
    dec ecx
    jnz .keepLooping
    call shareCriticalError
    jnc .reloadCounter  ;If user selected retry, we retry
.exit:
    pop rcx
    return

shareCheckWriteLockViolation:
    mov byte [rwFlag], 1    ;Called in write, might not be set so set it
    jmp short shareCheckReadLockViolation.common
shareCheckReadLockViolation:
    mov byte [rwFlag], 0    ;Called in read, might not be set so set it
.common:
;Input:
;rdi -> SFT for the file we are reading
    test word [rdi + sft.wOpenMode], openSFTFCB
    jnz .fcbQuirk   ;FCB?? opened files immediately will cause an error here
    push rax
    movzx eax, word [rdi + sft.wOpenMode]
    and eax, 0F0h   ;Save second nybble only (sharing modes)
    pop rax
    jnz .notInCompatMode    ;Jump if not zero only!
.fcbQuirk:
    call shareLockViolationCriticalError
    retnc
.notInCompatMode:
    xor ecx, ecx    ;Number of bytes xferred
    mov eax, errLokVio
    stc ;Set the flag for error
    return

shareLockViolationCriticalError:
;This does NOT force rwFlag to 0 and signals a lock violation
    push rdi
    mov eax, errLokVio
    jmp short shareCriticalError.common
shareCriticalError: ;Int 2Fh AX=120Ah
;Used for share Read requests
;Input: eax = Error code
    push rdi
    mov byte [rwFlag], 0    ;Default to read
.common:
    mov byte [Int24bitfld], critRetryOK | critFailOK
    mov rbp, qword [workingDPB] 
    xor edi, edi   ;Indicate that this was due to share
    call diskDevErr
    pop rdi
    cmp al, critRetry   ;If we returned retry, return plainly, else set CF
    rete
    stc
    return