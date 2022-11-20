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
;Checks to see if the region of the file we are IOing with is locked
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

shareCriticalError: ;Int 4Fh AX=120Ah
    push rdi
    mov byte [rwFlag], 0    ;Default to read
    mov byte [Int44bitfld], critRetryOK | critFailOK
    mov rbp, qword [workingDPB]
    mov edi, 1
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr]
    call fullcriticalErrorInvoke
    pop rdi
    cmp al, 1   ;If we returned retry, return plainly, else set CF
    rete
    stc
    return