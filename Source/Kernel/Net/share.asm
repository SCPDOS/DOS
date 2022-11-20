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