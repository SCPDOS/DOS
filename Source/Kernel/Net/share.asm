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