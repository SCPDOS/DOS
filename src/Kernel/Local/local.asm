;Localisation based functions live here

getsetSwitchChar:  ;ah = 37h, allows changing default switch from / to anything
;al = 0 => Get the switch char
;al = 1 => Set the switch char to whats in DL
;COMPATIBILITY FUNCTIONS BELOW. C.F. Undocumented DOS Sec. Ed. p.686
;al = 2 => Get the status of the requirement for /DEV/ prefix on char devices
;       Always returns -1 => /DEV/ optional
;al = 3 => Set the requirement for /DEV/ prefix on char devices
;       Input value is ignored
    cmp al, 01
    je .setSC
    jb .getSC
    cmp al, 03
    jb .getDev
    je .setDev
    mov al, -1
    return
.setSC:
    mov byte [switchChar], dl
    return
.getSC:
    mov dl, byte [switchChar]   ;Get switchchar
    jmp short .getRet
.getDev:
    mov dl, -1
.getRet:
    call getUserRegs
    mov byte [rsi + callerFrame.rdx], dl
.setDev:    ;Don't set anything, just return immediately. No fanfare.
    return


getsetCountryInfo: ;ah = 38h, localisation info
;Currently only accept subfunction al = 0, current country
;AL > 0 => errInvFnc, Subfunction error
;rdx = Ptr to buffer. If -1 => Set Country information. Also error for now.
    test al, al
    jz .currentCountry
.invalidFunction:
    mov eax, errInvFnc
    jmp extErrExit
.currentCountry:
    cmp rdx, -1
    je .invalidFunction
    lea rsi, ctryTbl
    mov rdi, rdx
    mov ecx, countryStruc_size
    rep movsb
    xor eax, eax
    jmp extGoodExit


getExtLocalInfo:   ;ah = 65h, Get Extended Country Info
getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage
    return

caseMapFunc:
;Input: AL=Char to convert to uppercase (above 80h)
    cmp al, 80h
    retb
    push rbx
    lea rbx, ucTbl
    sub al, 80h ;Turn into an offset into the table
    xlatb
    pop rbx
    return