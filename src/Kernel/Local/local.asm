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
    mov eax, errAccDen
    jmp extErrExit
getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage
;If al = 01h -> Get Global Codepage
;Return:    ebx = Active (current) codepage
;           edx = System (default) codepage
;If al = 02h -> Set Global Codepage
;   (e)bx = Active (current) codepage
;   (e)dx = System (default) codepage. Not needed, so don't document.
    cmp al, 1
    jne .setCodepage
;Here we get the codepage
    call getUserRegs
    movzx ebx, word [extCtryTbl.activeCP]   ;Get the active codepage value
    movzx edx, word [defaultCP] ;Get the default codepage
    mov dword [rsi + callerFrame.rbx], ebx
    mov dword [rsi + callerFrame.rdx], edx
    xor eax, eax
    jmp extGoodExit
.exitBadFunc:
    mov eax, errInvFnc
    jmp extErrExit
.setCodepage:
    cmp al, 2
    jne .exitBadFunc
    movzx edx, word [defltCtry] ;Get the country ID
    mov eax, 1400h
    int 4Fh
    cmp al, -1
    jne .exitBadFunc
    lea rsi, dosCodepage    ;Get pointer to the DOS codepage in rsi
    mov eax, 1401h  ;Set global codepage
    int 4Fh
    test al, al
    jz extGoodExit
    cmp al, errNLSAcDen
    jne extErrExit
    cbw     ;Zero extend al into ax (as we know al = 41h)
    mov word [errorExCde], ax
    mov byte [errorAction], eActIgn
    mov byte [errorLocus], eLocChr
    mov byte [errorClass], eClsHrdFlt
    jmp extErrExit.noXlat   ;Jump to error exit without translating the error


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