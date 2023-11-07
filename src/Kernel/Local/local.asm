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
;---------------------------------------------------------
;Common input registers
;al = Country code, 0 means current country info.
;bx = Country code if al = -1.
;Get country info:
;   rdx -> Buffer for the country table (not extended table)
;Set country info:
;   rdx = -1
;---------------------------------------------------------
;Return:
;Get country info:
;   CF=CY -> Error, ax = Error code
;   CF=NC -> OK, ax=bx=Country code, buffer @ rdx filled
;Set country info:
;   CF=CY -> Error, ax = Error code
;   CF=NC -> OK
;---------------------------------------------------------
    mov rdi, rdx    ;Move the pointer/set indicator to rdi
    movzx edx, al   ;Move potential partial country code, zx to edx
    cmp al, -1      ;Does bx have the country code?
    cmovne ebx, edx ;If not, move it there
    xor ebp, ebp    ;Get country table
    mov ecx, 1      ;Set country table
    cmp rdi, -1     ;Set or Get?
    cmove ebp, ecx  ;If rdi = -1, it's set, so init ebp to 1
    je .goToNlsFunc ;If set, immediately goto nls func
    test ebx, ebx   ;Else, in get, are we looking for current country?
    jnz .goToNlsFunc    ;If not zero, goto nls func
    ;Get current country info here, works w/o NLSFUNC
.copyCountryTable:
    lea rsi, ctryTbl
    mov ecx, countryStruc_size
    rep movsb
    movzx ebx, word [extCtryTbl.countryCode]    ;Get current country code
.exitWithCountryCode:
    call getUserRegs
    mov word [rsi + callerFrame.rbx], bx    ;And store it in users bx
.exitNoCountryCode: 
    mov eax, ebx    ;Move country code into eax for return (undocumented)
    jmp extGoodExit
.goToNlsFunc:
    call .nlsWrap   ;Access the NLS functionality
    jc extErrExit   ;If CF=CY, exit error (error code in al)
    test ebp, ebp   ;If set, exit, else get, we may need to copy data
    jnz .exitNoCountryCode 
    test ebx, ebx   ;Has NLSfunc copied the table into the user buffer?
    jnz .copyCountryTable   ;If not, copy the table into user buffer
    mov ebx, edx    ;Move the country code into ebx
    jmp short .exitWithCountryCode
.nlsWrap:
;Subroutine to wrap NLS functions. Should do nothing if we are looking
; for the current country (to avoid hitting NLSFUNC and erroring if
; not installed, for compatibility with DOS 2).
;Input: ebp = 0 -> Get country info
;           = 1 -> Set code page
;        bx = country code
;Output: CF = CY -> Error, al has error code (-1 is generic error)
;        CF = NC -> OK
;It appears in the case of 1404h, bx is a flag to indicate if we 
; need to copy the data from our internal table or not to the users'
; buffer. If bx = 0, no copy. It is assumed that NFS func did the copy for 
; us, leaving our internal table intact (unchanged).
; Else, we copy from our internal table.
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
; CONFIRMED IN DOS 3.3: NFSFUNC copies the data into the users buffer
;!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!
;Final check to make, is to see if the bx = 0 set below is an indicator
; to NLSFUNC to do the copying for us

    cmp bx, word [extCtryTbl.countryCode]   ;Do nothing if its current country
    rete
    mov edx, ebx    ;Save the country code in edx
    xor ebx, ebx    ;Guarantee ebx is not 0EDCh (RBIL)
    mov eax, 1400h  ;Is NLS installed?
    int 4fh
    cmp al, -1      ;If al <> -1, error exit
    jne .errNotInstalled
    lea rsi, dosCodepage    ;Point rsi to the DOS codepage area
    mov eax, 1404h  ;Get Country Info, for country code in dx (bx <> codepage)
    mov ecx, 1403h  ;Set codepage, for country code in dx (bx <> codepage)
    test ebp, ebp   ;What can I do you for amigo?
    cmovnz eax, ecx ;Set codepage if this is non-zero
    int 4fh
    test al, al ;If al = 0, all ok and return CF=NC!
    retz  
.exitErr:   ;Else return with the retuned error code
    stc     ;and CF set
    return
.errNotInstalled:
    mov al, errInvFnc  ;Set invalid function signature
    jmp short .exitErr


getExtLocalInfo:    ;ah = 65h, Get Extended Country Info
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