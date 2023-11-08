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
    je .goToNlsFuncSet ;If set, immediately goto nls func
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
.goToNlsFuncSet:
    mov ebp, ecx    ;Also set ebp to 1
.goToNlsFunc:
    call .nlsWrap   ;Access NLS functionality
    jc extErrExit   ;If CF=CY, exit error (error code in al)
    test ebp, ebp   ;If set, exit, else get, we may need to copy data
    jnz .exitNoCountryCode 
    test ebx, ebx   ;If ebx = 0, we entered NLS and data copied for us.
    jnz .copyCountryTable   ; If not, we gotta copy from our internal copy.
    mov ebx, edx    ;Move the country code into ebx
    jmp short .exitWithCountryCode
.nlsWrap:
;Subroutine to wrap NLS functions. Should do nothing if we are looking
; for the current country (to avoid hitting NLSFUNC and erroring if
; not installed).
;Input: ebp = 0 -> Get country info
;           = 1 -> Set DOS country info
;        bx = Country code (bx <> 0 here)
;       rdi -> User buffer
;       rsi -> DOS internal NLS structure
;Output: CF = CY -> Error, al has error code (-1 is generic error)
;        CF = NC -> OK
;        bx = 0 => Entered NLS and data copied to user buffer.
;           > 0 => User requested current country code. Data NOT copied.
;All other registers remain the unaffected.
    cmp bx, word [extCtryTbl.countryCode]   ;No NLS access if current ctry.
    rete
    mov edx, ebx    ;Save the country code in edx
    xor ebx, ebx    ;Set indicator that we are accessing NLS.
    mov eax, 1400h  ;Is NLS installed?
    int 4fh
    cmp al, -1      ;If al <> -1, error exit
    jne .errNotInstalled
    lea rsi, dosNLSPtr    ;Point rsi to the DOS codepage area
    mov eax, 1404h  ;Get Country Info, for country code in dx
    mov ecx, 1403h  ;Set DOS Country Info, to country code in dx
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
;al = info ID
;   01h get general internationalization info
;   02h get pointer to uppercase table
;   04h get pointer to filename uppercase table
;   05h get pointer to filename terminator table
;   06h get pointer to collating sequence table
;bx = code page (FFFFh=global code page)
;dx = country ID (FFFFh=current country)
;rdi -> country information buffer
;ecx = size of buffer (>= 9), fail if less! NOTE THE CHANGE FROM 5 to 9!!!!
;Return:
;CF=CY if error
;ax = error code
;CF=NC if successful
;ecx = size of country information returned
;rdi -> country information filled in
;Undocumented: ax = default Codepage if nls or requested codepage if internal
    cmp ecx, 9  ;Is our buffer of minimum acceptable size?
    jb .invFuncExit
    lea rsi, dosNLSPtr
    cmp dx, -1
    cmove dx, word [extCtryTbl.countryCode] ;Get the current country code
    cmp bx, -1
    cmove bx, word [extCtryTbl.activeCP]    ;Get the current codepage
    cmp dx, word [extCtryTbl.countryCode]
    jne .nlsReq
    cmp bx, word [extCtryTbl.activeCP]
    jne .nlsReq
    ;We access our local DOS nationalisation tables to 
    ; avoid hitting NLS.
    lea rsi, charTableArray
    movzx ebp, word [ctryFunctions]    ;Get max number of tables to parse
.loopTableSearch:
    cmp al, byte [rsi]  ;Is the table ours?
    jne .tblFound
    add rsi, 9          ;Go to next table (skip one byte and one qword)
    dec ebp
    jnz .loopTableSearch
    ;Fallthrough to error if no match
.invFuncExit:
    mov eax, 1
    jmp extErrExit
.tblFound:
    movsb   ;Copy over the first byte, moving both pointers by 1
    cmp al, 1
    je .getExtCtryTbl
    ;Here we simply copy over a qword
    mov ecx, 8  ;Copy 8 more bytes
    mov eax, 9  ;Number of bytes to save as having had been written
.copyTable:
;Come here with eax = full copy len, ebx = codepage value, ecx = bytes to xfr
    rep movsb   ;Copy the rest of the bytes
    call getUserRegs
    mov dword [rsi + callerFrame.rcx], eax    ;Store in ecx # of bytes
    ;Undocumented, if success, ax contains the requested codepage
    ; value. This is undocumented and should not be relied upon.
    mov eax, ebx    
    jmp extGoodExit
.getExtCtryTbl:
;ecx has length of caller buffer
    sub ecx, 3  ;Remove the byte from the count and the length we will store
    xor eax, eax    ;Clear eax
    lodsw           ;Get word at rsi (full table len) and advance rsi by 2
    cmp eax, ecx    ;Do we have more bytes in buffer than we need?
    cmova ecx, eax  ;Set it to exactly the table length in that case
    mov eax, ecx    ;Copy the number of bytes back to eax too
    stosw           ;Store number of bytes we will copy and adv rdi by 2
    lea eax, dword [ecx + 3] ;Add 3 to eax, for first 3 bytes copied
    jmp short .copyTable
    
.nlsReq:    ;nlsReq moved here to reach the short jumps :)
;As before, now rsi -> DOSNLS structure
; and bpl has the function code (1,2,4,5,6)
    movzx ebp, al   ;Place the function code in ebp (low byte, zx the rest)
    mov eax, 1400h  ;Install check!
    int 4fh
    cmp al, -1          ;If not installed, error exit
    jne .invFuncExit    
    mov eax, 1402h      ;Get codepage info
    int 4fh
    test al, al         ;If the return code is 0, we are ok, else
    jne extErrExit      ; al has error code
    ;Undocumented, if success, ax contains the default codepage
    ; value. This is undocumented and should not be relied upon.
    ;This value never changes, not even by NLSFUNC.
    movzx eax, word [defaultCP]
    jmp extGoodExit

getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage
;If al = 01h -> Get Global Codepage
;Return:    ebx = Active (current) codepage
;           edx = System (default) codepage
;If al = 02h -> Set Global Codepage
;   bx = Active (current) codepage
;   dx = System (default) codepage. Not needed, so don't document.
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
    lea rsi, dosNLSPtr  ;Get pointer to the DOS codepage in rsi
    mov eax, 1401h      ;Set global codepage
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