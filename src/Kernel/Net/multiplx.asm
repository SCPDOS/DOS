;Default multiplexer and install check for other DOS multiplex functions.
;Mainly to handle the DOS multiplexer, i.e. ah=12h. Three other 
; cases also handled as outlined below.
;------------------------------------------------------------------------
;Handle ah = 10h,11h,14h install check requests only (al=0).
; If al <> 0, return error, set CF and al = 1.
; For reference:
; ah = 10h, (SHARE)
;           Return: al = 0, not installed, ok to install
;                   al = 1, not installed, not ok to install
;                   al = -1, installed.
; ah = 11h, (REDIR)
; ah = 14h, (NLS)
;Any other value of ah simply falls through the IRETQ
;------------------------------------------------------------------------
;If an argument needs to be in al for any function, it must be pushed on
; stack before the interrupt is called. This word is always read from the
; stack, but the stack is not rejiggled to remove it.

multiplexHdlr:          ;Int 4Fh, AH=12h, exposed internal functions
    cmp ah, 10h ;Are we share?
    je .installCheck
    cmp ah, 11h ;Are we redir?
    je .installCheck
    cmp ah, 12h
    je .goMplx
    cmp ah, 14h ;Are we NLS?
    je .installCheck
    iretq

.installCheck:
;Here if a share, redir or NLS request.
;All of these should be installed over us if installed.
;Thus we are not installed. Return not installed, if that is what the 
;request wants. (AL=0)
;If anything else requested, return error.
    test al, al
    jz .exit
.exitBad:
    mov eax, errInvFnc
    mov byte [errorLocus], eLocUnk
    or byte [rsp + 8h*2], 1 ;Set CF
.exit:
    iretq

.goMplx:
    cmp al, mDispTblL / 2
    jae .exitBad   ;If above or equal, exit
    ;Rejiggle stack! 
    push rbx    ;Storage for return from DOS ret addr
    push rbx    ;Storage for DOS function
    push rbx    ;Stores rbx value
    lea rbx, .retAddr
    mov qword [rsp + 2*8], rbx    ;Store ret addr from DOS routine

    push rcx
    lea rbx, qword mDispTbl   ;Get mplx displacement tbl
    push rbx
    movzx ecx, al   ;Get the subfunction number into ecx
    mov rax, qword [rsp + 10*8]  ;Pick the word pushed on the stack before call 
    shl ecx, 1   ;multiply by 2
    inc rbx         ;Go past the first byte (length count)
    movzx ebx, word [rbx + rcx] ;Get the word at this address
    pop rcx ;Get the EA of the displacement table in rcx
    add rbx, rcx
    pop rcx
    mov qword [rsp + 8], rbx
    pop rbx
    mov rax, qword [rsp + 7*8]  ;Get qword that may be passed on stack
    return

.retAddr:
    push rbx
    pushfq
    pop rbx
    mov qword [rsp + 3*8], rbx
    pop rbx
    iretq

multiplexTest:
    mov al, -1
    ret

getDosDataSeg:  ;Int 4Fh, AX=1203h
;Return: r8 = Dos Data Segment Pointer
    lea r8, dosDataArea
    return
