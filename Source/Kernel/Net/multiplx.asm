;Default multiplexer. Current only installed function is ah=12h
;If a ah=11h request is made, we return CF=CY (as intended)
;If an argument needs to be in al for any function, it must be pushed on
; stack before the interrupt is called. This word is always read from the
; stack, but the stack is not rejiggled to remove it.

multiplexHdlr:          ;Int 4Fh, AH=12h, exposed internal functions
    cmp ah, 12h
    jne .exitBad       ;Exit if not for us
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
    mov rax, qword [rsp + 8*8]  ;Get qword that may be passed on stack
    return

.retAddr:
    push rbx
    pushfq
    pop rbx
    mov qword [rsp + 3*8], rbx
    pop rbx
    iretq

.exitBad:
    mov eax, errInvFnc
    mov byte [errorLocus], eLocUnk
    or byte [rsp + 8h*2], 1 ;Set CF
    iretq
multiplexTest:
    mov al, -1
    ret
getDosDataSeg:  ;Int 4Fh, AX=1203h
;Return: r8 = Dos Data Segment Pointer
    lea r8, dosDataArea
    return

