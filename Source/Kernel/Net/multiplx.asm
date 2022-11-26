;Default multiplexer. Current only installed function is ah=12h
;If a ah=11h request is made, we return CF=CY (as intended)
;If an argument needs to be in al for any function, it must be pushed on
; stack before the interrupt is called. This word is always read from the
; stack, but the stack is not rejiggled to remove it

multiplexHdlr:          ;Int 4Fh, AH=12h, exposed internal functions
    cmp ah, 12h
    jne .exitBad       ;Exit if not for us
    cmp al, mDispTblL / 2
    jae .exitBad   ;If above or equal, exit
    push rbx
    push rcx
    xor ecx, ecx
    lea rbx, mDispTbl   ;Get multiplex displacement table
    mov cl, al   ;Get the subfunction number into ecx
    mov rax, qword [rsp + 5*8]  ;Pick the word pushed on the stack before call 
    shl ecx, 1   ;multiply by 2
    add rbx, rcx    ;rbx now points to function to call
    call rbx
    pop rcx
    pop rbx
    iretq
.exitBad:
    or byte [rsp + 8h*2], 1
    iretq
multiplexTest:
    mov al, -1
    ret
getDosDataSeg:  ;Int 4Fh, AX=1203h
;Return: r8 = Dos Data Segment Pointer
    lea r8, dosDataArea
    return

