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
    lea rbx, mDispTbl   ;Get multiplex displacement table
    push rbx
    movzx ecx, al   ;Get the subfunction number into ecx
    mov rax, qword [rsp + 5*8]  ;Pick the word pushed on the stack before call 
    shl ecx, 1   ;multiply by 2
    movzx ebx, word [rbx + rcx] ;Get the word at this address
    pop rcx ;Get the EA of the displacement table in rcx
    add rbx, rcx
    mov qword [oldRBX], rbx
    pop rcx
    pop rbx
    call qword [oldRBX]
    mov qword [oldRBX], rbx
    pushfq  ;Move flags on the stack
    pop rbx
    mov qword [rsp + 8*2], rbx  ;Replace entry flags with our own flags
    mov rbx, qword [oldRBX]
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

