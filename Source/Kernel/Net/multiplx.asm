;Default multiplexer. Current only installed function is ah=12h

multiplexHdlr:          ;Int 4Fh, AH=12h, exposed internal functions
    cmp ah, 12h
    jne .exit       ;Exit if not for us
    cmp al, mDispTblL / 2
    jae .exit   ;If above or equal, exit
    push rbx
    push rcx
    xor ecx, ecx
    lea rbx, mDispTbl   ;Get multiplex displacement table
    mov cl, al   ;Get the subfunction number into ecx
    shl ecx, 1   ;multiply by 2
    add rbx, rcx    ;rbx now points to function to call
    call rbx
    pop rcx
    pop rbx
    iretq
.exit:
    or byte [rsp + 8h*2], 1
    iretq
multiplexTest:
    mov al, -1
    ret
getDosDataSeg:  ;Int 4Fh, AX=1203h
;Return: r8 = Dos Data Segment Pointer
    lea r8, dosDataArea
    return