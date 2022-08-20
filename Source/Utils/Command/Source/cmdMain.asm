;Initial Portion, jumped past data asection
commandStart:
    ;Resize Allocation, jump here with endpointer in rbx
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8 + 11h]    ;Get # of bytes for COMMAND.COM
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8

    lea rax, qword [r8 + psp.dta]
    mov qword [cmdLinePtr], rax
    mov word [rax], 0080h ;Setup command line to accept up to 128 bytes
.inputMain:
    call .printPrompt
    mov rdx, qword [cmdLinePtr]
    mov eax, 0C0Ah  ;Flush Keyboard and Buffered input
    int 41h
    jmp short .inputMain

.printPrompt:
    ;Currently, fixed prompt X>
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    mov byte [currentDrv], al   ;Save drv number
    add al, "A"
    mov byte [basicPrompt + 2], al  ;Save letter in prompt string
    lea rdx, basicPrompt
    mov ah, 09h ;Print String
    int 41h
    return