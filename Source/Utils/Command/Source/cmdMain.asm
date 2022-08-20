;Initial Portion, jumped past data asection
commandStart:
    ;Resize Allocation
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [endOfAlloc + r8 + 11h]    ;Get bytes for CMD.COM
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8
    mov rax, qword [cmdLinePtr]
    mov word [rax], 0080h ;Accept up to 128 bytes
.inputMain:
    call .printPrompt
    mov rdx, qword [cmdLinePtr]
    mov ah, 0Ah  ;Buffered input
    int 41h
    jmp short .inputMain

.printPrompt:
    ;Currently, fixed prompt X>
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    mov byte [currentDrv], al   ;Save drv number
    add al, "A"
    mov byte [basicPrompt], al  ;Save letter in prompt string
    lea rdx, basicPrompt
    mov ah, 09h ;Print String
    int 41h
    return