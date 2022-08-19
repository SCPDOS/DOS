;Initial Portion, jumped past data asection
commandStart:
    mov word [cmdLine], 0080h ;Accept up to 128 bytes
.inputMain:
    call .printPrompt
    lea rdx, cmdLine
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