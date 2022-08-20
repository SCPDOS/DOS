commandStart:
    ;Resize Allocation, jump here with endpointer in rbx
    ;Ideally would have this jettisoned too but cannot guarantee
    ; that the jump to safety won't be gobbled up when multitasking
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [rbx + r8 + 11h]    ;Get # of bytes for COMMAND.COM
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8
    int 41h
    jmp short commandMain
applicationReturn:  ;Return point from a task
    mov eax, 4D00h ;Get Return Code
    int 41h
    mov word [returnCode], ax
;Reset our PSP vectors (and IVT copies) in the event they got mangled
    lea rdx, critErrorHandler
    mov qword [r8 + psp.oldInt44h], rdx
    mov eax, 2544h
    int 41h
    lea rdx, int43h
    mov qword [r8 + psp.oldInt43h], rdx
    mov eax, 2543h
    int 41h
    lea rdx, applicationReturn
    mov qword [r8 + psp.oldInt42h], rdx
    mov eax, 2542h
    int 41h
;Close all handles from 5->MAX
    movzx ecx, word [numHdls]
    mov ebx, 5
.handleClose:
    mov ah, 3Eh ;File close
    int 41h
    inc ebx ;Goto next file
    cmp ebx, ecx
    jbe .handleClose    ;Keep looping whilst below or equal

commandMain:
;Setup Commandline
    lea rax, qword [r8 + psp.dta]
    mov qword [cmdLinePtr], rax
    mov word [rax], 007Eh ;Setup command line to accept up to 126 bytes
.inputMain:
    call printPrompt
    mov rdx, qword [cmdLinePtr]
    mov eax, 0C0Ah  ;Flush Keyboard and Buffered input
    int 41h
    jmp short .inputMain

printPrompt:
    ;Currently, fixed prompt X>
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    mov byte [currentDrv], al   ;Save drv number
    add al, "A"
    mov byte [basicPrompt], al  ;Save letter in prompt string
    lea rdx, crlf
    mov ah, 09h ;Print a new line
    int 41h
    lea rdx, basicPrompt
    mov ah, 09h ;Print prompt
    int 41h
    return

int4Eh:   ;Otherwise known as Int 4Eh
    pop rax ;Pop RIP
    pop rbx ;Pop CS selector
    pop rbx ;Pop flags
    push rax    ;Push RIP back onto stack again so we can return normally
parseCommandLine:    ;And this is how we enter it normally
;rsi must be pointing to the count byte (byte 1) of the 41h/0Ah string
; and the string must be CR (0Dh) terminated (not accounted for in the count)
    movzx ecx, byte [rsi]
    cmp byte [rsi + rcx + 1], CR
    retne
    return