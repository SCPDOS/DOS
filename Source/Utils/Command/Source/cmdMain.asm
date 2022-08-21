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
    lea rax, cmdLine
    mov word [rax], 007Eh ;Setup command line to accept up to 126 bytes
.inputMain:
    call printCRLF
    call printPrompt

    lea rdx, cmdLine
    mov eax, 0C0Ah  ;Flush Keyboard and Do Buffered input
    int 41h
    call printCRLF  ;Note we have accepted input

    lea rsi, qword [cmdLine + 1]    ;Point at count byte
    call parseCommandLine
    jmp short .inputMain

printPrompt:
    ;Currently, fixed prompt X>
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    mov byte [currentDrv], al   ;Save drv number
    add al, "A"
    mov byte [basicPrompt], al  ;Save letter in prompt string
    lea rdx, basicPrompt
    mov ah, 09h ;Print prompt
    int 41h
    return

printCRLF:
    lea rdx, crlf
    mov ah, 09h ;Print a new line
    int 41h
    return

getDriveLetter:
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    inc al  ;Convert to a 1-based number
    mov byte [currentDrv], al   ;Save drv number
    return

putDateinPrompt:

putVersionInPrompt:
    push rcx
    push rsi
    lea rsi, dosName
    mov ecx, dosNameL
    rep movsb
    pop rsi
    pop rcx
    ;Now we get the version number in ax. Maj = al, Min = ah
    mov ah, 30h
    int 41h
    mov edx, eax
    return
putMoneyInPrompt:
;Input:  rdi points to where to write the | symbol
;Output: rdi points to the next char to replace
    mov al, "$"
    stosb
    return

putCRLFinPrompt:
;Input:  rdi points to where to write the CRLF
;Output: rdi points to the next char to replace
    mov ah, LF
    mov al, CR
    stosw
    return
putEquInPrompt:
;Input:  rdi points to where to write the | symbol
;Output: rdi points to the next char to replace
    mov al, "="
    stosb
    return
putPipeInPrompt:
;Input:  rdi points to where to write the | symbol
;Output: rdi points to the next char to replace
    mov al, "|"
    stosb
    return

putGTinPrompt:
;Input:  rdi points to where to write the > symbol
;Output: rdi points to the next char to replace
    mov al, ">"
    stosb
    return

putLTinPrompt:
;Input:  rdi points to where to write the < symbol
;Output: rdi points to the next char to replace
    mov al, "<"
    stosb
    return

putDriveInPrompt:
;Input:  rdi points to where to write the current drive letter
;Output: rdi points to the next char to replace
    mov al, byte [currentDrv]   ;Get 1 based drive number
    add al, "@"
    stosb   ;Store drive letter
    return

putCWDInPrompt:
;Input:  rdi points to where to write the current working directory
;Output: rdi points to the next char to replace
    push rsi
    mov dl, byte [currentDrv]   ;Get 1 based drive number
    mov al, dl
    mov ah, ":"
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 41h
    xor al, al  ;Scan for the terminating zero
    mov ecx, 66    ;Max path length - 1
    rep scasb   ;Scan for the terminating null
    dec rdi ;Go back a char to point to last char or ascii null
    pop rsi
    return


int4Eh:   ;Otherwise known as Int 4Eh
    pop rax ;Pop RIP
    pop rbx ;Pop CS selector
    pop rbx ;Pop flags
    push rax    ;Push RIP back onto stack again so we can return normally
parseCommandLine:    ;And this is how we enter it normally
;rsi must be pointing to the count byte (byte 1) of the 41h/0Ah string
; and the string must be CR (0Dh) terminated (not accounted for in the count)
    
    return