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
    mov ah, 0Ah  ;Do Buffered input
    int 41h
    call printCRLF  ;Note we have accepted input

    lea rsi, qword [cmdLine + 1]    ;Point at count byte
    call doCommandLine
    jmp short .inputMain

printPrompt:
    cmp word [promptPtr], -1
    jne .validPrompt
    ;Here we print the default prompt
    call putCWDInPrompt
    call putGTinPrompt
    return
.validPrompt:
    return


int4Eh:   ;Interrupt interface for parsing and executing command lines
    mov ah, 51h ;Get Current PSP in rdx
    int 41h
    push rdx
    call doCommandLine
    pop rbx ;Get Old current PSP in rbx
    mov ah, 50h ;Set Current PSP
    int 41h
    iretq
doCommandLine:    ;And this is how we enter it normally
;rsi must be pointing to the count byte (byte 1) of the 41h/0Ah string
; and the string must be CR (0Dh) terminated (not accounted for in the count)
    
    return