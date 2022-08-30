dir:
    ;If a particular path is given, we search for it.
    ;Else, if rsi is pointing at CR, . or " " we search in CWD for *.*
    cmp byte [rsi], CR
    je .searchCWD

.searchCWD:
    return
chdir:
    test byte [arg1Flg], -1
    jnz .changeDir
    ;Print CWD
.printCWD:
    call putCWDInPrompt ;Exactly the same procedure
    call printCRLF
    return
.printDiskCWD:
;Print CWD for a specified drive
    mov dl, byte [r8 + fcb1 + fcb.driveNum] ;Get 1 based drive number in dl
    mov al, dl
    add al, "@" ;Get the UC letter
    mov ah, ":"
    lea rdi, searchSpec
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 41h
    call strlen
    add ecx, 2 ;Add two for the X:
    mov ah, 40h ;Write to handle
    mov ebx, 1  ;STDOUT
    lea rdx, searchSpec
    int 41h
    call printCRLF
    return
.changeDir:
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je .badDrv  ;IF the drive is good, but FCB name blank, either X: or \ 
    cmp byte [r8 + fcb1 + fcb.filename], " "
    jne .getFQPath
    ;Now we double check that on the command line we have . or ..
    movzx eax, byte [arg1Off]
    lea rsi, cmdBuffer
    add rsi, rax
    mov al, byte [pathSep]
    cmp byte [rsi], al  ;Is the first char a pathsep?
    je .getFQPath
    cmp byte [rsi], "."
    jne .printDiskCWD
    ;If the path is . or .., its acceptable, else fail
.getFQPath:
    call buildCommandPath   ;Else build a fully qualified pathname
    jc .badDir  ;If this returns CF=CY, its a badDir
    lea rdx, searchSpec
    mov ah, 3Bh ;CHDIR
    int 41h
    jc .badDir
    return

.badDrv:
    lea rdx, badDrv
    mov eax, 0900h
    int 41h
    return
.badDir:
    lea rdx, badDir
    mov eax, 0900h
    int 41h
    return


mkdir:
    test byte [arg1Flg], -1
    jz .badParams
    test byte [arg2Flg], -1
    jnz .badParams
    ;We have exactly one argument
    mov al, byte [arg1FCBret]
    cmp al, -1 
    je .badDrv  ;If a drive was specified and was bad, jump
    call buildCommandPath
    jc .badMake
    lea rdx, searchSpec
    mov eax, 3900h  ;MKDIR
    int 41h
    jc .badMake   ;Return if not carry
    mov ah, 0Dh
    int 41h ;Flush to disk
    return
.badMake:   ;Else, bad make
    lea rdx, badMD
    mov eax, 0900h
    int 41h
    return
.badDrv:
    lea rdx, badDrv
    mov eax, 0900h
    int 41h
    return
.badParams:
    lea rdx, badArgs
    mov eax, 0900h
    int 41h
    return

rmdir:
erase:
date:
time:
copy:
ctty:
    return
cls:  
    mov eax, 4400h  ;Get device info
    mov ebx, 1      ;for handle 1
    int 41h         ;in dx
    test edx, devCharDev
    jz .doAnsi  ;Make files register an ansi cls sequence
    test edx, charDevFastOut
    jz .doAnsi
    ;Test if Int 49h uses Int 30h
    ;Tests if within the first 1024 bytes we have the sequence Int 30h (30CD)
    ;Int 49h MUST be terminated with a IRETQ, within 1024 bytes
    mov eax, 3549h  ;Get the vector for interrupt 49h
    int 41h
.biosCheck:
    cmp word [rbx], 30CDh
    je .biosConfirmed
    cmp word [rbx], 0CF48h   ;CFh = IRET, 48h=REX.W
    je .doAnsi
    inc rbx
    jmp short .biosCheck
.biosConfirmed:
    ;Supports a SCP/BIOS compatible routine, use BIOS   
    mov ah, 0Bh  ; Set overscan to black (when Graphics becomes supported)
    xor ebx, ebx
    int 30h
    mov ah, 0Fh ;Get screen mode
    int 30h
    movzx edx, ah   ;Get number of columns in dl
    dec dl
    mov dh, 25  ;Number of rows is standard
    xor eax, eax
    mov ecx, eax
    mov bh, 7   ;Screen attributes
    mov ah, 6   ;Scroll
    int 30h
    xor edx, edx    ;Set cursor coordinates to top left of screen
    mov bh, 0   ;Page 0
    mov ah, 2
    int 30h
    return
.doAnsi:
;If an ANSI driver is not installed, this will simply insert blank lines
;4 chars in the ansi routine
;Will just put the ANSI escape sequence on the screen if it doesn't 
; understand ANSI codes
    lea rsi, ansiCls
    mov ecx, 4
    mov ah, 06h ;Raw char output
.ansiLp:
    lodsb   ;Get the char in 
    mov dl, al
    int 41h
    dec ecx
    jnz .ansiLp
    return

break:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    ;Here we just get the status of break
    mov eax, 3300h  ;Get break status in dl
    int 41h
    mov bl, dl
    lea rdx, breakIs
    mov ah, 09h
    int 41h
    lea rdx, onMes
    lea rcx, offMes
    test bl, bl ;IF bl = 0, break is off
    cmovz rdx, rcx
    mov ah, 09h
    int 41h
    return
.argumentProvided:
    lea rsi, qword [r8 + fcb1 + fcb.filename]  ;Point to the first fcb name
    lodsd   ;Read the word
    mov ebx, eax
    and eax, 0DFDFh  ;Convert first two chars to uppercase
    shr ebx, 10h     ;Get high word low
    cmp bx, "  " ;Two spaces is a possible ON 
    je .maybeOn
    cmp ax, "OF"
    jne .badArgument
    and bx, 0FFDFh ;Convert only the third char to UC. Fourth char MUST BE SPACE
    cmp bx, "F "
    jne .badArgument
    ;Set off
    xor edx, edx    ;DL=0 => BREAK is off
    jmp short .setBreak
.maybeOn:
    cmp ax, "ON"
    jne .badArgument
    ;Set on
    mov edx, 1
.setBreak:
    mov eax, 3301h  ;Set break
    int 41h
    return
.badArgument:
    lea rdx, badOnOff
    mov ah, 09h
    int 41h
    return

rename:
    return
truename:
    test byte [arg1Flg], -1
    jnz .argumentProvided
    lea rdx, badArgs
    mov ah, 09h
    int 41h
    return
.argumentProvided:
    call buildCommandPath
    ;Explicitly call Truename if we remove truename from this function
    lea rdi, searchSpec
    call strlen
    dec ecx ;Don't print terminating null
    lea rdx, searchSpec
    mov ebx, 01
    mov ah, 40h
    int 41h
    call printCRLF
    return


launchChild:
;We run EXEC on this and the child task will return via applicationReturn
    return