dir:
    ;If a particular path is given, we search for it.
    ;Else, if rsi is pointing at CR, . or " " we search in CWD for *.*
    cmp byte [rsi], CR
    je .searchCWD

.searchCWD:
    return
chdir:
mkdir:
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
truename:
launchChild:
;We run EXEC on this and the child task will return via applicationReturn
    return