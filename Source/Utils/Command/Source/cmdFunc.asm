dir:
    ;If a particular path is given, we search for it.
    ;Else, if rsi is pointing at CR, . or " " we search in CWD for *.*
    cmp byte [rsi], CR
    je .searchCWD

.searchCWD:
chdir:
mkdir:
rmdir:
erase:
date:
time:
copy:
ctty:
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
rename:
truename:
launchChild:
;We run EXEC on this and the child task will return via applicationReturn