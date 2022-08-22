;Misc functions and subroutines for command.com

printCRLF:
    lea rdx, crlf
    mov ebx, 2
    mov ah, 40h ;Print a new line
    mov ecx, 2  ;Two chars to write
    int 41h
    return

putDateinPrompt:

putVersionInPrompt:

putMoneyInPrompt:
    mov dl, "$"
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putEquInPrompt:
    mov dl, "="
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putPipeInPrompt:
    mov dl, "|"
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putGTinPrompt:
    mov dl, ">"
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putLTinPrompt:
    mov dl, "<"
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putDriveInPrompt:
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    add al, "A" ;Convert to letter
    mov dl, al
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return

putCWDInPrompt:
    lea rdi, strBuf
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    add al, "A" ;Convert to letter
    mov ah, ":"
    stosw   ;Store X:, rdi+=2
    mov al, byte [pathSep]
    stosb   ;Store pathSep, inc rdi
    mov ah, 47h ;Get Current Working Directory
    mov rsi, rdi    ;rsi points to buffer to write to
    int 41h
    call strlen
    add ecx, 2 ;Add two for the X:
    ;We repurpose the fact that strlen counts the NULL to account for "\"
    mov ah, 40h ;Write to handle
    mov ebx, 1  ;STDOUT
    lea rdx, strBuf
    int 41h
    return

BCDtoHex:
;Converts a BCD value to a Hex byte
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    mov ecx, eax    ;Save al in ecx
    and eax, 0Fh    ;Get lower nybble
    and ecx, 0F0h   ;Get upper nybble
    shr ecx, 4      ;Shift upper nybble value down
.bth:
    add eax, 10
    dec ecx
    jnz .bth
    pop rcx
    ret

hexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in al, returns in al (zero-ed upper seven bytes)
    push rcx
    movzx eax, al   ;Zero extend
    xor ecx, ecx
.htb0:
    cmp eax, 10
    jb .htb1
    sub eax, 10
    inc ecx
    jmp short .htb0
.htb1:
    shl ecx, 4  ;Move to upper nybble
    or al, cl   ;Move upper nybble into al upper nybble
    pop rcx
    ret

strlen:
;Gets the length of a ASCIIZ string
;Input: rdi = Source buffer
;Output: ecx = Length of string, INCLUDING TERMINATING NULL
    push rax
    push rdi
    xor al, al
    xor ecx, ecx    ;ONLY USE ECX!!!
    dec ecx ;rcx = -1
    repne scasb
    not ecx
    pop rdi
    pop rax
    return