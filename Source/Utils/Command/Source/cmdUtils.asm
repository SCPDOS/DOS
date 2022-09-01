;Misc functions and subroutines for command.com

printCRLF:
    lea rdx, crlf
    mov ebx, 2
    mov ah, 40h ;Print a new line
    mov ecx, 2  ;Two chars to write
    int 41h
    return
printDate:
    ;al = day of the week (0=Sunday)
	;cl = year (1980-2099)
	;dh = month (1-12)
	;dl = day (1-31)
printFileDate:
	;cl = year (1980-2099)
	;dh = month (1-12)
	;dl = day (1-31)
    return
printTime:
    return
putVersionInPrompt:
    lea rdx, dosVer
    mov ah, 09h ;Print String
    int 41h
    mov ah, 30h ;Get ver in al=Maj ver, ah = Min ver
    int 41h
    push rax    ;Save minor version
    call hexToBCD   ;Get in al a bcd representation for major version
    call printPackedBCD ;Print al
    mov dl, "."
    mov ah, 02h
    int 41h
    pop rax
    mov al, ah  ;Get the minor version low
    call hexToBCD
    call printPackedBCD
    return
putEscInPrompt:
    mov dl, ESC
    jmp short outChar

putMoneyInPrompt:
    mov dl, "$"
    jmp short outChar

putEquInPrompt:
    mov dl, "="
    jmp short outChar

putPipeInPrompt:
    mov dl, "|"
    jmp short outChar

putGTinPrompt:
    mov dl, ">"
    jmp short outChar

putLTinPrompt:
    mov dl, "<"
    jmp short outChar

putDriveInPrompt:
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    add al, "A" ;Convert to letter
    mov dl, al
outChar:
    mov ah, 02h ;Echo to STDOUT
    int 41h
    return
putCWDInPrompt:
    lea rdi, currDirStr ;Update the current directory string
    mov ah, 19h ;Get 0-based current drive number in al
    int 41h
    mov dl, al  ;Get drive letter in dl for path
    inc dl
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
    lea rdx, currDirStr
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

printPackedBCD:
;Gets a packed BCD digit in al and prints al[7:4] if non zero,
; then prints al[3:0]. Prints a space if the upper nybble is zero
;Preserves all registers
    push rax
    push rdx
    mov ah, al
    and al, 0Fh     ;Isolate lower nybble
    and ah, 0F0h    ;Isolate upper nybble
    jnz .upperNybble
    mov dl, " "
    mov ah, 02h
    int 41h
    jmp short .lowerNybble
.upperNybble:
    push rax
    shr ah, 4
    add ah, "0"  ;Convert to an ASCII digit
    mov dl, ah
    mov ah, 02h ;Print DL
    int 41h
    pop rax
.lowerNybble:
    add al, "0"
    mov dl, al
    mov ah, 02h ;Print DL
    int 41h
    pop rdx
    pop rax
    return

getCurrentDrive:
;Returns the current drive in al
    mov ah, 19h
    int 41h
    return

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

findTerminatorOrEOC:
;Advances rsi to the next string terminator char or the next End of command
; char
;Returns with al = terminator and rsi pointing to the char in the string
;If a end of command char found, also sets CF
    lodsb
    cmp al, CR
    je .endOfInput
    call isALterminator
    jz .exit
    cmp al, byte [pathSep]
    je .exit
    cmp al, byte [switchChar]
    je .exit
    jmp short findTerminatorOrEOC
.endOfInput:
    call .exit
    stc 
    return
.exit:
    dec rsi ;Point to the terminating char
    return

findTerminator:
;Advances rsi to the next string terminator char
;Returns with al = terminator and rsi pointing to the char in the string
    lodsb
    call isALterminator
    jnz findTerminator
    dec rsi
    return
isALterminator:
;Returns: ZF=NZ if al is not a terminator (Not including CR)
;         ZF=ZY if al is a terminator
    cmp al, " "
    rete
    cmp al, ";"
    rete
    cmp al, "="
    rete
    cmp al, ","
    rete
    cmp al, TAB
    rete
    cmp al, LF
    return

findEndOfCommand:
;Moves rsi to the | or CR that terminates this command
    lodsb
    call isALEndOfCommand
    jnz findEndOfCommand
    dec rsi
    return
isALEndOfCommand:
    cmp al, "|"
    rete
    cmp al, CR
    return

scanForRedir:
;Returns: AL = 0 => No redirection, terminate with CR
;         AL = 1 => Redirection, type <
;         AL = 10 => Redir, type >
;         AL = 20 => Redir, type >>
;If multiple redirs found, the last one of that type counts.
    push rsi
    push rbp
    xor ah, ah
.lp:
    lodsb
    cmp al, ">"
.exit:
    pop rbp
    pop rsi
    return


skipSpaces:
;Also skips tabs
;Input: rsi must point to the start of the data string
;Output: rsi points to the first non-space char
    cmp byte [rsi], " "
    je .skip    ;If equal to a space, skip it
    cmp byte [rsi], TAB
    retne   ;If not equal to a tab or space, return
.skip:
    inc rsi
    jmp short skipSpaces

printPrompt:
    cmp word [promptPtr], -1
    jne .validPrompt
    ;Here we print the default prompt
    call putCWDInPrompt
    call putGTinPrompt
    return
.validPrompt:
    return

clearCommandState:
;Clears the command state
    lea rdi, cmdStatePtr
    mov ecx, cmdStateL
    xor eax, eax
    rep stosb
    return

clearCommandLineState:
;Clears the command line state after a 0Dh encountered
    lea rdi, cmdLineStatePtr
    mov ecx, cmdLineStateL
    xor eax, eax
    rep stosb
    return

asciiToFCB:
;Converts a filename in the form FILENAME.EXT,0 to FILENAMEEXT
;Don't uppercase any lowercase chars as this could be used with user buffers.
;Also doesn't check if chars are valid
;Names such as SYS.COM get converted to "SYS     COM"
;Name is space padded.
;Input: rsi = ASCII string buffer
;       rdi = FCB name buffer
;Output: al = Char that terminated the source string 
    push rbx    
    push rdi
    mov ecx, 11
    mov al, " "
    rep stosb   ;Fill the buffer with spaces (so we don't need to fill later)
    pop rdi
    mov rbx, rdi    ;Use rbx as the base pointer of this buffer
    jmp short asciiFilenameToFCB.processName
asciiFilenameToFCB:
;Converts a filename in the form FILENAME.EXT,0 to FILENAMEEXT
;Don't uppercase any lowercase chars as this could be used with user buffers.
;Also doesn't check if chars are valid
;Names such as SYS.COM get converted to "SYS     COM"
;Name is space padded.
;Input: rsi = ASCII string buffer
;       rdi = FCB name buffer
;Output: al = Char that terminated the source string 
    push rbx    
    push rdi
    mov ecx, 11
    mov al, " "
    rep stosb   ;Fill the buffer with spaces (so we don't need to fill later)
    pop rdi
    mov rbx, rdi    ;Use rbx as the base pointer of this buffer
    ;Preprocess for Dir Searches
    cmp byte [rsi], "."
    jne .processName
    movsb   ;Store the first dot
    cmp byte [rsi], "."
    jne .exitBadChar
    movsb
    jmp short .exitBadChar
.processName:
    lodsb   ;Get the char in al
    test al, al ;If the char is a null, must be at the end of the name
    jz .exit
    cmp al, " " ;If space or a period, go to extension field. If null, exit
    je .extSpace
    cmp al, "."
    je .ext
    stosb   ;Store the char
    jmp short .processName
.extSpace:
;Now we scan for a period in the name
    lodsb   ;Get a char and increase rsi
    test al, al
    jz .exit
    cmp al, "."     ;If al is not a period...
    jne .extSpace   ; keep searching
.ext:
    lea rdi, qword [rbx + filename.fExt]    ;Put destination at the extension
.processExt:
    lodsb
    test al, al
    jz .exit
    cmp al, " "
    je .exit
    stosb
    jmp short .processExt
.exitBadChar:
    xor al, al  ;Return a null terminator
.exit:
    pop rbx
    return

FCBToAsciiz:
;Converts a filename in the form FILENAMEEXT to FILENAME.EXT,0
;Name is space padded too
;Input: rsi = FCB name buffer
;       rdi = ASCIIZ string buffer
    mov ecx, 8
    rep movsb   ;Move the name over
.scanNameSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space?
    jne .ext
    dec rdi
    inc ecx
    cmp ecx, 8
    jb .scanNameSpace
.ext:
    cmp word [rsi], "  "    ;Are the first two chars a space?
    jne .validExt
    cmp byte [rsi + 2], " " ;Is the final char a space?
    je .exit
.validExt:
    mov al, "." ;We have a valid extension, store a period
    stosb
    mov ecx, 3
    rep movsb   ;Move the three extension chars over
.scanExtSpace:
    cmp byte [rdi - 1], " " ;Is the previous char a space
    jne .exit
    dec rdi
    jmp short .scanExtSpace
.exit:
    xor eax, eax
    stosb   ;Store a null at the end
    return

buildCommandPath:
;Based on the first argument on the command line
; will build a full ASCIIZ path in searchSpec to the file/dir specified
    ;If this is a relative path, will handle correctly (tho unnecessary)
    movzx eax, byte [arg1Off]
    lea rsi, cmdBuffer
    add rsi, rax    ;Go to the start of the command
    mov bh, byte [pathSep]
    mov bl, ":"
    cmp byte [rsi], bh  ;Is the first char relative to root?
    je .absoluteCurrent
    cmp word [rsi + 1], bx    ;This checks if absolute or relative
    je .absolutePath
    cmp byte [rsi + 1], bl  ;Check if a drive separator
    je .relativeGiven
;No drive letter given, must get Current Drive
    call getCurrentDrive    ;Get current drive number (0 based) in al
    add al, "A"
    jmp short .relativeCommon
.relativeGiven:
;Drive letter pointed to by AL
    mov al, byte [rsi]  ;Get drive letter in al
    and al, 0DFh    ;Convert to UC
    add rsi, 2  ;Skip the given drive letter and the colon
.relativeCommon:
    ;al has drive letter
    mov dl, al  ;Save drive letter in dl
    sub dl, "@" ;Get 1 based drive number in dl
    mov ah, ":" ;Get the colon in too
    lea rdi, searchSpec ;Start building our search path here
    stosw   ;Store X:
    mov al, byte [pathSep]
    stosb   ;Store pathSep
    push rsi    ;Save user input string
    mov rsi, rdi    ;Put the current directory here for this drive
    mov ah, 47h ;Get Current Working Directory, dl has drive number
    int 41h ;Won't fail as drive letter in dl confirmed ok
    pop rsi
    ;Now want to find terminating null
    xor al, al
    xor ecx, ecx
    dec ecx
    repne scasb ;Search for the terminating null
    dec rdi ;Go back one once found
    mov al, byte [pathSep]
    cmp byte [rdi - 1], al
    je .buildPath ;If the previous char is a pathsep, skip storing another
    stosb   ;Store the pathsep
    jmp short .buildPath  ;Now we copy the user string over and good to go
.absoluteCurrent:
    call getCurrentDrive    ;Get current drive number (0 based) in al
    add al, "A"
    mov ah, ":" ;ax has X: now to store 
    lea rdi, searchSpec
    stosw
    mov al, byte [pathSep]
    stosb
    jmp short .buildPath
.absolutePath:
    lea rdi, searchSpec
.buildPath:
    call copyCommandTailItem    ;Terminates with a 0 for free
    lea rsi, searchSpec
    lea rdi, searchSpec
    mov ah, 60h ;Truename it to avoid issues
    int 41h
    retc    ;Return if an error with CF=CY
    ;Here we do one final check to ensure we dont end up with a A: but A:"\"
    xor al, al
    xor ecx, ecx
    dec ecx
    repne scasb
    dec rdi ;Go back to the final non-null char
    cmp byte [rdi - 1], ":" ;Is the final non-null char a colon?
    jne .okExit   ;Return if not
    xor eax, eax
    mov al, byte [pathSep]  ;IF it is, insert a pathsep
    stosw   ;Store the terminating 0 after the pathsep
.okExit:
    clc
    return
printDecimalWord:
;Takes qword in edx:eax and print it's decimal representation
    push rdx    ;Save upper qword for later
;Takes the qword in eax and prints its decimal representation
    xor ecx, ecx
    xor ebp, ebp  ;Use bp as #of digits counter
    mov ebx, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    shl rcx, 8    ;Space for next nybble
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    mov cl, dl    ;Save remainder byte
    test rax, rax
    jnz .dpfb0
.dpfb2:
    mov dl, cl    ;Get most sig digit into al
    mov ah, 02h
    int 41h
    shr rcx, 8    ;Get next digit down
    dec ebp
    jnz .dpfb2
    pop rdx
    test edx, edx   ;If this is zero, we are done
    retz    ;Return
    mov eax, edx    ;Else move it to eax
    xor edx, edx    ;And clear it
    jmp short printDecimalWord

getDecimalWord:
;Works on MAX A dword in eax
;Gets the decimalised DWORD to print in rcx (at most 8 digits)
    xor ecx, ecx
    xor ebp, ebp  ;Use bp as #of digits counter
    mov ebx, 0Ah  ;Divide by 10
.dpfb0:
    inc ebp
    shl rcx, 8    ;Space for next nybble
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .dpfb1
    add dl, 'A'-'0'-10
.dpfb1:
    mov cl, dl    ;Save remainder byte
    test rax, rax
    jnz .dpfb0
    return