;Generic Find First and Find Next functions here

genericFindFirst:
    return
genericFindNext:
    return
getDrvLetterFromPath:
;Gets the drive letter for the path in al
;Input: rsi = Buffer to process
;Output: If al = 0, rsi NOT incremented by 2. Else, rsi incremented by 2 
;       ZF=ZE and al = 0 => Null path
;       ZF=NZ and al = 0 => Relative path, splice flag = 0 or Net path
;       ZF=NZ and al = -1 => Bad drive number
;       ZF=NZ and al = Drive letter => All oki
    xor al, al
    cmp byte [rsi], 00h ;Is this a null path?
    retz    ;Return if a null path
    cmp byte [rsi + 1], ":" ;Path separator?
    retne   ;If not equal, Relative path or network path
    lodsw   ;Get first word
    ;Make char lower case if its not and then convert to offset from "a" - 1
    or al, 20h  ;Set the bit for lowercase chars
    sub al, 60h
    retnz ;If the number is non-zero, then a potentially valid drive number
    mov al, -1  ;Else not a valid drive number
    return
transferPath:
;Called with:
; rdi = SDA Buffer for filename
; rsi = Potentially unqualified filename
    mov byte [fileDirFlag], -1  ;Set to neither File not Dir
    mov al, -1
    mov byte [spliceFlag], al   ;Reset splice (0 = Relative path, 1 = Full path)
    mov byte [filspcExist], al  ;We are searching for a file that exists
    mov qword [fname1Ptr], rdi  ;Save the SDA buffer we are using for this file
    cbw  ;make ah = -1
    mov word [lastPartOff], ax  ;Store -1 for the last part of the path
    lea rbp, qword [rdi + 086h] ;Goto first non X:\ char of next buffer in rbp
    test byte [dosInvoke], -1   ;Was it invoked via server? -1 = Server
    jz .notServer

.notServer:
    ;Make Redir request to qualify the filename if NOT invoked by server call
    mov qword [workingCDS], -1  ;Set workingCDS to unknown
    mov eax, 1123h
    int 4fh ;CF=CY if not resolved. CF=NC if resolved
    jnc .notServerExit  ;Return if resolved
    call getDrvLetterFromPath ;Get the drive letter in al (or -1)
    push rax    ;Save whether rsi is incremented by 2
    mov ax, word [rsi]   ;Get the word pointed to by rsi
    call swapPathSeparator  ;Convert al if it is a path separator
    xchg ah, al ;Now swap al into ah to check if we on a network path (i.e. \\)
    call swapPathSeparator  ;Returns ZF=ZE if al = / or \
    jnz .notNet
    cmp ah, al  ;If they are equal, we have a net path
    jne .notNet
    pop rax ;We are in a net situation, so rsi is pointing at \\
    movsw   ;Tfr the two chars rsi, rdi + 2
.moveNetChars:
    lodsb   ;Get the third char into al and inc rsi
    call uppercaseChar  ;Make char in al uppercase
    test al, al
    jz .netEnd
    call swapPathSeparator  ;If path sep, swap it
    mov rbx, rdi    ;Store current buffer offset in rbx
    stosb
    jnz .moveNetChars  ;If not a path separating char in al, keep looking
    call qualifyPath    ;Now qualify the path portion
    return
.netEnd:
    stosb
    return
.notServerExit:
    return
.notNet:
    pop rax
    cmp byte [rsi], 0
    jnz .notNetNotNull  ;Not a null path
    mov eax, errFnf ;File not found error
    stc
    return
.notNetNotNull:
    push rax
    push rbp

qualifyPath:
    return


getPathType:
;We establish that the path given in rdx is acceptable:
;Five path types:
;1) Network, assumed fully qualified here
;2) Given Drive, Relative to Root (not split)
;3) Given Drive, Relative to CWD (split)
;4) Current Drive, Relative to Root (not split)
;5) Current Drive, Relative to CWD (split)
;Establish which type of path and dispatch routine to
; fill buffer1 with a Fully Qualified path as needed
;Input: rdx = Valid Path
;Output: buffer1 = Fully qualified path for search
;ah = 1 -5 for above numberings
; Set flag if relative to CWD to start search from CWD
;THIS NEEDS TO BE CORRECTED
    lea rdi, buffer1    ;Set destination buffer
    movzx eax, word [rdx]   ;Get first two chars
    cmp eax, "\\"
    je .network
    cmp eax, "//"
    je .network
    cmp ah, ":" ;Is this a given drive request?
    jne .currentDrive
.givenDrive:    ;Fall thru given drive paths
;al has drive letter. Conver to UC and set as working
    call uppercaseChar
    sub al, "A" ;Get 1 based drive number
    call getCDS ;Set working drv and working CDS in vars
    retc    ;Return with CF=CY if error
    mov eax, dword [rdx + 2]    ;Get char 3
    call .checkRootRel
    mov ah, 2   ;Given drive signature
    lea rsi, qword [rdx + 2]    ;Move rsi to skip X:
    jz .rootRelative
    jmp short .cwdRel
.currentDrive:
    xor al, al  ;Get current drive
    call getCDS ;Set working drv and working CDS in vars
    retc
    mov ah, 4   ;Current drive signature
    call .checkRootRel
    mov rsi, rdx
    jz .rootRelative
.cwdRel:
    inc ah  ;Convert from relative to root to relative to cwd
    ;Copy path from CDS to buffer
    mov rsi, qword [workingCDS]
.rootRelative:
    call .copyBuffer
    return
.network:
;For network calls, the path name must already be Fully qualified
;Transfer buffer into buffer1
    mov rsi, rdx
    mov ah, 1   ;Mark the buffer as a network path buffer
    call .copyBuffer
    return
.checkRootRel:
;If return with ZF=ZE then Root Relative
;Input: al = Char to check
    cmp al, "/"
    rete
    cmp al, "\"
    return
.copyBuffer:
    ;Input: rsi = Position of the Path
    ;Output: rsi shifted
    lodsb
    cmp ah, 1   ;Net?
    je .skipQualification   ;Yup, skip qualification
    call uppercaseChar
    call swapPathSeparator
.skipQualification:
    stosb   ;Store terminating null
    test al, al
    retz
    jmp short .copyBuffer
