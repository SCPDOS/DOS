;Generic Find First and Find Next functions here

genericFindFirst:
    ret
genericFindNext:
    ret

resetSearchVars:
    

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
