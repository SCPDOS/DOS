
;Initialises the share program
;1) Ensure correct DOS version. We are DOS 1.
;2) Parse the command tail. Any malformed char or switch and we abort install.
;3) Allocate a block for MFTs. If we cannot allocate we abort install.
;4) Allocate a block for file locks. If we cannot allocate we abort install.
;5) Halt interrupts
;6) Install Int 2Fh handler.
;7) Copy over all pointers to internal routines to DOS and set Share byte on.
;8) Close all file handles and free the environment.
;9) Terminate and stay resident.
ep:
    mov eax, 3000h  ;Get version number
    int 21h
    cmp al, 1       ;If below or equal to version 1, we are ok with this
    jbe goInit
    mov al, -1
badVerExit:
;Input: al = Error code
    lea rdx, sBadVer
    jmp badPrintAndExit
goInit:
    mov qword [pPSP], r8    ;Save our PSP pointer
    mov eax, 5200h  ;Get ptr to sysvars
    int 21h
    sub rbx, sysVarsPtr ;Subtract the offset into the segment
    mov qword [pDosseg], rbx    ;Store ptr to the head of the segment
    mov al, 80h         ;Error code for no SFT 
    cmp qword [rbx + sftHeadPtr], 0    ;Check if no SFT ptr
    je badVerExit
    mov eax, 1000h              ;SHARE Int 2f check
    int 2Fh
    cmp al, -1
    jne goInstall
    lea rdx, sInstalled
    jmp badPrintAndExit         ;Jump with al = -1
goInstall:
    mov eax, 6100h              ;Get the pointer to the environment in rdx
    int 21h
    test rdx, rdx
    jz skipEnvFree
    push r8
    mov r8, rdx
    mov eax, 4900h              ;And free the environment
    int 21h
    pop r8
skipEnvFree:
;Initialise the sizes with the defaults
    mov dword [dMftArenaSz], MFT_SIZE_DFLT
    mov word [wNumLocks], LOCKS_DFLT
;Get switchchar
    mov eax, 3700h              ;Get switchchar
    int 21h
    mov byte [bSwitchChar], dl
;Point rsi to cmd line and get string count
    lea rsi, qword [r8 + 80h]   ;Go to the command line
    lodsb                       ;Get the string length
    movzx ecx, al               ;into ecx
cmdlineLp:
;Here we process the command line. We only allow for /f and /l switches.
;Capital and lower case allowed. Parse based on string length.
;We really strictly parse to ensure that the user does EXACTLY what they want
    test ecx, ecx
    jz .endParse
    lodsb           ;Read a char
    dec ecx         ;Dec the count
    call isALsep    ;Is this a sep char?
    je cmdlineLp    ;Keep looping if so
    test ecx, ecx   ;If not, but we have no more chars to process, end parse
    jz .endParse
    cmp al, byte [bSwitchChar]
    jne .badParamExit
    mov dl, al      ;Capitalise the char
    mov eax, 6520h  ;Capitalise char in dl with normal table
    int 21h
    cmp dl, "F"
    je .mftSwitch
    cmp dl, "L"
    jne .badParamExit
;Here we figure out how many locks were specified.
    call getNextChar
    cmp al, ":"  ;Next char MUST be a semicolon
    jne .badParamExit
    call getASCIINumber
    cmp ebx, MAX_LOCKS   ;Check the argument not insane
    ja .badParamExit
    cmp word [wNumLocks], bx    ;If the new value is less than default, ignore
    ja cmdlineLp
    mov word [wNumLocks], bx
    jmp short cmdlineLp
.badParamExit:
    lea rdx, sBadParam
    mov al, -1
    jmp badPrintAndExit
.mftSwitch:
    call getNextChar
    cmp al, ":"  ;Next char MUST be a semicolon
    jne .badParamExit
    call getASCIINumber
    cmp ebx, MAX_MFT_SIZE
    ja .badParamExit
    cmp dword [dMftArenaSz], ebx
    ja cmdlineLp
    mov dword [dMftArenaSz], ebx
    jmp cmdlineLp
.endParse:
;Now we allocate the arenas of the required size and place pointers 
; in the right places. 
;Do MFT first
    mov ebx, dword [dMftArenaSz]    ;This is in bytes
    add ebx, 0Fh    ;Round up 
    shr ebx, 4      ;And convert to paragraphs
    mov eax, 4800h
    int 21h
    jc badMemError
    mov qword [pMftArena], rax
;Now initialise this arena. Do this by:
; 1) Zeroing the whole block
; 2) Placing an End MFT entry at the end
; 3) Making a single MFT entry that is free and the size of the arena
    mov rdi, rax
    xor eax, eax
    mov ecx, dword [dMftArenaSz]
    mov edx, ecx    ;Save number of free bytes in the arena
    mov rsi, rdi    ;Save the ptr to the head of the arena
    rep stosb   ;Clear the whole arena
;rdi now points one byte past the end
    dec rdi     ;Now point rdi to the last byte in the arena
    mov byte [rdi + mft.bSig], mftEnd   ;Mark as end of arena
    dec edx     ;One less free byte in the arena
;Allocate the first free mft (which takes up the whole arena) at the head 
    mov byte [rsi + mft.bSig], mftFree
    mov dword [rsi + mft.dLen], edx
;Get a File Lock arena
    movzx ecx, word [wNumLocks]
    mov eax, fileLock_size
    mul ecx     ;Multiply eax with ebx. Fits in eax. Sets edx = 0
    mov ecx, eax    ;Get number of bytes in ecx
    mov ebx, ecx
    add ebx, 0Fh    ;Round up
    shr ebx, 4      ;Get number of paras
    mov eax, 4800h
    int 21h
    jc badMemError
    mov qword [pLockArena], rax 
    mov rsi, rax    ;Point rsi to the head of the arena
    mov rdi, rsi    ;Point rdi there too
    xor eax, eax    
    rep stosb   ;Sanitise the space we requested (ignore any overhang)
    movzx ecx, word [wNumLocks] ;Get number of locks to process
    mov qword [pFreeLock], rsi  ;rsi -> Head of the free locks list
.freeLp:
    mov rdi, rsi    ;Point rdi to where rsi is pointing
    add rsi, fileLock_size  ;Goto next lock
    mov qword [rdi + fileLock.pNext], rsi
    dec ecx         ;One less lock to process
    jnz .freeLp     ;If not done yet, keep adding them to the free lock list
;Get the original Int 2Fh handler in rbx and replace it with our own
    mov eax, 352Fh
    int 21h 
    mov qword [pOldI2Fh], rbx    ;Save the original Int 2Fh handler
    lea rdx, i2fHandler         ;And install our own
    mov eax, 252Fh
    int 21h
;Set SHARE byte in DOS
    mov rbx, qword [pDosseg]    ;Get dosseg ptr
    mov byte [rbx + shareFlag], -1
;Halt interrupts now as we are about to move the share pointers in
    cli
    lea rsi, shareTable
    lea rdi, qword [rbx + shareHooks]
    mov ecx, shareTableL    ;Get number of entries 
;copyLp:
;    movsq
;    lodsq
;    add rax, r8             ;Add the program segment base address to relative addr
;    stosq
;    dec ecx
;    jnz copyLp
    rep movsq   ;PE loader does the pointer fixups
    sti
;Close all handles. We get the number of elements in the jft and 
;close them all. This is done like this to account for the fact that the
;parent let us inherit more than just the standard handles.
    movzx edx, byte [r8 + psp.jftSize]  ;Get the JFT size
    xor ebx, ebx
closeLp:
    mov eax, 3E00h
    int 21h
    inc ebx
    cmp ebx, edx
    jne closeLp
    mov edx, resLenParas + ((psp_size + 0Fh)>> 4)     
    mov eax, 3100h  ;Terminate and stay resident
    int 21h
;A TSR call never returns so we end here.

;Init Common Routines. Not to be used in the main files

getASCIINumber:
;Accumulates the value in ebx and returns it.
;First char read must be a digit, else, we treat a non-digit
; as a terminator of the number. 
;If the value is greater than 32 bits, treat as invalid input
    xor ebx, ebx
    call getNextChar    ;First char after : must be a digit
    call isAlDigit
    jc cmdlineLp.badParamExit
.lp:
    and eax, 0Fh    ;Save lower nybble only and zero the rest of the register
    mov ebp, ebx    ;Dont use lea because we cant check for carry
    shl ebx, 2      ;4*ebx
    jc cmdlineLp.badParamExit
    add ebx, ebp    ;5*ebx
    jc cmdlineLp.badParamExit
    shl ebx, 1      ;10*ebx
    jc cmdlineLp.badParamExit
    add ebx, eax    ;Add new digit value
    jc cmdlineLp.badParamExit
    test ecx, ecx   ;Stop if we run out of chars to process
    retz
    call getNextChar.noCheck    ;Else get the next char
    call isAlDigit              ;If it is a digit, keep processing
    jnc .lp
;Else, we reset to the first non-digit char and return.
    dec rsi
    inc ecx
    return

getNextChar:
;Gets the next char in al and decrements count. If count on entry, fail.
;Used to wrap processing of chars after a switch char
    test ecx, ecx
    jz cmdlineLp.badParamExit
.noCheck:
    lodsb
    dec ecx
    return


isAlDigit:
    cmp al, "0"
    jb .notDigit
    cmp al, "9"
    ja .notDigit
    clc
    return
.notDigit:
    stc 
    return

isALsep:
    cmp al, SPC
    rete
    cmp al, TAB
    rete
    cmp al, LF
    return

badMemError:
    lea rdx, sBadMem
    mov al, -1
badPrintAndExit:
;Input: al = Error code to report
;       rdx -> String to print
    push rax
    mov eax, 0900h
    int 21h
    pop rax
badExit:
    mov ah, 4Ch  ;Exit with error code in al
    int 21h

;All the init vars
bSwitchChar db "/"
;Here we place all the init strings
sBadVer     db "Incorrect DOS version",CR,LF,"$"
sBadParam   db "Incorrect parameter",CR,LF,"$"
sBadMem     db "Not enough memory",CR,LF,"$"
sInstalled  db "SHARE already installed",CR,LF,"$"
;Function table, to be ejected
shareTable:
    dq 0
    dq open           
    dq close          
    dq closeAllByMachine      
    dq closeAllByProcess      
    dq closeAllByName    
    dq lockFile       
    dq unlockFile     
    dq checkRegionLock  
    dq getMFTInfo   
    dq updateFCB 
    dq getFirstClusterFCB   
    dq closeNetworkFiles   
    dq closeRenDel
    dq dirUpdate 
shareTableL equ ($ - shareTable)/8