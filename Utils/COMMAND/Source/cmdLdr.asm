cmdLdr:
;First check if the version is ok. If not, return.
    mov ah, 30h
    int 41h
    cmp al, 01h ;Version 1
    jbe .okVersion
    lea rdx, badVerStr
    mov ah, 09h
    int 41h
    int 40h ;Exit to caller or DOS to print bad command interpreter line
.okVersion:
;If ok then store self as parent in the PSP, to prevent accidental closure
    mov qword [pspPtr], r8  ;Store PSP ptr in internal var 
    mov rax, qword [r8 + psp.parentPtr] ;Get PSP parent
    mov qword [r8 + psp.parentPtr], r8  ;Store self as parent
    mov qword [realParent], rax ;Preserve the real parent address
;Setup Int 42h, Int 43h and Int 44h
    mov rax, qword [r8 + psp.oldInt42h] ;Preserve the original addresses
    mov qword [parentInt42], rax

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
;Get a pointer to DOS Sysvars
    mov ah, 52h ;Get sysvars
    int 41h
    mov qword [sysVars], rbx    ;Save ptr to sysVars
;Call for simple internationalisation data
    mov eax, 3700h  ;Get switchchar in dl
    int 41h
    cmp al, -1
    je .skipSwitch
    mov byte [switchChar], dl   ;Store the switchChar in var
    cmp dl, "-" ;Is the switchChar Unix?
    jne .skipSwitch
    mov byte [pathSep], "/" ;Swap default path separator to UNIX style
.skipSwitch:
    mov eax, 3800h  ;Get current country data
    lea rdx, ctryData
    int 41h ;Write the data to the internal country table
;Now determine if this is the master copy of COMMAND.COM
;Check if Int 4Eh has the same address as Int 4Dh. If so, we are master.
    mov eax, 354Eh  ;Get int 4Eh address
    int 41h
    mov rdx, rbx    ;Save the pointer in rdx
    mov eax, 354Dh  ;Get int 4Dh address
    int 41h
    cmp rdx, rbx    ;If these are equal then this is first boot!
    jne .skipMaster
;Ok so we are master command.com
;Now make myself the real parent
    mov byte [permaSwitch], -1  ;Set the permanently resident switch on
    mov qword [realParent], r8
;Set master environment as mine
    lea rax, masterEnv
    mov qword [r8 + psp.envPtr], rax
;Set current Drive in COMSPEC
    mov al, 19h ;Get current Drive
    int 41h
    add al, "A"
    mov byte [masterEnv], al
;Set Int 4Eh up
    lea rdx, int4Eh
    mov eax, 254Eh ;Set this as Int 4Eh
    int 41h
;Now, open and parse AUTOEXEC.BAT. Build Master Environment here
;If no AUTOEXEC.BAT, request time and date from user
    lea rdx, crlf
    mov ah, 09h
    int 41h
    call time
    call date
    lea rdx, crlf
    mov ah, 09h
    int 41h
    lea rbx, endOfAlloc ;Save the Master Environment
    jmp short .printInit
.skipMaster:
    lea rdi, qword [r8 + psp.progTail]
    movzx ecx, byte [r8 + psp.parmList]
    movzx eax, byte [switchChar]
    repne scasb
    jecxz .noSwitches
    movzx eax, byte [rdi]   ;RDI points to the char after the switch
    and al, 0DFh    ;Convert to UC
    cmp al, "P" ;Is it permanent switch?
    jne .noSwitches
    mov byte [permaSwitch], -1  ;Set the permanently resident switch on
.noSwitches:
    lea rbx, endOfAllocNoMaster  ;This is the base address to jettison
.printInit:
;Finish by printing INIT string.
    push rbx
    lea rdx, initString
    mov ah, 09h
    int 41h ;Print init string
    call version.printVersionNumber
    lea rdx, initString2
    mov ah, 09h
    int 41h ;Print init string
    pop rbx
    ;Now we add the stack to the alloc and paragraph align
    add rbx, stackSize
    add rbx, 11h    ;Go one para up
    shr rbx, 4      ;Round to this new para boundary
    shl rbx, 4
    mov rsp, rbx    ;Move the stack pointer to this address
    mov qword [stackTop], rbx   ;Save this value of the stack ptr in var
    jmp commandStart    ;We jump with rbx = base address to jettison
;Loader Data here
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version $"
initString2:
    db CR,LF, "          (C)Copyright Scientific Computer Reserach 2022.",CR,LF,"$"
badVerStr: db "Incorrect DOS version",CR,LF,"$"