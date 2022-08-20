cmdLdr:
;First store self as parent in the PSP, to prevent accidental closure
    mov rax, qword [r8 + psp.parentPtr] ;Get PSP parent
    mov qword [r8 + psp.parentPtr], r8  ;Store self as parent
    mov qword [realParent], rax ;Preserve the real parent address
;Setup Int 43h and Int 44h
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
;Determine if this is the master copy of COMMAND.COM
;Check if Int 4Eh has the same address as Int 4Dh. If so, we are master.
    mov eax, 354Eh  ;Get int 4Eh address
    int 41h
    mov rdx, rbx    ;Save the pointer in rdx
    mov eax, 354Dh  ;Get int 4Dh address
    int 41h
    cmp rdx, rbx    ;If these are equal then this is first boot!
    jne .skipMaster
;Ok so we are master command.com
    lea rdx, strtmsg
    mov ah, 09h
    int 41h
;Now myself as the real parent
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
    lea rdx, parseCommandLine
    mov eax, 254Eh ;Set this as Int 4Eh
    int 41h
;Now, open and parse AUTOEXEC.BAT. Build Master Environment here
    
    lea rbx, endOfAlloc ;Save the Master Environment
    jmp short .printInit
.skipMaster:
    lea rbx, masterEnv  ;This is the base address to jettison
.printInit:
;Finish by printing INIT string.
    lea rdx, initString
    mov ah, 09h
    int 41h ;Print init string
    jmp commandStart    ;We jump with rbx = base address to jettison
;Loader Data here
strtmsg: db "Starting SCP/DOS...",0Ah,0Dh,"$"
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version 1.0",CR,LF
    db       "          (C)Copyright Scientific Computer Reserach 2022.",CR,LF,"$"