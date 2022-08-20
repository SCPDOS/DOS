cmdLdr:
    mov al, 19h ;Get current Drive
    int 41h
    add al, "A"
    mov byte [masterEnv], al
    lea rax, qword [r8 + psp.dta]
    mov qword [cmdLinePtr], rax
;Check if Int 4Eh has the same address as Int 4Dh. If so, 
; it is as when DOS initially was run.
    mov eax, 354Eh  ;Get int 4Eh address
    int 41h
    mov rdx, rbx    ;Save the pointer in rdx
    mov eax, 354Dh  ;Get int 4Dh address
    int 41h
    cmp rdx, rbx    ;If these are equal then this is first boot!
    jne .skipMaster
    lea rdx, strtmsg
    mov ah, 09h
    int 41h
;Open and parse AUTOEXEC.BAT. Build Master Environment here
.skipMaster:
;Finish by printing INIT string.
    lea rdx, initString
    mov ah, 09h
    int 41h ;Print init string
    jmp commandStart    ;Once we jump, we then jettison the loader too
;Loader Data here
strtmsg: db "Starting SCP/DOS...",0Ah,0Dh,"$"
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version 1.0",CR,LF
    db       "          (C)Copyright Scientific Computer Reserach 2022.",CR,LF,"$"