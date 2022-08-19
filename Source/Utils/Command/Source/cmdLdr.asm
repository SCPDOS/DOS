cmdLdr:
    ;Resize Allocation
    neg r8  ;Convert r8 to -r8
    lea rbx, qword [endOfAlloc + r8 + 11h]    ;Get bytes for CMD.COM
    shr ebx, 4  ;Convert to paragraphs
    mov ah, 4Ah ;Realloc
    neg r8  ;Convert -r8 to r8
    int 41h
;Open and parse AUTOEXEC.BAT. Build Environment here
;Finish by printing INIT string.
    lea rdx, initString
    mov ah, 09h
    int 41h ;Print init string
    jmp commandStart
;Static Data Here
masterEnv:
    db "_:COMMAND.COM",0
    db "PATH=",0
    db ($ - masterEnv) dup (" ")    ;Pad the environment with spaces
endOfAlloc:
initString: 
    db CR,LF,"Scientific Computer Research(R) SCP/DOS(R) Version 1.0",CR,LF
    db       "          (C)Copyright Scientific Computer Reserach 2022.",CR,LF,"$"