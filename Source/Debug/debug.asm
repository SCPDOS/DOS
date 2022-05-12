;This file contains debugging macros, procs and strings
; that we can use to output to COM1 bypassing the operating system.
;A serial terminal must be connected for this to work.


debDebuggerActive:
    push rax
    push rsi
    push rdx
    xor edx, edx  ;COM1
    lea rsi, .debLogon
.getChar:
    lodsb
    test al, al
    jz .exit
    mov ah, 01h
    int 34h
    jmp short .getChar
.exit:
    pop rdx
    pop rsi
    pop rax
    ret
.debLogon    db "SCP/BIOS Boot complete.",0Ah,0Dh,
             db "SCP/DOS Kernel Debugger Connected on COM1:2400,n,8,1",0Ah,0Dh,0


