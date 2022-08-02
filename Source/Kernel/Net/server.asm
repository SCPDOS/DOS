;Network functions here

dosServer:  ;ah = 5Dh
;Dispatches the server function in al. Many of these are share hooks.
;Input: For all functions except al = 06h, 07h, 08h, 09h
;   rdx = Pointer to DPL
    cmp al, 07h
    jb .skip
    cmp al, 09h
    jbe .skip2
.skip:
    mov rsi, rdx
    mov rbx, qword [rsi + dpl.compID]   ;Low word only used
    mov word [machineNum], bx
    mov rbx, qword [rsi + dpl.procID]
    mov qword [serverPSP], rbx
    
.skip2:
serverDispatch: ;AX=5D00h
;Input: rsi points to the DOS parameter list
    pop rax
    push rsi    ;Put dpl ptr in rdi
    pop rdi
    call getUserRegs
    xchg rdi, rsi   ;Put ptr to caller frame in rdi and return dpl ptr into rsi
    push rsi
    mov ecx, 6
    rep movsq   ;Copy over first 6 registers from dpl to caller frame
    add rdi, 8  ;Skip rbp on the caller stack
    movsq   ;Transfer r8...
    movsq   ;... and r9
    pop rsi
    mov rax, qword [rsi + callerFrame.rax]
    mov rbx, qword [rsi + callerFrame.rbx]
    mov rcx, qword [rsi + callerFrame.rcx]
    mov rdx, qword [rsi + callerFrame.rdx]
    mov rdi, qword [rsi + callerFrame.rdi]
    mov r8, qword [rsi + callerFrame.r8]
    mov r9, qword [rsi + callerFrame.r9]
    mov rsi, qword [rsi + callerFrame.rsi]
    mov qword [oldRBX], rbx
    mov byte [dosInvoke], -1    ;Mark Server invoke
    jmp functionDispatch.serverEP   ;Enter the server EP


netServices:   ;ah = 5Eh, do nothing
netRedir:;ah = 5Fh, do nothing
    return