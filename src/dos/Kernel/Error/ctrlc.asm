ctrlBreakHdlr:
    mov al, 03h ;Always guarantee a ^C will be printed
    call printCaretASCII
    call printCRLF
    ;Reset the console back to 0
    call vConRetDriver
;Handles a control break, juggles stacks and enters int 21h 
.exceptEP:
;If return via RET/RET 8 with CF set, DOS will abort program with errorlevel 0
;Else (RET/RET 8 with CF clear or IRET with CF ignored)
;   interrupted DOS call is restarted
	cli
	mov rsp, qword [oldRSP]	;Get registers frame
	call dosPopRegs ;Get user state back
    mov byte [inDOS], 0 ;Make sure we "exit" DOS 
    mov byte [critErrFlag], 0
    mov qword [xInt23hRSP], rsp  ;Save user rsp. This is the og psp rsp.
    clc
    int 23h ;Call critical error handler
    cli ;Clear interrupts again
    mov qword [oldRAX], rax     ;Save eax (has DOS fn to do if not terminate)
    pushfq                      ;Get returned flags in rax
    pop rax 
    cmp rsp, qword [xInt23hRSP] ;Did the user return with ret/ret 8?
    jne .checkCF                ;If yes, check the returned CF flag.
.returnToDOS:                   ;Else, execute the DOS function in eax.
    mov rax, qword [oldRAX]     ;Get the function number to execute
    jmp functionDispatch        ;Goto int 21h
.checkCF:
    mov rsp, qword [xInt23hRSP]  ;Reset the stack pointer to pre int 23h
    test al, 1      ;Was CF=CY returned?
    jz .returnToDOS ;If CF=NC, DOS function in eax
    mov eax, 4c00h  ;Else, exit with default error code (00h)
    mov byte [ctrlCExit], -1  ;Signal CTRL+BREAK termination
    jmp functionDispatch ;When jumping now, rsp will go back into psp.rsp!
