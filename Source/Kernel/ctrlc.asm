criticalDOSError:
;Will swap stacks and enter int 44h safely and handle passing the right data 
; to the critical error handler.
; Called with ax, di and rsi set as required by Int 44h (caller decides)
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;                  = 2 - Terminate the Program  (Abort)
;                  = 3 - Fail the DOS call      (Fail)
; Return response from int 44h in al
; Caller must preserve rsp, rbx, rcx, rdx if they wish to return to DOS
    cli ;Disable Interrupts
    mov byte [critErrFlag], 1   ;Set flag for critical error
    mov byte [Int44RetVal], 0   ;Clear the return value
    mov byte [Int44bitfld], ah  ;Save the bitfield
    mov qword [xInt44hRSP], rsp
    mov di, word [Int44Error]   ;Get the error code in di
    mov rsp, qword [oldRSP] ;Get the old RSP value
    sti
    int 44h ;Call critical error handler
    cli
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    mov byte [critErrFlag], 0   ;Clear critical error flag
    mov byte [Int44RetVal], al  ;Save the return value
    sti ;Reenable Interrupts
    ret

ctrlBreakHdlr:
;Handles a control break, juggles stacks and enters int 41h 
	cli
	mov rsp, qword [oldRSP]	;Get registers frame
	call dosPopRegs ;Get user state back
    mov qword [xInt43hRSP], rsp  ;Save user rsp
    clc
    int 43h ;Call critical error handler
    cli ;Clear interrupts again
    mov qword [oldRAX], rax ;Save rax
    pushfq  ;Get flags in rax
    pop rax 
    cmp rsp, qword [xInt43hRSP] ;Did the user return with ret 8?
    jne .checkCF
.returnToDOS:
    mov rax, qword [oldRAX]
    jmp functionDispatch    ;Goto int 41h
.checkCF:
    add rsp, 8  ;Account for the flags left on the stack
    test al, 1  ;CF set?
    jz .returnToDOS ;Yes, subfunction number must be in al
    mov eax, 4c00h  ;Exit without error code
    mov byte [critExit], -1  ;CTRL+BREAK termination
    jmp functionDispatch

