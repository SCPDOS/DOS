diskDevErr:
;Called, NOT Jumped to. 
;Input: rdi = Disk Buffer pointer
;       eax = Status word (Zero Extended)
; [Int44hbitfld] = Specific bitflags (r/w AND potential extra ok responses)
;Output: al = Int 44h response (0-3)
; All other registers preserved
    mov qword [xInt44RDI], rdi  ;Save rdi (Disk Buffer pointer)
    mov edi, eax    ;Store status word in rdi
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov ah, byte [Int44bitfld]  ;Get the permissions in var
    or ah, critFailOK | critRetryOK ;Set bits
    ;Test for correct buffer data type
    push rbx    ;Save rbx temporarily
    mov bl, byte [rdi + bufferHdr.bufferFlags]  ;Get the buffer data type
    test bl, dosBuffer
    jnz .df0
    or ah, critDOS  ;Add DOS data type bit
    jmp short .df3
.df0:
    test bl, fatBuffer
    jnz .df1
    or ah, critFAT  ;Add FAT data type bit
    jmp short .df3
.df1:
    test bl, dirBuffer
    jnz .df2
    or ah, critDir  ;Add Directory data type bit
    jmp short .df3
.df2:
    or ah, critData ;Here it must be a data buffer
.df3:
    pop rbx
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Get driver header ptr from dpb
    call criticalErrorSetup ;Save ah and rbp in this function
    mov rbp, qword [tmpDPBPtr]  ;Get back rbp that was saved by critErrSetup
    mov rdi, qword [xInt44RDI]  ;Return original rdi value
    return

charDevErr:
;Called with ah with additional bits
    or ah, critIgnorOK | critRetryOK | critFailOK  ;Ignore,Retry,Fail OK
criticalErrorSetup:
    mov byte [Int44bitfld], ah  ;Save bitfield
    mov qword [tmpDPBPtr], rbp  ;rbp is the DPB if a disk operation errored
    and edi, 00FFh  ;Save only low byte of error
    ;For now, fall through, but need much work to change it later! 


criticalDOSError:   ;Int 4Fh, AX=1206h, Invoke Critical Error Function 
;Will swap stacks and enter int 44h safely and handle passing the right data 
; to the critical error handler.
; Called with rsi set as required by Int 44h (caller decides), ax, di
; and with Int44Bitfield set
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
;               XXX= 2 - Terminate the Program  (Abort)XXX
;                  = 3 - Fail the DOS call      (Fail)
; Return response from int 44h in al
; Caller must preserve rsp, rbx, rcx, rdx if they wish to return to DOS
; This function will terminate the program if an abort was requested!
; This function also destroys RBP
    cmp byte [critErrFlag], 1
    jb .noIntError  ;If not 0, enter
    mov al, critFail    ;Else, return Fail always
    jmp short .checkResponse
.noIntError:
    cli ;Disable Interrupts
    inc byte [critErrFlag]  ;Set flag for critical error
    dec byte [inDOS]    ;Exiting DOS
    mov qword [xInt44hRSP], rsp
    mov rsp, qword [oldRSP] ;Get the old RSP value
    xor ebp, ebp
    int 44h ;Call critical error handler, sets interrupts on again
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    mov byte [critErrFlag], 0   ;Clear critical error flag
    inc byte [inDOS]    ;Reenter DOS
    sti ;Reenable Interrupts
    ;Now we check that the response given was allowed, and translate if needed
.checkResponse:
    cmp al, critRetry
    jb .checkIgnore
    je .checkRetry
    cmp al, critFail
    jne .abort   ;Must be abort
;Here is for fail
    test byte [Int44bitfld], critFailOK
    jnz .abort
.exit:
    mov byte [errorDrv], -1 ;Unknown drive (to be set)
    return
.checkIgnore:
    test byte [Int44bitfld], critIgnorOK
    jnz .exit
    jmp short .setFail
.checkRetry:
    test byte [Int44bitfld], critRetryOK
    jnz .exit
.setFail:
    mov al, critFail
    inc byte [Int44Fail]        ;Inc the fail counter!
    jmp short .checkResponse    ;Double check if I can return Fail
.abort:
;Prepare to abort. We abort from within!
;Currently fall into ^C
ctrlBreakHdlr:
    mov al, 03h ;Always guarantee a ^C will be printed
    call printCaretASCII
    call printCRLF
    ;Reset the console back to 0
    mov byte [vConDrvSwp],  0   ;Set to 0
.avoidCON:
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
    mov byte [ctrlCExit], -1  ;CTRL+BREAK termination
    jmp functionDispatch

