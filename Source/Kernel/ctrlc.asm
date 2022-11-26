fullcriticalErrorInvoke:
    mov eax, 03 ;Always fail for now
    stc
    return

diskDevErr:
;Called, NOT Jumped to. 
;Input: rdi = Disk Buffer pointer
;       eax = Status word (Zero Extended)
;       rbp = Disk DPB pointer
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
    jmp short .exit     ;Don't translate fail to abort
.noIntError:
    cli ;Disable Interrupts
    inc byte [critErrFlag]  ;Set flag for critical error
    dec byte [inDOS]    ;Exiting DOS
    mov qword [xInt44hRSP], rsp
    mov rsp, qword [oldRSP] ;Get the old RSP value
    xor ebp, ebp    ;Always zeroed
    int 44h ;Call critical error handler, sets interrupts on again
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    mov byte [critErrFlag], 0   ;Clear critical error flag
    inc byte [inDOS]    ;Reenter DOS
    sti ;Reenable Interrupts
    ;Now we check that the response given was allowed, and translate if needed
.checkResponse:
    cmp al, critIgnore
    je .checkIgnore
    cmp al, critRetry
    je .checkRetry
    cmp al, critFail
    jne .abort   ;Must be abort
.setFail:   ;Here is for fail
    mov al, critFail    ;Reset al to contain fail (even if Int44 responded Fail)
    inc byte [Int44Fail]        ;Inc the fail counter!
    test byte [Int44bitfld], critFailOK
    jz .abort  ;If bit not set, fail not permitted, abort
.exit:
    mov byte [errorDrv], -1 ;Unknown drive (to be set)
    return
.checkIgnore:
    test byte [Int44bitfld], critIgnorOK
    jnz .exit
    jmp short .setFail  ;If ignore not permitted, return Fail
.checkRetry:
    test byte [Int44bitfld], critRetryOK
    jnz .exit   
    jmp short .setFail  ;If retry not permitted, return Fail
.abort:
;Prepare to abort. We abort from within!
;If a network request requests abort, translate to fail
    cmp byte [dosInvoke], -1
    jne .kill   ;If this is zero, local invokation
    mov byte [Int44Trans], -1   ;We are translating a Abort to Fail. Mark it
    jmp short .exit
.kill:
    mov word [errorExCde], di ;Save the error code if Abort
    mov eax, edi    ;Make the return error code the Driver Error Code
    mov byte [exitType], 2    ;We are returning from Abort, ret type 2!
    jmp terminateClean.altEP

ctrlBreakHdlr:
    mov al, 03h ;Always guarantee a ^C will be printed
    call printCaretASCII
    call printCRLF
    ;Reset the console back to 0
    mov byte [vConDrvSwp],  0   ;Set to 0
;Handles a control break, juggles stacks and enters int 41h 
.exceptEP:
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

;CPU Exception handlers
;If a CPU exception occurs, and inDOS = 0, the default behaviour will be to 
; CTRL+C exit.
; If inDOS > 0, then we assume the error is within DOS and thus we halt the
; system.
; Also halt if the application is it's own parent.
;An application should hook these interupts if they wish to not 
; CTRL+C exit, and instead return to DOS.
i0:
    xor eax, eax
    jmp cpu_2args
i1:
    mov eax, 1
    jmp cpu_2args
i2:
    mov eax, 2
    jmp cpu_2args
i3:
    mov eax, 3
    jmp cpu_2args
i4:
    mov eax, 4
    jmp cpu_2args
i5:
    mov eax, 5
    jmp short cpu_2args
i6:
    mov eax, 6
    jmp short cpu_2args
i7:
    mov eax, 7
    jmp short cpu_2args
i8:
    mov eax, 8
    jmp short cpu_3args
i9:
    mov eax, 9
    jmp short cpu_2args
i10:
    mov eax, 0Ah
    jmp short cpu_3args
i11:
    mov eax, 0Bh
    jmp short cpu_3args
i12:
    mov eax, 0Ch
    jmp short cpu_3args
i13:
    mov eax, 0Dh
    jmp short cpu_3args
i14:
    mov eax, 0Eh
    jmp short cpu_4args
i15:
    mov eax, 0Fh
    jmp short cpu_2args
i16:
    mov eax, 10h
    jmp short cpu_2args
i17:
    mov eax, 11h
    jmp short cpu_3args
i18:
    mov eax, 12h
    jmp short cpu_2args
i19:
    mov eax, 13h
    jmp short cpu_2args
i20:
    mov eax, 14h
    jmp short cpu_2args
i21:
    mov eax, 15h
cpu_4args:
    mov ecx, 3
    jmp short cpu_exception
cpu_3args:
    mov ecx, 2
    jmp short cpu_exception
cpu_2args:
    mov ecx, 1
cpu_exception:
;Enter with:
; eax = Exception number in binary
; ecx = Number of arguments to print and pop from stack - 1 
    lea rsi, .fatalt0   ;Get the ptr
    mov ebx, fatalt0L  ;Get the length
    call .writeExceptionMessage

    cmp byte [inDOS], 1
    jae .introStop
    cmp eax, 2
    je .introStop

    mov rbx, qword [currentPSP] ;If a command shell craps out, Halt
    cmp rbx, qword [rbx + psp.parentPtr]
    je .introStop

    lea rsi, .fatal1
    mov ebx, fatal1L
    call .writeExceptionMessage
    jmp short .introEnd
.introStop:
    mov byte [haltDOS], -1
    lea rsi, .fatalHalt   ;Get the ptr
    mov ebx, fatalHaltL  ;Get the length
    call .writeExceptionMessage
.introEnd:
    lea rdi, byteBuffer
    call .printbyte ;Store the error code in the byte buffer
    lea rsi, byteBuffer
    mov ebx, 2  ;Print the two nybbles
    call .writeExceptionMessage

    cmp cl, 1
    ja .cpuextendederror    ;rax contains error code, or extra cr2 value
.cpurollprint:
    lea rdi, byteBuffer
    mov rdx, qword [rsp]    ;Get address
;Takes whats in rdx, rols left by one byte, prints al
    mov cl, 8    ;8 bytes
.cpurollprint1:
    rol rdx, 8
    mov al, dl
    push rdx
    call .printbyte
    pop rdx
    dec cl
    jnz .cpurollprint1

    mov ebx, 16 ;Print the 16 nybbles
    lea rsi, byteBuffer
    call .writeExceptionMessage

    mov ebx, crlfL
    lea rsi, .crlf
    call .writeExceptionMessage    

    test byte [haltDOS], -1
    jnz .fatalStop
    call .readInputChar
    jmp ctrlBreakHdlr.exceptEP ;Jump to CTRL+C out (without a ^C printed)
.fatalStop:
;This is called if inDOS > 1 or NMI occured
;Permanently locks up the system by turning off interrupts and infinite looping.
    call dosCrit2Enter  ;Suspend multitasking now
.fatalLp:
    cli
    hlt
    jmp short .fatalLp

.cpuextendederror:
    lea rsi, .fatal2
    mov ebx, fatal2L  ;Print the colon string
    call .writeExceptionMessage

    pop rdx
    dec rcx
    push rcx
    lea rdi, byteBuffer
    mov cl, 2    ;CAN CHANGE TO 4 BYTES IN THE FUTURE
    xchg dl, dh   
.pr1:
    mov al, dl
    push rdx
    call .printbyte
    pop rdx
    ror edx, 8    ;Print just dx for now
    dec cl
    jnz .pr1

    lea rsi, byteBuffer
    mov ebx, 4  ;Print four nybbles
    call .writeExceptionMessage

    lea rsi, .fatal2
    mov ebx, fatal2L  ;Print the colon string
    call .writeExceptionMessage

    pop rcx    ;Bring the comparison value back into rcx
    dec rcx
    jz .cpurollprint

    mov cl, 8   ;16 nybbles
    mov rdx, cr2    ;Get page fault address
    lea rdi, byteBuffer
.pr2:
    rol rdx, 8    ;Print rdx
    mov al, dl
    push rdx
    call .printbyte
    pop rdx
    dec cl
    jnz .pr2

    lea rsi, byteBuffer
    mov ebx, 16
    call .writeExceptionMessage

    lea rsi, .fatal2
    mov ebx, fatal2L  ;Print the colon string
    call .writeExceptionMessage

    jmp .cpurollprint


.char:    ;Print a single character
    lea rbx, .ascii
    xlatb    ;point al to entry in ascii table, using al as offset into table
    stosb   ;Store the byte in the buffer and inc rdi
    ret
.printbyte:
    mov dl, al            ;save byte in dl
    and ax, 00F0h        ;Hi nybble
    and dx, 000Fh        ;Lo nybble
    shr ax, 4            ;shift one hex place value pos right
    call .char
    mov ax, dx            ;mov lo nybble, to print
    call .char
    ret        


.readInputChar:
    mov byte [critReqHdr + ioReqPkt.cmdcde], drvREAD    ;Wait for a char!
    lea rsi, singleIObyt
    mov ebx, 1  ;Read one char
    jmp short .ioException
.writeExceptionMessage:
;Called with ebx=Number of chars to print, rsi -> String to print
    mov byte [critReqHdr + ioReqPkt.cmdcde], drvWRITE
.ioException:
    mov byte [critReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov word [critReqHdr + ioReqPkt.status], 0
    mov dword [critReqHdr + ioReqPkt.tfrlen], ebx
    lea rbx, critReqHdr
    mov qword [critReqHdr + ioReqPkt.bufptr], rsi
    mov rsi, qword [vConPtr]
    call goDriver
    return


;Error messages
.ascii:    db '0123456789ABCDEF'
.fatalt0:  db CR,LF,LF,"SCP/DOS EXCEPTION DETECTED!",CR,LF,LF
fatalt0L   equ $ - .fatalt0

.fatal1:   
    db "A potentially fatal error has been detected and the current application"
    db " will be terminated.",CR,LF,
    db "Press any key to continue or CTRL+ALT+DEL to restart the machine."
    db CR,LF,"SYSTEM ERROR: "   ;Print standard Error message here
fatal1L equ $ - .fatal1

.fatal2:   db " : "
fatal2L    equ $ - .fatal2

;The below error is displayed is inDOS > 1 or NMI occured
.fatalHalt: db "SCP/DOS SYSTEM STOP: "
fatalHaltL equ $ - .fatalHalt
.crlf:  db CR,LF,LF
crlfL  equ $ - .crlf