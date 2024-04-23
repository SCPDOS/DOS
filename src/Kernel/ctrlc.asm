
diskIOError:
;Called in Binary Disk Read/Write if getting access to shared resource fails
;Input: rwFlag = 0 or 1 for read/write
;       eax = Status word
;       rdi -> disk pointer
;       rbp -> DPB ptr
    cmp al, drvBadDskChnge
    jne .doReq
    push rax    ;If a bad disk change, drop the volume label ptr here
    mov rax, qword [primReqHdr + ioReqPkt.desptr]   ;Get volume label ptr
    mov qword [errorVolLbl], rax    ;and save it!
    ;Later versions will include a serial number after the lbl too
    pop rax
.doReq:
    call diskDevErr ;Preserves the disk pointer
    return
xlatHardError:
;Translates a hard error code to a generic DOS error
;Input: edi = Hard Error Code
;       ah = Bitfield
;       al = Failing drive number
    push rax    ;Wanna preserve ax
    cmp di, hardXlatTblL    ;If errorcode > 15, do not adjust!!
    movzx eax, di  ;Clears 64 bits and moves error code into ax
    jae .skipXlat   ;Skip xlat if above 15, for IOCTL return errors
    push rbx
    lea rbx, hardXlatTbl
    xlatb    ;Get translated byte from the table in al
    pop rbx
.skipXlat:
    mov word [errorExCde], ax   ;Store this error code here
    pop rax
    push rsi
    lea rsi, extErrTbl
    call setErrorVars
    pop rsi
    return

charDevErr:
;Hard character device errors come here
;Input:
; ah = Additional Int 24h flags. Top bit should be set!
;edi = error code in low byte
;rbp -> Not accessed but preserved
    or ah, critIgnorOK | critRetryOK | critFailOK   ;Set the always bits
    mov byte [Int24bitfld], ah
    mov qword [tmpDPBPtr], rbp
    push rsi
    movzx edi, dil    ;Zero extend the error code up
    call hardErrorCommon
    pop rsi
    return
diskDevErr:
;Called, NOT Jumped to. 
;Input: rdi = Disk Buffer pointer (or 0 to mean share)
;       eax = Status word (error code in al)
;       rbp = Disk DPB pointer
; [Int24hbitfld] = Specific bitflags (r/w AND potential extra ok responses)
;Output: al = Int 24h response (0-3)
; All other registers preserved
    mov bl, dataBuffer  ;Set dflt flags for invoke
    test rdi, rdi       ;Is this a share invokation?
    je .skipbufferread  ;Jump if so, since share lock issues occur on data io
    mov bl, byte [rdi + bufferHdr.bufferFlags]  ;Else get the buffer data type
.skipbufferread:
    push rdi        ;Save the buffer pointer
    movzx edi, al   ;Store status code in dil, zero extend
    cmp edi, drvWPErr
    jne .notReset
    ;Reset the error drive to report dpb drive if a write protect error!
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov byte [errorDrv], al ;Store this value
.notReset:
    mov ah, byte [Int24bitfld]  ;Get the permissions in var
    or ah, critFailOK | critRetryOK ;Set the always bits
    ;Test for correct buffer data type
    test bl, dosBuffer
    jnz .df0
    or ah, critDOS  ;Add DOS data type bit
    jmp short .df3
.df0:
    test bl, fatBuffer
    jnz .df1
    or ah, critFAT  ;Add FAT data type bit
    mov dword [rbp + dpb.dNumberOfFreeClusters], -1 ;Invalidate the count!
    jmp short .df3
.df1:
    test bl, dirBuffer
    jnz .df2
    or ah, critDir  ;Add Directory data type bit
    jmp short .df3
.df2:
    or ah, critData ;Here it must be a data buffer
.df3:
    and byte [rwFlag], 1    ;Save only the bottom bit
    or ah, byte [rwFlag]    ;And set the low bit here
    or ah, byte [Int24bitfld]
    ;Continue down with failing disk buffer pointer on stack
    call diskDevErrBitfield
    pop rdi ;Pop back the disk buffer pointer
    return   
diskDevErrBitfield:
;Called with Int24Bitfield constructed and in ah and error code in dil
;This is to avoid rebuilding the bitfield.
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get the drive number
    mov qword [tmpDPBPtr], rbp  ;Save the DPB 
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;And get the driver ptr in rsi
    xor ebp, ebp    ;Finally, set ebp to 0 to simulate the segment
hardErrorCommon:
;The common fields, with the vars set up. 
;Ensure we dont have a crazy error code.
;Entered with: ah = bitfield, al = Fail drive (0 based) if not char
; dil = Driver error code, rsi -> Driver header
;tmpDBPPtr = Fail DPB if not char, rwFlag set/clear
    call xlatHardError
    push rax
    mov eax, errGF - drvErrShft
    cmp edi, eax    ; If the returned error code is above largest driver code
    cmova edi, eax  ; return the driver largest code
    pop rax
criticalDOSError:   ;Int 2Fh, AX=1206h, Invoke Critical Error Function 
;Will swap stacks and enter int 24h safely and handle passing the right data 
; to the critical error handler.
; Called with rsi set as required by Int 24h (caller decides), ax, di
; and with Int24Bitfield set
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
; Return response from int 24h in al
; Caller must preserve rsp, rbx, rcx, rdx if they wish to return to DOS
; This function will terminate the program if an abort was requested!
; This function also destroys RBP
    cmp byte [critErrFlag], 1
    jb .noIntError  ;If not 0, enter
    mov al, critFail    ;Else, return Fail always
    jmp short .exit     ;Don't translate fail to abort
.noIntError:
    mov qword [xInt24hRSP], rsp ;Save our critical error stack
    cmp word  [currentNdx], -1  ;If this is -1, we are not opening a file
    je .notOpeningFile
    push rdi
    mov rdi, qword [curHdlPtr]  ;Get the pointer to the current handle entry
    mov byte [rdi], -1          ;Free this handle
    pop rdi
.notOpeningFile:
    cli                         
    inc byte [critErrFlag]      ;Set flag for critical error
    dec byte [inDOS]            ;Exiting DOS
    mov rsp, qword [oldRSP]     ;Get the stack ptr after regs were pushed
    xor ebp, ebp                ;Always zeroed for DOS portability!
    int 24h                     ;Call crit. err. hdlr. Ints reset on
    mov qword [oldRSP], rsp     ;Allows user to change reg vals on fail!
    mov rsp, qword [xInt24hRSP] ;Ret to DOS stack for failing device
    mov byte [critErrFlag], 0   ;Clear critical error flag
    inc byte [inDOS]            ;Reenter DOS
    mov rbp, qword [tmpDPBPtr]
    sti                         
    ;Now we check that the response given was allowed, and translate if needed
.checkResponse:
    cmp al, critIgnore
    je .checkIgnore
    cmp al, critRetry
    je .checkRetry
    cmp al, critFail
    jne .abort   ;Must be abort
.setFail:   ;Here is for fail
    mov al, critFail    ;Reset al to contain fail (even if Int24 responded Fail)
    inc byte [Int24Fail]        ;Inc the fail counter!
    test byte [Int24bitfld], critFailOK
    jz .abort  ;If bit not set, fail not permitted, abort
.exit:
    mov byte [errorDrv], -1 ;Unknown drive (to be set)
    cmp byte [currentNdx], -1   ;Is there a file that needs handling?
    rete    ;Only if this is not equal
    ;In that case, we set the jft entry to its initial value, whatever it was
    push rax
    push rdi
    movzx eax, word [currentNdx]
    mov rdi, qword [curHdlPtr]
    mov byte [rdi], al
    pop rdi
    pop rax
    return
.checkIgnore:
    test byte [Int24bitfld], critIgnorOK
    jnz .exit
    jmp short .setFail  ;If ignore not permitted, return Fail
.checkRetry:
    test byte [Int24bitfld], critRetryOK
    jnz .exit   
    jmp short .setFail  ;If retry not permitted, return Fail
.abort:
;Prepare to abort. We abort from within!
    call vConRetDriver  ;Always reset the driver flag on abort
;If a network request requests abort, translate to fail
    cmp byte [dosInvoke], -1
    jne .kill   ;If this is zero, local invokation
    mov byte [Int24Trans], -1   ;We are translating a Abort to Fail. Mark it
    jmp short .exit
.kill:
    add di, drvErrShft
    mov word [errorExCde], di ;Save the error code if Abort
    mov eax, edi    ;Make the return error code the Driver Error Code
    mov byte [exitType], 2      ;We are returning from Abort, ret type 2!
    mov byte [volIdFlag], 0     ;Clear special vol search byte if set
    ;Before returning, we need to set the aborting psp.rspPtr back to 
    ; the oldRSP as a syscall during Int 24h would change this value.
    ;As a result, the stack frame pointed to would not have a valid SS:RSP
    ; for the IRETQ at the end of EXIT which would lead to a GP Fault.
    ;oldRSP remains unchanged in Int24h even if a syscall made, though may 
    ; point to a new stack as a result of the programmer of Int 24h changing 
    ; the stack to a new stack of vars. This new stack MUST have a valid 
    ; CS:RIP, RFLAGS and SS:RSP set at the end, otherwise EXIT could again 
    ; lead to a GP Fault.
    mov rdi, qword [currentPSP]
    mov rbx, qword [oldRSP]
    mov qword [rdi + psp.rspPtr], rbx
    jmp terminateClean.altEP

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
    mov qword [oldRAX], rax ;Save rax
    pushfq  ;Get flags in rax
    pop rax 
    cmp rsp, qword [xInt23hRSP] ;Did the user return with ret 8?
    jne .checkCF
.returnToDOS:
    mov rax, qword [oldRAX]
    jmp functionDispatch    ;Goto int 21h
.checkCF:
    mov rsp, qword [xInt23hRSP]  ;Account for the flags and SS:RSP left on stack
    test al, 1  ;CF set?
    jz .returnToDOS ;If yes, subfunction number must be in al
    mov eax, 4c00h  ;Exit without error code
    mov byte [ctrlCExit], -1  ;CTRL+BREAK termination
    jmp functionDispatch ;When jumping now, rsp will go back into psp.rsp!

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

    lea rsi, .fatal2
    mov ebx, fatal2L  ;Print the colon string
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
    ;Set error code to General Failure
    mov word [errorExCde], errGF
    mov byte [errorAction], eActAbt
    mov byte [errorClass], eClsAppFlt
    mov byte [errorLocus], eLocUnk
    mov eax, 4cFFh
    mov byte [ctrlCExit], -1
    ;If a -1 error code and ctrlC exit and the extended error
    ; setup as above, chances are it was a CPU error
    jmp functionDispatch    ;Call Int 21h politely, clean up resources
.fatalStop:
;This is called if inDOS > 1 or NMI occured
;Waits 1 minute then reboots
    mov eax, 8200h  ;Exit all critical sections
    int 2Ah
    call dosCrit1Enter  ;Get the lock to internal DOS structures
    call dosCrit2Enter  ;Get the lock to end all multitasking
    call getDateAndTimeOld  ;Get time packed in edx (edx[0:4] = Seconds/2)
    mov ebx, edx
    and ebx, 1Fh    ;Save the relevent bits
.loopForNextSecond:
    call .getTimeDateCompare
    je .loopForNextSecond
.loopTillTimeElapsed:
    call .getTimeDateCompare
    jne .loopTillTimeElapsed
    ;Now we triple fault
    lidt [.resetIDT] ;Triple fault the machine
    jmp short .toHell
.toHell:
    int 00h ;Call div by 0 to trigger reboot if not somehow failed yet
    jmp short .toHell
.resetIDT:
    dw 0
    dq 0
.getTimeDateCompare:
    push rbx
    call getDateAndTimeOld
    pop rbx
    and edx, 1Fh
    cmp edx, ebx
    return

.cpuextendederror:
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
.fatalt0:  db CR,LF,LF,"        SCP/DOS EXCEPTION DETECTED!",CR,LF,LF
fatalt0L   equ $ - .fatalt0

.fatal1:   
    db "    A potentially fatal error has been detected",CR,LF
    db "    and the current application must be terminated.",CR,LF,LF
    db "    Press any key to continue or CTRL+ALT+DEL to restart the machine."
    db CR,LF, LF,"    SYSTEM ERROR: "   ;Print standard Error message here
fatal1L equ $ - .fatal1

.fatal2:   db " : "
fatal2L    equ $ - .fatal2

;The below error is displayed is inDOS > 1 or NMI occured
.fatalHalt: db "    SCP/DOS SYSTEM STOP: "
fatalHaltL equ $ - .fatalHalt
.crlf:  db CR,LF,LF
crlfL  equ $ - .crlf