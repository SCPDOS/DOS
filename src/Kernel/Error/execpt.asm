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

    mov byte [haltDOS], 0   ;Clear first as we are recycling this byte in SDA
    cmp byte [inDOS], 1     ;Are we in DOS? 
    jae .introStop          ;Crap out, cant guarantee DOS is stable anymore
    cmp eax, 2              ;NMI?
    je .introStop           ;Freeze the PC to stop it from hurting itself...

    mov rbx, qword [currentPSP] ;If self-parent craps out... 
    cmp rbx, qword [rbx + psp.parentPtr] ;Who do we call? COMSPEC?
    je .introStop   ;Nah, for now, just hard stop like if DOS is bad.

    lea rsi, .fatal1
    mov ebx, fatal1L
    call .writeExceptionMessage
    jmp short .introEnd
.introStop:
    mov byte [haltDOS], -1  ;Set crap out byte...
    lea rsi, .fatalHalt     ;Get the ptr
    mov ebx, fatalHaltL     ;Get the length
    call .writeExceptionMessage
.introEnd:
    lea rdi, extErrByteBuf
    call .printbyte ;Store the error code in the byte buffer
    lea rsi, extErrByteBuf
    mov ebx, 2  ;Print the two nybbles
    call .writeExceptionMessage

    lea rsi, .fatal2
    mov ebx, fatal2L  ;Print the colon string
    call .writeExceptionMessage

    cmp cl, 1
    ja .cpuextendederror    ;rax contains error code, or extra cr2 value
.cpurollprint:
    lea rdi, extErrByteBuf
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
    lea rsi, extErrByteBuf
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
    ;If a errGF error code and ctrlC exit and the extended error
    ; setup as above, chances are it was a CPU error
    jmp functionDispatch    ;Call Int 21h politely, clean up resources
.fatalStop:
;This is called if inDOS > 1 or NMI occured
;Freezes the machine
    mov eax, 8200h  ;Exit all critical sections
    int 2Ah
    call dosCrit1Enter  ;Get the lock to internal DOS structures
    call dosCrit2Enter  ;Get the lock to drivers
    cli                 ;Halt all interrupts
.fatalLp:
    pause
    jmp short .fatalLp  ;Stay here forever, we cant guarantee anything anymore!
.cpuextendederror:
    pop rdx
    dec rcx
    push rcx
    lea rdi, extErrByteBuf
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

    lea rsi, extErrByteBuf
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
    lea rdi, extErrByteBuf
.pr2:
    rol rdx, 8    ;Print rdx
    mov al, dl
    push rdx
    call .printbyte
    pop rdx
    dec cl
    jnz .pr2

    lea rsi, extErrByteBuf
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
    mov byte [critReqPkt + ioReqPkt.cmdcde], drvREAD    ;Wait for a char!
    lea rsi, singleIObyt
    mov ebx, 1  ;Read one char
    jmp short .ioException
.writeExceptionMessage:
;Called with ebx=Number of chars to print, rsi -> String to print
    mov byte [critReqPkt + ioReqPkt.cmdcde], drvWRITE
.ioException:
    mov byte [critReqPkt + ioReqPkt.hdrlen], ioReqPkt_size
    mov word [critReqPkt + ioReqPkt.status], 0
    mov dword [critReqPkt + ioReqPkt.tfrlen], ebx
    lea rbx, critReqPkt
    mov qword [critReqPkt + ioReqPkt.bufptr], rsi
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