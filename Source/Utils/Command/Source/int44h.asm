critErrorHandler:   ;Int 44h
;User Stack in usage here, must be swapped to before this is called
;Entered with:  
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
    push rbx
    push rcx
    push rdx
    push rdi
    push rsi
    cld         ;Make String ops go forward

    mov bx, ax  ;Save ah in bh and al in bl (if needed)
    lea rdx, crlf
    mov ah, 09h ;Print String
    int 41h

    and edi, 00FFh   ;Zero the upper bytes of DI just in case
    mov ecx, 0Ch
    cmp edi, ecx  ;Check if the error number is erroniously above Gen Error
    cmova edi, ecx  ;If it is, move Gen Error into edi
    movzx rdi, di
    mov rdx, rdi    ;Copy error code
    shl rdi, 4  ;Multiply by 16
    shl rdx, 1  ;Multiply by 2
    add rdi, rdx    ;Add the resultant multiplications
    lea rdx, qword [.errorMsgTable]
    lea rdx, qword [rdx+rdi]   ;Load EA to rdx
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print first part of message

    lea rdx, qword [.readmsg]
    lea rdi, qword [.writemsg]
    test bh, 1  ;Bit 0 is set if write operation
    cmovnz rdx, rdi ;Move the correct r/w part of the message to rdx
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print error reading/writing portion

    test bh, 80h    ;Test bit 7 for char/Disk assertation
    jnz .charError
;Disk error continues here
    lea rdx, qword [.drive] ;Drive message
    mov ah, 09h
    int 41h
    mov dl, bl  ;Get zero based drive number into dl
    add dl, "A" ;Add ASCII code
    mov ah, 02h ;Print char in dl
    int 41h
.userInput:
    lea rdx, crlf  ;Print new line
    mov ah, 09h
    int 41h
;Abort, Retry, Ignore, Fail is word order
;Last message gets a ?, otherwise a comma followed by a 20h (space)
.userAbort:
;Abort is always an option
    lea rdx, qword [.abortmsg]
    mov ah, 09h
    int 41h ;Call DOS to prompt user for ABORT option
.userRetry:
    test bh, 10h  ;Bit 4 is retry bit
    jz .userIgnore    ;If clear, dont print message
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.retrymsg]
    mov ah, 09h
    int 41h
.userIgnore:
    test bh, 20h    ;Bit 5 is ignore bit
    jz .userFail
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.ignoremsg]
    mov ah, 09h
    int 41h
.userFail:
    test bh, 08h    ;Bit 3 is Fail bit
    jz .userMsgEnd
    lea rdx, qword [.betweenMsg]
    mov ah, 09h
    int 41h
    lea rdx, qword [.failmsg]
    mov ah, 09h
    int 41h
.userMsgEnd:
    lea rdx, qword [.endMsg]
    mov ah, 09h
    int 41h
;Get user input now 
    xor ecx, ecx  ;4 Possible Responses
    lea rdi, qword [.responses] ;Go to start of string
    mov ah, 01h ;STDIN without Console Echo
    int 41h ;Get char in al
    cmp al, "a" ;Chack if lowercase
    jb .uip1    ;If the value is below, ignore subtraction
    sub al, "a"-"A"  ;Turn the char into uppercase
.uip1:
    scasb   ;Compare char to list, offset gives return code
    je .validInput  ;If they are equal, ecx has return code
    inc ecx
    cmp ecx, 4
    jne .uip1
    jmp .userInput ;If valid char not found, keep waiting 
.validInput:
    mov al, cl  ;Move the offset into .responses into al
;Now check if the input is permitted
    cmp al, 2   ;Check if abort, abort always permitted
    je .cehExit
    test al, al ;Check if 0 => Ignore
    je .viIgnore
    cmp al, 1   ;Check if 1 => Retry
    je .viRetry
.viFail:    ;Fallthrough for fail (al = 3)
    test bh, 8  ;Bit 3 is Fail bit
    jz .userInput  ;If bit 3 is zero, prompt and get input again
    jmp short .cehExit
.viIgnore:
    test bh, 20h    ;Bit 5 is Ignore bit
    jz .userInput
    jmp short .cehExit
.viRetry:
    test bh, 10h    ;Bit 4 is Retry bit
    jz .userInput
.cehExit:
    pop rsi
    pop rdi
    pop rdx
    pop rcx
    pop rbx
    iretq
.charError:
    mov ecx, 8  ;8 chars in device name
    add rsi, drvHdr.drvNam  ;Get the address of the Drive name
.ce1:
    lodsb   ;Get a string char into al and inc rsi
    mov dl, al  ;Move char into dl
    mov ah, 02h
    int 41h ;Print char
    loop .ce1   ;Keep looping until all 8 char device chars have been printed
    jmp .userInput

.errorMsgTable: ;Each table entry is 18 chars long
            db "Write Protect $   "       ;Error 0
            db "Unknown Unit $    "       ;Error 1
            db "Not Ready $       "       ;Error 2
            db "Unknown Command $ "       ;Error 3
            db "Data $            "       ;Error 4
            db "Bad Request $     "       ;Error 5
            db "Seek $            "       ;Error 6
            db "Unknown Media $   "       ;Error 7
            db "Sector Not Found $"       ;Error 8
            db "Out Of Paper $    "       ;Error 9
            db "Write Fault $     "       ;Error A
            db "Read Fault $      "       ;Error B
            db "General Failure $ "       ;Error C

.drive      db "drive $"
.readmsg    db "error reading $"
.writemsg   db "error writing $"
.abortmsg   db "Abort$" 
.ignoremsg  db "Ignore$"
.retrymsg   db "Retry$"
.failmsg    db "Fail$"
.betweenMsg db ", $"
.endMsg     db "? $"
.responses  db "IRAF"   ;Abort Retry Ignore Fail

int43h:
    test byte [permaSwitch], -1
    jnz .exit   ;If this is non-zero, just exit as normal
    ;Else, we juggle parent PSP's
    push rax
    push rbx
    mov rax, qword [realParent]
    mov rbx, qword [pspPtr]
    mov qword [rbx + psp.parentPtr], rax    ;Store the parent there
    pop rbx
    pop rax
.exit:
    stc     ;Set CF to kill the task
    ret 8   ;Return and pop CS off the stack to indicate we wanna kill task