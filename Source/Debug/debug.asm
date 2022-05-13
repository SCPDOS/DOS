;This file contains debugging macros, procs and strings
; that we can use to output to COM1 bypassing the operating system.
;A serial terminal must be connected for this to work.

;Variables and equates

debOldRSP   dq 0    ;The RSP value when entering a debug output block
debDigitStringLen equ 20
debDigitString db debDigitStringLen dup(0)
debascii: db '0123456789ABCDEF'
;Common procedures

debPrintHexByte:
;Print the hex byte in al as a hex value
    push rdx
    push rbx
    push rax

    mov dl, al           ;save byte in dl
    and ax, 00F0h        ;Hi nybble
    and dx, 000Fh        ;Lo nybble
    shr ax, 4            ;shift one hex place value pos right
    call .wrchar
    mov ax, dx           ;mov lo nybble, to print
    call .wrchar

    pop rax
    pop rbx
    pop rdx
    ret
.wrchar:
    xchg bx, bx
    lea rbx, debascii
    xlatb    ;point al to entry in ascii table, using al as offset into table
    mov ah, 01h
    int 34h  ;print char
    ret

debPrintHexQword:
;Print the hexadecimal qword in rax as a hex value
    push rax
    push rcx
    mov ecx, 8
.printChar:
    call debPrintHexByte
    shr rax, 8
    loop .printChar
    pop rcx
    pop rax
    ret
debPrintDecQword:
;Print the hexadecimal qword in rax in decimal
    push rax
    push rbx
    lea rdi, debDigitString   ;Use the default line as a buffer
    ;Sanitise the digit buffer
    push rdi
    push rcx
    push rax
    xor eax, eax
    mov ecx, debDigitStringLen/8
    rep stosq
    pop rax
    pop rcx
    pop rdi

    add rdi, debDigitStringLen - 1 ;Go to the end of the buffer
    std ;Reverse string ops
    push rax
    xor al, al  ;Place delimiter
    stosb
    pop rax
    mov rbx, 0Ah  ;Divide by 10
.pdw0:
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .pdw1
    add dl, 'A'-'0'-10
.pdw1:
    push rax
    mov al, dl    ;Save remainder byte
    stosb   ;Store the byte and add one to rdi
    pop rax
    test rax, rax
    jnz .pdw0
    cld ;Return string ops to normal
    inc rdi ;Skip the extra 0 that was inserted
    mov rbp, rdi    ;Point rbp to the head of the string
    call debPrintNullString
    pop rbx
    pop rax
    ret

debPrintNullString:
;Print a null terminated string pointed to by rbp
    push rsi
    push rax
    push rdx
    xor edx, edx    ;Select COM1
    mov rsi, rbp
.getChar:
    lodsb
    test al, al
    jz .exit
    mov ah, 01h
    int 34h
    jmp short .getChar
.exit:
    pop rdx
    pop rax
    pop rsi
    ret

debPrintDOSStack:
;Function that shows me the state on entering and exiting a DOS kernel function
;Shows me which function, and parameters passed and returned
    lea rbp, .a1
    call debPrintNullString
    mov rdx, qword [oldRSP] ;Get caller RSP value
    lea rbx, .a0
    mov rax, qword [rdx + callerFrame.rax]
    call .storeQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rbx]
    call .storeQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rcx]
    call .storeQword
    add rbx, 22 + 2  ;Skip crlf
    mov rax, qword [rdx + callerFrame.rdx]
    call .storeQword
    add rbx, 22 
    mov rax, qword [rdx + callerFrame.rsi]
    call .storeQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rdi]
    call .storeQword
    add rbx, 22 + 2 ;Skip crlf
    mov rax, qword [rdx + callerFrame.rbp]
    call .storeQword
    add rbx, 22
    mov rax, qword [oldRSP]
    add rax, callerFrame_size
    call .storeQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rip]
    call .storeQword
    add rbx, 22 + 2 ;Skip crlf
    mov rax, qword [rdx + callerFrame.flags]
    call .storeQword
    lea rbp, .a0
    call debPrintNullString
    ret
.storeQword:
    ;Called with number in rax
    ;table in rbx
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, rbx
    mov rdx, rax
    add rbp, 4 + 15 ;Go to end of number
    mov ecx, 8 ;16 digits, 2 at a time
    lea rbx, debascii
.sq0:
    mov al, dl  ;Go low nybble first
    and al, 0Fh
    xlatb
    mov byte [rbp], al
    dec rbp ;Go down one char pos
    mov al, dl
    and al, 0F0h    ;Hi nybble next
    shr al, 4   ;Shift hi nybble low
    xlatb
    mov byte [rbp], al  ;Store char
    shr rdx, 8  ;Get next digit from rdx
    dec rbp
    dec ecx
    jnz .sq0
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    ret

.a0 db "rax=0000000000000000h " ;each line is 22 chars long
    db "rbx=0000000000000000h "
    db "rcx=0000000000000000h "
    db  0Ah,0Dh
    db "rdx=0000000000000000h "
    db "rsi=0000000000000000h "
    db "rdi=0000000000000000h "
    db 0Ah, 0Dh
    db "rbp=0000000000000000h "
    db "rsp=0000000000000000h "
    db "rip=0000000000000000h"
    db 0Ah, 0Dh
    db "flg=0000000000000000h "
    db 0Ah,0Dh,0
.a1 db "Registers on Int 41h stack",0Ah,0Dh,0

debPrintFunctionName:
;Prints the DOS function that has been entered
;Entered with ah = Function number
    mov al, ah  ;Get dos function number
    lea rbx, debascii
    lea rdx, .number
    and al, 0F0h    ;Get high nybble first
    shr al, 4
    xlatb
    mov byte [rdx], al
    mov al, ah
    and al, 0Fh
    xlatb
    mov byte [rdx + 1], al
    lea rbp, .dosString
    call debPrintNullString
    ret
.dosString db "Calling DOS function Int 41h/AH="
.number db "00h",0Ah,0Dh,0

;----------------:
;!!!! MACROS !!!!:
;----------------:
;Insert macro when entering a debug block
%macro debugEnterM 0
    mov qword [debOldRSP], rsp  ;Save rsp
    mov rsp, debStackTop
;Push all registers except rsp on stack
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15
    pushfq
%endmacro
;Insert macro when exiting a debug block
%macro debugExitM 0
    popfq
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    mov rsp, qword [debOldRSP]  ;Return original stack pointer
%endmacro