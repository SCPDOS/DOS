;This file contains debugging macros, procs and strings
; that we can use to output to COM1 bypassing the operating system.
;A serial terminal must be connected for this to work.

;Variables and equates

debOldRSP   dq 0    ;The RSP value when entering a debug output block

debascii: db '0123456789ABCDEF'
;Common procedures
debMakeDebuggerRespond:
    lea rbp, .msg
    call debPrintNullString
    mov ecx, 100000h
.waiting:
    dec ecx
    jz .timeout
    mov ah, 02h ;Blocking recieve!
    xor edx, edx
    int 34h
    test ah, 80h    ;Was no char recieved? Keep waiting
    jnz .waiting
    ret
.timeout:
    lea rbp, .msg2
    jmp debPrintNullString
.msg: db "Strike a key at the terminal to continue or wait for timeout...",0Ah,0Dh,0
.msg2: db "Timeout. Continuing...",0Ah,0Dh,0
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
    lea rbx, qword [.a0 + 4]    ;Go past the rax= part
    mov rax, qword [rdx + callerFrame.rax]
    call overlayQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rbx]
    call overlayQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rcx]
    call overlayQword
    add rbx, 22 + 2  ;Skip crlf
    mov rax, qword [rdx + callerFrame.rdx]
    call overlayQword
    add rbx, 22 
    mov rax, qword [rdx + callerFrame.rsi]
    call overlayQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rdi]
    call overlayQword
    add rbx, 22 + 2 ;Skip crlf
    mov rax, qword [rdx + callerFrame.rbp]
    call overlayQword
    add rbx, 22
    mov rax, qword [oldRSP]
    add rax, callerFrame_size
    call overlayQword
    add rbx, 22
    mov rax, qword [rdx + callerFrame.rip]
    call overlayQword
    add rbx, 22 + 2 ;Skip crlf
    mov rax, qword [rdx + callerFrame.flags]
    call overlayQword
    lea rbp, .a0
    call debPrintNullString
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
.a1 db "Registers on Int 21h stack",0Ah,0Dh,0

debDPBptr:
    ;rbp has dpb pointer in it or if -1, no dpb
    lea rbx, qword [.dpb + 10]   ;Goto first number
    mov rax, rbp
    call overlayQword
    lea rbp, .dpb
    call debPrintNullString
    ret
.dpb: 
    db "DPB ptr @ 0000000000000000h ",0Ah,0Dh,0

debDPBBPBptr:
    ;rbp has dpb ptr in it or -1 if no dpb
    ;rsi has bpb ptr in it or -1 if no bpb
    cmp rbp, -1
    je .baddpb
    cmp rsi, -1
    je .badpbp
    lea rbx, qword [.dpb + 10]   ;Goto first number
    mov rax, rbp
    call overlayQword
    add rbx, 33
    mov rax, rsi
    call overlayQword
    mov al, byte [rbp + dpb.bDriveNumber]
    add al, "A"
    mov byte [.dpbLetter], al
    lea rbp, .dpb
    call debPrintNullString
    ret
.dpb: 
    db "DPB ptr @ 0000000000000000h from "
    db "BPB ptr @ 0000000000000000h for Drive "
.dpbLetter:
    db 0Ah,0Dh,0
.badpbp:
    lea rbp, .badpbpS
    call debPrintNullString
    ret
.badpbpS:
    db "BAD BPB PTR",CR,LF,0
.baddpb:
    lea rbp, .baddpbS
    call debPrintNullString
    cmp rsi, -1
    je .badpbp
    ret
.baddpbS:
    db "BAD DPB PTR",CR,LF,0

overlayByte:
    ;Called with number in rax
    ;pointer to START of 16 byte space for number in rbx
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, rbx
    mov rdx, rax
    inc rbp ;Go to end of number

    lea rbx, debascii
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

    pop rbp
    pop rdx
    pop rcx
    pop rbx
    ret

overlayWord:
    ;Called with number in rax
    ;pointer to START of 16 byte space for number in rbx
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, rbx
    mov rdx, rax
    add rbp, 3 ;Go to end of number
    mov ecx, 2 ;4 digits, 2 at a time
    lea rbx, debascii
.ow0:
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
    jnz .ow0
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    ret

overlayDword:
    ;Called with number in rax
    ;pointer to START of 16 byte space for number in rbx
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, rbx
    mov rdx, rax
    add rbp, 7 ;Go to end of number
    mov ecx, 4 ;8 digits, 2 at a time
    lea rbx, debascii
.od0:
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
    jnz .od0
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    ret

overlayQword:
    ;Called with number in rax
    ;pointer to START of 16 byte space for number in rbx
    push rbx
    push rcx
    push rdx
    push rbp
    mov rbp, rbx
    mov rdx, rax
    add rbp, 15 ;Go to end of number
    mov ecx, 8 ;16 digits, 2 at a time
    lea rbx, debascii
.oq0:
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
    jnz .oq0
    pop rbp
    pop rdx
    pop rcx
    pop rbx
    ret

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
.dosString db "DOS function Int 21h/AH="
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