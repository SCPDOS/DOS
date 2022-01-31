    
    .x64p

    INCLUDE fatStruc.inc
    INCLUDE dosStruc.inc
    INCLUDE driverStruc.inc
    INCLUDE dosData.inc

loadCode SEGMENT USE64
    ASSUME ds:FLAT, es:FLAT
; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
    dw 0AA55h           ;Initial signature
    xchg bx, bx
    mov byte ptr fs:[dSeg.bootDrive], dl ;Save the boot drive in memory

    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in

    mov qword ptr fs:[dSeg.dataSegPtr], rdi  
    add rdi, SIZEOF dSeg
    mov qword ptr fs:[dSeg.codeSegPtr], rdi
    mov rax, rdi    ;Save the codeSegment address in rax
    lea rsi, OFFSET resCode ;Get offset of the segment in the file into rsi
    mov ecx, 1000h
    rep movsq
    lea rsi, nData
    add qword ptr [rsi + 10h], rax  ;Add the code segment address to the ptrs
    add qword ptr [rsi + 18h], rax
    mov rdi, qword ptr fs:[dSeg.nulDevHdr]
    mov ecx, 34
    rep movsb


    lea rbp, startmsg   ;Get the absolute address of message
    mov eax, 1304h
    int 30h
    jmp short $

    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
nData   LABEL QWORD
    dq conHdr
    dw 08004h
    dq nulDriver
    dq nulIntr
    db "NUL     "
loadCode ENDS

;codeSegment points to the start of this segment!
resCode SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
FATprocs    PROC
FATprocs    ENDP

drivers PROC
conHdr  drvHdr <auxHdr,  08013h, commonStrat, conDriver, "CON     ">
auxHdr  drvHdr <prnHdr,  08000h, commonStrat, auxDriver, "AUX     ">
prnHdr  drvHdr <clkHdr,  0A040h, commonStrat, prnDriver, "PRN     ">
clkHdr  drvHdr <msdHdr,  08008h, commonStrat, clkDriver, "CLOCK$  ">
msdHdr  drvHdr <com1Hdr, 00840h, commonStrat, msdIntr, <0,0,0,0,0,0,0,0>>
com1Hdr drvHdr <com2Hdr, 08000h, commonStrat, com1Intr, "COM1    ">
com2Hdr drvHdr <com3Hdr, 08000h, commonStrat, com2Intr, "COM2    ">
com3Hdr drvHdr <com4Hdr, 08000h, commonStrat, com3Intr, "COM3    ">
com4Hdr drvHdr <lpt1Hdr, 08000h, commonStrat, com4Intr, "COM4    ">
lpt1Hdr drvHdr <lpt2Hdr, 0A040h, commonStrat, lptIntr, "LPT1    ">
lpt2Hdr drvHdr <lpt3Hdr, 0A040h, commonStrat, lptIntr, "LPT2    ">
lpt3Hdr drvHdr <-1, 0A040h, commonStrat, lptIntr, "LPT3    ">
commonStrat PROC
;DOS calls this function with rbx=Ptr to request header
;DOS also sets fs to point to its data segment when entered
    mov qword ptr [reqHdrPtr], rbx
    ret
reqHdrPtr  dq -1    ;Where the default device drivers store the ReqPtr
commonStrat ENDP

nulDriver   PROC
    mov word ptr [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
nulIntr LABEL BYTE
    ret
nulDriver   ENDP

conDriver   PROC
    push rax
    push rbx
    mov rbx, qword ptr [reqHdrPtr]
    mov al, byte ptr [rbx + drvReqHdr.cmdcde]
    pop rbx
    pop rax
    ret
conError:
conInit:
conIOCTLRead:
    mov word ptr [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
    ret
conRead:
    push rdi
    push rcx
    mov rdi, qword ptr [rbx + drvReqHdr.bufptr] ;Point rdi to destination buffer
    mov ecx, dword ptr [rbx + drvReqHdr.tfrlen] ;Number of chars to read
@@:
    xor ax, ax  ;Wait for char input
    int 36h     ;Get the ASCII char into al
    stosb       ;Save al in buffer and inc rdi
    loop @b
    pop rcx
    pop rdi
    ret
conNondestructiveRead:
conInputStatus:
conFlushInputBuffers:
conWrite:
    push rsi
    push rcx
    mov rsi, qword ptr [rbx + drvReqHdr.bufptr] ;Point rsi to buffer pointer
    mov ecx, dword ptr [rbx + drvReqHdr.tfrlen] ;Number of chars to transfer
@@: 
    lodsb   ;Get char into al, and inc rsi
    int 49h ;Fast print char
    loop @b ;keep printing until ecx is zero
    pop rcx
    pop rsi
    mov word ptr [rbx + drvReqHdr.status], 0100h    ;Request complete
    ret
conOutputStatus:
conFlushOutputBuffers:
conIOCTLWrite:
conOpen:
conClose:
conOutUntilBusy:
conGenericIOCTL:
    mov word ptr [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
    ret
int49hHook: ;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
conDriver   ENDP

auxDriver   PROC
auxDriver   ENDP

prnDriver   PROC
prnDriver   ENDP

clkDriver   PROC
clkDriver   ENDP

msdDriver   PROC
msdIntr     LABEL   BYTE
    push rax
    push rdi
    push r8
    push r9
    push r10
    push r11
    push r12
    push r13
    push r14
    push r15
    mov rdi, qword ptr [reqHdrPtr]  ;Get the ptr to the req header in rdi
    mov al, byte ptr [rdi + drvReqHdr.cmdcde]   ;Get command code in al
    cmp al, 24  ;Check cmd num is valid
    ja msdError
    test al, al
    jz msdInit
    cmp al, 01
    jz msdMedChk
    cmp al, 02
    jz msdBuildBPB
    cmp al, 03
    jz msdIOCTLRead
    cmp al, 04
    jz msdRead
    cmp al, 08
    jz msdWrite
    cmp al, 09
    jz msdWriteVerify
    cmp al, 12
    jz msdIOCTLWrite
    cmp al, 13
    jz msdDevOpen
    cmp al, 14
    jz msdDevClose
    cmp al, 15
    jz msdRemovableMedia
    cmp al, 19
    jz msdGenericIOCTL
    cmp al, 23
    jz msdGetLogicalDev
    cmp al, 24
    jz msdSetLogicalDev
msdError:
;Place Error, Unknown Command error in status field
    mov word ptr [rdi + drvReqHdr.status], 8003h
msdIntrExit:
    pop r15
    pop r14
    pop r13
    pop r12
    pop r11
    pop r10
    pop r9
    pop r8
    pop rdi
    pop rax
    ret
msdInit:            ;Function 0
    int 31h ;Get number of Int 33h devices in R8b, and aux devices in byte 3
    movzx r10, r8b   ;Isolate the number of Int 33h devs only
    mov r9, 2
    cmp r10, 1
    cmove r8, r9    ;If we have one device detected only, make it two!
    mov byte ptr [msdHdr + drvHdr.drvNam], r8b ;Save num Int 33h devs here
msdMedChk:          ;Function 1
msdBuildBPB:        ;Function 2
msdIOCTLRead:       ;Function 3
msdRead:            ;Funciton 4
msdWrite:           ;Function 8
msdWriteVerify:     ;Function 9
msdIOCTLWrite:      ;Function 12
msdDevOpen:         ;Function 13
msdDevClose:        ;Function 14
msdRemovableMedia:  ;Function 15
msdGenericIOCTL:    ;Function 19
msdGetLogicalDev:   ;Function 23
msdSetLogicalDev:   ;Function 24
    jmp short msdIntrExit
msdDriver   ENDP

comDriver   PROC
com1Intr    LABEL   BYTE
    jmp short comIntr
com2Intr    LABEL   BYTE
    jmp short comIntr
com3Intr    LABEL   BYTE
    jmp short comIntr
com4Intr    LABEL   BYTE
comIntr:
    ret
comDriver   ENDP

lptDriver   PROC    ;Drivers for LPT 1, 2, 3

lptIntr    LABEL   BYTE    ;LPT act as null device drivers
    push rdi
    mov rdi, qword ptr [reqHdrPtr]
    mov word ptr [rdi + drvReqHdr.status], 0100h    ;Done bit set
    pop rdi
    ret
lptDriver   ENDP

driverDataPtr   LABEL   BYTE
drivers ENDP

resCode ENDS

END