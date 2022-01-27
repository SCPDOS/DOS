    
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
    lea rsi, OFFSET resCode ;Get offset of the segment in the file into rsi
    mov ecx, 1000h
    rep movsq
    lea rbp, startmsg   ;Get the absolute address of message
    mov eax, 1304h
    int 30h
    jmp short $
    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
loadCode ENDS

resCode SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
    jmp short $
resCode ENDS

END