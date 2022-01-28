    
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
    dq -1
    dw 08004h
    dq commonStrat
    dq nulDriver
    db "NUL     "
loadCode ENDS

resCode SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
conHdr  drvHdr <auxHdr,  08013h, commonStrat, conDriver, "CON     ">
auxHdr  drvHdr <prnHdr,  08000h, commonStrat, auxDriver, "AUX     ">
prnHdr  drvHdr <clkHdr,  0A040h, commonStrat, prnDriver, "PRN     ">
clkHdr  drvHdr <msdHdr,  08008h, commonStrat, clkDriver, "CLOCK$  ">
msdHdr  drvHdr <com1Hdr, 00840h, commonStrat, msdDriver, <0,0,0,0,0,0,0,0>>
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
    push rdi
    mov rdi, qword ptr [reqHdrPtr]
    mov word ptr [rdi + drvReqHdr.status], 0100h    ;Done bit set
    pop rdi
    ret
nulDriver   ENDP

conDriver   PROC
conDriver   ENDP

auxDriver   PROC
auxDriver   ENDP

prnDriver   PROC
prnDriver   ENDP

clkDriver   PROC
clkDriver   ENDP

msdDriver   PROC
msdIntr:
    mov rdi, qword ptr [reqHdrPtr]
    cmp byte ptr [rdi + drvReqHdr.cmdcde], 24  ;Check cmd num is valid
    ja msdError
    ret
msdError:
;This function returns a command done, and replaces the default msdInit function
msdInit:
msdMedChk:
msdBuildBPB:
msdIOCTLRead:
msdRead:
msdWrite:
msdWriteVerify:
msdIOCTLWrite:
msdDevOpen:
msdDevClose:
msdRemovableMedia:
msdGenericIOCTL:
msdGetLogicalDev:
msdSetLogicalDev:
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

resCode ENDS

END