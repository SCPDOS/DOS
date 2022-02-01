    
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
    lea rsi, OFFSET resCodeStart ;Get offset of the segment in the file into rsi
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
@@:
    xor ax, ax
    int 36h
    mov ah, 0Eh
    int 30h
    jmp short @b

    startmsg db 0Ah,0Dh,"Starting SCP/DOS...",0Ah,0Dh,0
nData   LABEL QWORD
    dq conHdr
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     "
loadCode ENDS

;codeSegment points to the start of this segment!
resCode SEGMENT BYTE USE64
    ASSUME ds:FLAT, es:FLAT
;-----------------------------------:
;        Data Area Instance         :
;-----------------------------------:
data dSeg <>    ;Initialise data area as BSS 
        ORG $ 
;Offset all addresses below here by size of data area since it is uninitialised
; to not take up space in the binary file.
resCodeStart LABEL BYTE
;-----------------------------------:
;       Misc System routines        :
;-----------------------------------:
findLRUBuffer  PROC 
;Finds least recently used buffer, links it and returns ptr to it in rbx
    push rdx
    mov rbx, qword ptr [data.bufHeadPtr]
@@:
    mov rdx, rbx    ;Save a ptr to the previous buffer header
    mov rbx, qword ptr [rdx + bufferHdr.nextBufPtr] ;Get next buffer header ptr
    cmp qword ptr [rbx + bufferHdr.nextBufPtr], -1 ;Check if at LRU buffer
    jne @b   ;If not LRU, keep walking, else process
    mov qword ptr [rdx + bufferHdr.nextBufPtr], -1  ;Make prev node the LRU node
    mov rdx, qword ptr [data.bufHeadPtr]    ;Now copy old MRU buffer ptr to rdx
    mov qword ptr [data.bufHeadPtr], rbx    ;Sysvars to point to new buffer
    mov qword ptr [rbx + bufferHdr.nextBufPtr], rdx
    pop rdx
    ret
findLRUBuffer  ENDP

;-----------------------------------:
;       File System routines        :
;-----------------------------------:
FATprocs    PROC
FATprocs    ENDP

;-----------------------------------:
;        Interrupt routines         :
;-----------------------------------:
int49hHook  PROC    ;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
int49hHook  ENDP

;-----------------------------------:
;          Driver routines          :
;-----------------------------------:

drivers PROC
conHdr  drvHdr <auxHdr,  08013h, commonStrat, conDriver, "CON     ">
auxHdr  drvHdr <prnHdr,  08000h, commonStrat, com1Intr,  "AUX     ">
prnHdr  drvHdr <clkHdr,  0A040h, commonStrat, lpt1Hdr ,  "PRN     ">
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
reqHdrPtr  dq ?    ;Where the default device drivers store the ReqPtr
commonStrat ENDP

nulStrat   PROC
    mov word ptr [rbx + drvReqHdr.status], 0100h    ;Set done bit directly
nulIntr PROC
    ret
nulIntr     ENDP
nulStrat    ENDP

conDriver   PROC
    push rax
    push rbx
    mov rbx, qword ptr [reqHdrPtr]
    mov al, byte ptr [rbx + drvReqHdr.cmdcde]
    xor al, al
    jz conInit
    cmp al, 4
    jz conRead
    cmp al, 5
    jz conNondestructiveRead
    cmp al, 6
    jz conExit
    cmp al, 7
    jz conFlushInputBuffers
    cmp al, 8
    jz conWrite
    cmp al, 9
    jz conWrite
;All other cases fall through here
conExit:
    or word ptr [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rbx
    pop rax
    ret
conInit:    ;Function 0
    push rdx
    call conFlushInputBuffers  ;Call to flush keyboard buffer
    mov eax, 0500h  ;Set page zero as the default page
    int 30h
    mov ah, 02h
    xor edx, edx    ;Set screen cursor to top right corner
    mov bh, dl      ;Set cursor for page 0
    int 30h
    mov bh, 07h     ;Grey/Black attribs
    mov eax, 0600h  ;Clear whole screen
    int 30h
    pop rdx
    jmp short conExit
conIORead:
    mov word ptr [rbx + drvReqHdr.status], 8003h    ;Error, unknown command!
    jmp short conExit
conRead:    ;Function 4
    push rdi
    push rcx
    mov rdi, qword ptr [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
@@:
    cmp ecx, dword ptr [rbx + ioReqPkt.tfrlen]
    je @f
    xor eax, eax
    int 36h
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short @b
@@:
    mov dword ptr [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rdi
    jmp short conExit
conNondestructiveRead:  ;Function 5
    mov ah, 01h     ;Get key if exists
    int 36h
    jz @f           ;If zero clear => no key, go forwards
    ;Keystroke available
    mov byte ptr [rbx + nonDestInNoWaitReqPkt.retbyt], al   ;Move char in al
    jmp short conExit
@@: ;No keystroke available
    mov word ptr [rbx + nonDestInNoWaitReqPkt.status], 0300h   ;Set busy bit
    jmp short conExit
conFlushInputBuffers:   ;Function 7
    mov ah, 01      ;Get buffer status
    int 36h
    jz conExit      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short conFlushInputBuffers
conWrite:   ;Function 8 and 9
    push rsi
    push rcx
    mov rsi, qword ptr [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
@@: 
    cmp ecx, dword ptr [rbx + ioReqPkt.tfrlen]
    je @f
    lodsb   ;Get char into al, and inc rsi
    int 49h ;Fast print char
    inc ecx
    jmp short @b ;keep printing until all chars printed
@@:
    mov dword ptr [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rcx
    pop rsi
    jmp conExit
conDriver   ENDP

clkDriver   PROC
clkDriver   ENDP

msdDriver   PROC
msdIntr     LABEL   BYTE
    push rax
    push rbx
    mov rbx, qword ptr [reqHdrPtr]  ;Get the ptr to the req header in rdi
    mov al, byte ptr [rbx + drvReqHdr.cmdcde]   ;Get command code in al
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
msdIntrExit:
    or word ptr [rbx + drvReqHdr.status], 0100h ;Set done bit
    pop rbx
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
msdIOCTLRead:       ;Function 3, returns done
msdRead:            ;Funciton 4
msdWrite:           ;Function 8
    mov rdi, rbx
    mov ebp, dword ptr [rdi + ioReqPkt.tfrlen] ;Get num of sectors to transfer
    mov dword ptr [rdi + ioReqPkt.tfrlen], 0
@@:
    mov esi, 0FFh
    cmp ebp, esi
    mov eax, ebp    ;Default move remaining sectors into ax
    cmova eax, esi  ;If FF is less than bp, move FF by default
    mov rbx, qword ptr [rdi + ioReqPkt.bufptr]  ;Buffer head
    mov rcx, qword ptr [rdi + ioReqPkt.strtsc]  ;Start sector
    mov dl, byte ptr [rdi + ioReqPkt.unitnm]    ;Unit number for transfer
    int 33h
    jc @f
    movzx eax, al   ;Zero extend number of sectors transferred
    add dword ptr [rdi + ioReqPkt.bufptr], eax  ;Add number of 
@@:


msdWriteVerify:     ;Function 9, writes sectors then verifies then

msdIOCTLWrite:      ;Function 12, returns done
msdDevOpen:         ;Function 13
msdDevClose:        ;Function 14
msdRemovableMedia:  ;Function 15
msdGenericIOCTL:    ;Function 19
msdGetLogicalDev:   ;Function 23
msdSetLogicalDev:   ;Function 24
    jmp short msdIntrExit
msdDefLabel    db "NO NAME ",0 ;Default volume label
msdDriver   ENDP

comDriver   PROC
com1Intr    PROC
    mov byte ptr [comDevice], 0
    jmp short comIntr
com1Intr    ENDP
com2Intr    PROC
    mov byte ptr [comDevice], 1
    jmp short comIntr
com2Intr    ENDP
com3Intr    PROC
    mov byte ptr [comDevice], 2
    jmp short comIntr
com3Intr    ENDP
com4Intr    PROC
    mov byte ptr [comDevice], 3
    jmp short comIntr
com4Intr    ENDP
comIntr     PROC
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    mov rbx, qword ptr [reqHdrPtr]
    mov al, byte ptr [rbx + drvReqHdr.cmdcde]
    cmp al, 4
    jz comRead
    cmp al, 5
    jz comNondestructiveRead
    cmp al, 8
    jz comWrite
    cmp al, 9
    jz comWrite
;All other cases fall through here
comExit:
    or word ptr [rbx + drvReqHdr.status], 0100h    ;Merge done bit
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
comRead:
    push rdi
    mov rdi, qword ptr [rbx + ioReqPkt.bufptr]  ;Point rdi to caller buffer
    xor ecx, ecx    ;Zero the char counter
@@:
    cmp ecx, dword ptr [rbx + ioReqPkt.tfrlen]
    je @f
    mov eax, 02h    ;Recieve 
    mov dx, word ptr [comDevice]    ;Get transacting com device
    int 34h ;Recieve Char
    stosb   ;Store char in al into buffer and inc rdi
    inc ecx
    jmp short @b
@@:
    mov dword ptr [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    pop rdi
    jmp short comExit
comNondestructiveRead:
    mov word ptr [rbx + nonDestInNoWaitReqPkt.status], 0200h    ;Set busy bit 
    jmp short comExit
comWrite:
 mov rsi, qword ptr [rbx + ioReqPkt.bufptr] ;Point rsi to caller buffer 
    xor ecx, ecx    ;Zero the char counter
@@: 
    cmp ecx, dword ptr [rbx + ioReqPkt.tfrlen]
    je @f
    lodsb   ;Get char into al, and inc rsi
    mov ah, 01h ;Move function number into ah
    mov dx, word ptr [comDevice]
    int 34h ;Transmit char
    inc ecx
    jmp short @b ;keep printing until all chars printed
@@:
    mov dword ptr [rbx + ioReqPkt.tfrlen], ecx  ;Move num of transferred chars
    jmp short comExit
comDevice   db ?
comIntr     ENDP

comDriver   ENDP

lptDriver   PROC    ;Drivers for LPT 1, 2, 3

lptIntr     PROC    ;LPT act as null device drivers
    push rdi
    mov rdi, qword ptr [reqHdrPtr]
    mov word ptr [rdi + drvReqHdr.status], 0100h    ;Done bit set
    pop rdi
    ret
lptIntr     ENDP
lptDriver   ENDP
driverDataPtr   LABEL   BYTE
drivers ENDP

resCode ENDS

END