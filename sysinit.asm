Segment .text align=1
; We arrive here with the following values in the registers.
; rbx =  LBA of first Logical Block after SCP/BIOS
; dx  = Int 33h boot device number
; fs  = userbase pointer (pointer to first usable block of RAM)
tempPSP:    ;Here to allow the loader to use Int 41h once it is loaded high
    dw 0AA55h           ;Initial signature
    db (100h-2) dup (90h)   ;Duplicate NOPs for the PSP
    mov byte fs:[bootDrive], dl ;Save the boot drive in memory
    lea rdx, qword [tempPSP]    ;Get the address of the tempPSP
    mov qword fs:[currentPSP], rdx
;DOS allows for non-PARA aligned PSPs but DOS aligns all programs on PARA bndry
    mov ecx, 0C0000100h ;Read FS MSR
    rdmsr
    mov edi, edx        ;Get the hi dword, and clear the upper bytes
    shl rdi, 20h        ;Shift high
    mov edi, eax        ;Get the low dword in

    mov qword fs:[dosSegPtr], rdi 
    mov rbp, rdi    ;Save the start of dosSeg in rdx 
    add rdi, dSegLen ;Move destination past end of data area
    lea rsi, section.resSeg.start  ;Get RIP relative address to copy high
    mov ecx, 1000h
    rep movsq

    int 31h ;Get number of Int 33h devices in r8b
    mov byte fs:[numMSDdrv], r8b    ;Save number of physical int 33h devs
    mov byte fs:[lastdrvNum], 5     ;Last drive is by default 5
    mov byte fs:[numLDrives], 0     ;Number of logical drives

;Modify the pointers in nData before putting them in the data area
    add qword [nData + drvHdr.nxtPtr], rbp
    add qword [nData + drvHdr.strPtr], rbp
    add qword [nData + drvHdr.intPtr], rbp
;Copy the Null driver to its location in Sysvars
    mov ecx, drvHdr_size
    lea rsi, qword [nData]
    lea rdi, qword [rbp + nulDevHdr]
    rep movsb   

;Adjust the addresses in the other driver headers 
    mov rsi, conHdr ;Point to the first non-NUL dev in chain
    mov ecx, 12      ;12 drivers in data area
    lea rsi, qword [rsi + rbp]  ;Get effective addr of driver header
adjDrivers:
    call adjustDrvHdr
    loop adjDrivers

    ;Open NUL
    lea rbx, qword [rbp + nulDevHdr + drvHdr.strPtr]    ;Get ptr to strat ptr
    mov rbx, qword [rbx]    ;Get strat ptr
    xor al, al
    call rbx

;Open CON
conInit:    ;Rather than keeping this resident... do it here
.ci0:
    mov ah, 01      ;Get buffer status
    int 36h
    jz .ci1      ;If zero clear => no more keys to read
    xor ah, ah
    int 36h ;Read key to flush from buffer
    jmp short .ci0
.ci1:
    mov eax, 0500h  ;Set page zero as the default page
    int 30h
    mov ah, 02h
    xor edx, edx    ;Set screen cursor to top right corner
    mov bh, dl      ;Set cursor for page 0
    int 30h
    mov bh, 07h     ;Grey/Black attribs
    mov eax, 0600h  ;Clear whole screen
    int 30h

    ;Open Mass Storage
    lea rbx, qword [rbp + diskReqHdr]
    mov byte [rbx + initReqPkt.hdrlen], initReqPkt_size
    mov byte [rbx + initReqPkt.cmdcde], 00h     ;MSD init
    mov word [rbx + initReqPkt.status], 0       ;Zero status word
    mov al, byte fs:[numLDrives]
    mov byte [rbx + initReqPkt.drvnum], al      ;First unit is drive A
    call qword [rbp + msdHdr + drvHdr.strPtr]
    call qword [rbp + msdHdr + drvHdr.intPtr]

;Adjust Int 41h address table
adjInt41h:
    mov ecx, dispatchTableL/8 ;Number of elements in table
    mov rbx, functionDispatch.dispatchTable ;Get EA of table
    lea rbx, qword [rbp+rbx]    ;Point to the start of the relocated table 
.ai41h:
    add qword [rbx], rbp    ;Add base address value to entry in reloc table
    add rbx, 8              ;Each entry is size 8
    dec ecx
    jnz .ai41h  ;Keep looping until all entries have been adjusted

;Adjust Interrupt Entries Int 40h-49h
adjInts:
    mov bl, 40h
    mov eax, 0F007h ;Get the descriptor
    int 35h
    mov ecx, 40h    ;Start from interrupt 40h
    lea rdi, intData
    mov esi, eax    ;Move segment selector info to esi
.ai0:
    mov eax, 0F008h ;Set the descriptor
    mov rbx, qword [rdi]    ;Get address pointed to by rdi
    add rbx, rbp            ;Add the relocated base to rbx
    int 35h
    add rdi, 8
    inc ecx
    cmp ecx, 4Ah
    jne .ai0

;Fill in the default file table entries
    ;lea rbx, qword [rbp + firstSftHeader]
    ;mov qword [rbx + sfth.qNextSFTPtr], -1  ;Last sfth in chain
    ;mov word [rbx + sfth.wNumFiles], 5      ;5 default files
    ;mov qword fs:[sftHeadPtr], rbx  ;Save ptr to this sft header in SysVars

    ;lea rbx, qword [rbp + firstSft]
    ;mov word [rbx + sft.wNumHandles], 0 ;Nothing pointing to this file yet
    ;mov word [rbx + sft.w]


;Test Error Case
    ;mov ah, 00110000b
    ;mov al, 00h
    ;mov edi, 0Ch
    ;int 44h

    lea rdx, qword [startmsg]   ;Get the absolute address of message
    mov ah, 09h
    int 41h

    mov rsi, fs:[nulDevHdr]
    mov eax, 0C501h ;Connect debugger
    int 35h
l1:
    mov ah, 01h  ;Write with echo
    int 41h
    cmp al, 0
    je l2
    jmp short l1
l2:
    mov ah, 07h
    int 41h
    cmp al, 42h
    jne l1
l3:
    mov word fs:[CLOCKrecrd + clkStruc.dateWord], 0
    lea rbx, qword [rbp + charReqHdr] ;Get the address of this request block
    lea rax, qword [rbp + CLOCKrecrd]
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read the time
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 06
    call qword [rbp + clkHdr + drvHdr.strPtr]
    call qword [rbp + clkHdr + drvHdr.intPtr]

    mov ah, 03h
    xor bh, bh
    int 30h
    xor dl, dl  ;0 column
    mov ah, 02h
    int 30h

    lea rbx, qword [rbp + CLOCKrecrd]
    movzx eax, byte [rbx + clkStruc.hours]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.minutes]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, ":"
    int 30h
    movzx eax, byte [rbx + clkStruc.seconds]
    call .clkHexToBCD
    mov ah, 0Eh
    mov al, "."
    int 30h
    movzx eax, byte [rbx + clkStruc.hseconds]
    call .clkHexToBCD
    jmp l1
.clkHexToBCD:
;Converts a Hex byte into two BCD digits
;Takes input in each nybble of al
    push rbx
    ;xchg bx, bx
    mov rbx, 0Ah  ;Divide by 10
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb0
    add dl, 'A'-'0'-10
.chtb0:
    mov cl, dl    ;Save remainder byte
    xor edx, edx
    div rbx
    add dl, '0'
    cmp dl, '9'
    jbe .chtb1
    add dl, 'A'-'0'-10
.chtb1:
    mov ch, dl    ;Save remainder byte

    mov al, ch    ;Get most sig digit into al
    mov ah, 0Eh
    int 30h
    mov al, cl    ;Get least sig digit into al
    mov ah, 0Eh
    int 30h
    pop rbx
    ret

adjustDrvHdr:
;Input: rsi = Effective address of driver in DOS segment
;       rbp = Ptr to the start of the DOS segment
;Output: rsi = EA of next header in DOS segment
    add qword [rsi + drvHdr.nxtPtr], rbp    ;Adjust address
    add qword [rsi + drvHdr.strPtr], rbp
    add qword [rsi + drvHdr.intPtr], rbp
    add rsi, drvHdr_size
    ret
startmsg db "Starting SCP/DOS...",0Ah,0Dh,"$"
intData:
    dq terminateProcess ;Int 40h
    dq functionDispatch ;Int 41h
    dq terminateHandler ;Int 42h
    dq ctrlCHandler     ;Int 43h
    dq critErrorHandler ;Int 44h
    dq absDiskRead      ;Int 45h
    dq absDiskWrite     ;Int 46h
    dq terminateResident    ;Int 47h
    dq inDosHandler     ;Int 48h
    dq fastOutput       ;Int 49h
nData:
    dq conHdr
    dw 08004h
    dq nulStrat
    dq nulIntr
    db "NUL     " ;Default NUL data
