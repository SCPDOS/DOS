
;-----------------------------------:
;           Static Data             :
;-----------------------------------:
dosMajor    db 00h      ;Version 0
dosMinor    db 01h      ;.01
;-----------------------------------:
;       Misc System routines        :
;-----------------------------------:
criticalDOSError:
;Will swap stacks and enter int 44h safely and handle passing the right data 
; to the critical error handler.
; Called with ax, di and rsi set as required by Int 44h (caller decides)
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
; Return response from int 44h in al
    cli ;Disable Interrupts
    mov qword [xInt44hRSP], rsp
    mov rsp, qword [oldRSP] ;Get the old RSP value
    int 44h ;Call critical error handler
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    sti ;Reenable Interrupts
    ret
findLRUBuffer: 
;Finds first free or least recently used buffer, links it and returns ptr to it 
; in rbx
;Input: Nothing
;Output: rbx = Pointer to the buffer hdr to use
    push rdx
    mov rbx, qword [bufHeadPtr]
    cmp byte [rbx + bufferHdr.driveNumber], -1  ;Check if 1st entry is free
    je .flbExit 
    cmp qword [rbx + bufferHdr.nextBufPtr], -1  ;Check if 1st entry is last
    je .flbExit
.flbWalk:
    mov rdx, rbx    ;Save a ptr to the previous buffer header
    mov rbx, qword [rdx + bufferHdr.nextBufPtr] ;Get next buffer header ptr
    cmp byte [rbx + bufferHdr.driveNumber], -1
    je .flbFreeLink ;If free, link to head, and xlink prev and next buffs
    cmp qword [rbx + bufferHdr.nextBufPtr], -1 ;Check if at LRU buffer
    jne .flbWalk   ;If not LRU, keep walking, else process
    mov qword [rdx + bufferHdr.nextBufPtr], -1  ;Make prev node the LRU node
.flbHeadLink:
    mov rdx, qword [bufHeadPtr]    ;Now copy old MRU buffer ptr to rdx
    mov qword [bufHeadPtr], rbx    ;Sysvars to point to new buffer
    mov qword [rbx + bufferHdr.nextBufPtr], rdx
.flbExit:
    pop rdx
    ret
.flbFreeLink:
    push rcx
    mov rcx, qword [rbx + bufferHdr.nextBufPtr]
    mov qword [rdx + bufferHdr.nextBufPtr], rcx  ;Point prev buff past rbx
    pop rcx
    jmp short .flbHeadLink
findSectorInBuffer:
;Finds the Buffer for a sector
;If the sector is not in a buffer, returns with a -1
;Input: rax = Sector number
;        dl = Drive number
;Output: rbx = Buffer hdr pointer or -1
    mov rbx, qword [bufHeadPtr]
.fsiCheckBuffer:
    cmp byte [rbx + bufferHdr.driveNumber], dl
    jne .fsiGotoNextBuffer
    cmp qword [rbx + bufferHdr.bufferLBA], rax
    jne .fsiGotoNextBuffer
.fsiExit:
    ret
.fsiGotoNextBuffer:
    mov rbx, qword [rbx + bufferHdr.nextBufPtr]
    cmp rbx, -1     ;If rbx points to -1, exit
    je .fsiExit
    jmp short .fsiCheckBuffer
findDPB:
;Finds the DPB for a given drive
;Input:  dl = Drive number (0=A, 1=B etc...)
;Output: al = 00, rbx = Pointer to the DPB
;        al = -1, Failed, no DPB for device, rbx destroyed
    mov rbx, qword [dpbHeadPtr]
.fd1:
    xor al, al
    cmp byte [rbx + dpb.bDriveNumber], dl
    je .fd2
    mov rbx, qword [rbx + dpb.qNextDPBPtr]
    mov al, -1
    cmp rbx, -1 ;If rbx followed last item in list, no DPB exists for dl
    jne .fd1
.fd2:
    ret
;-----------------------------------:
;       File System routines        :
;-----------------------------------:
name2Clust:
;Converts a path + file name to a first cluster number
clust2FATEntry:
;Converts a cluster number to a FAT entry
;Entry:  rsi points to the DPB for the transacting device
;        eax = Cluster number to look for
;Exit: eax = Sector on disk of FAT, edx = 1.5Word/Word/DWord in sector of entry
    push rbx
    push rcx
    mov ebx, dword [rsi + dpb.dClusterCount]
    cmp ebx, fat16MaxClustCnt
    jae .fat32
    cmp ebx, fat12MaxClustCnt
    jb .fat12
;FAT16
    shl eax, 1  ;Multiply cluster number by 2
    jmp short .common
.fat12:
    mov ecx, eax    ;ecx = eax
    shr ecx, 1      ;ecx = ecx / 2
    add eax, ecx    ;eax = eax + ecx    (eax * 1.5)
    jmp short .common
.fat32:
    shl eax, 2  ;Multiply cluster number by 4
.common:
;eax has the FAToffset
    mov cl, byte [rsi + dpb.bBytesPerSectorShift]
    mov edx, 1
    shl edx, cl    ;Turn edx to number of bytes per sector
    mov ecx, edx
    xor edx, edx    ;edx = 0
    div ecx         ;Divide by bytes per sector (0:eax / ecx)
    add eax, dword [rsi + dpb.dFAToffset]   ;Add the offset to the first FAT
    pop rcx
    pop rbx
    ret
readBuffer:
;This function will return a pointer to the desired data OR 
; find the most appropriate buffer and read the relevant data into the buffer,
; returning a pointer to the sector buffer in rbx.
;Entry: rax = Sector to read
;        cl = Data type being read (FAT, DIR, Data) 
;       rsi = DPB of transacting drive
;Exit:  rbx = Pointer to buffer containing sector (not the buffer header)
;       All other registers as before
; If CF set, terminate the request.
    push rdx
    push rcx
    mov dl, byte [rsi + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rbx, -1
    je .rbReadNewSector
    add rbx, bufferHdr_size ;Have the pointer point to the data area
.rbExit:
    clc
.rbExitNoFlag:
    pop rcx
    pop rdx
    ret
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rbx
    call flushBuffer
    jc .rbExitNoFlag    ;Exit in error
;rbx points to buffer that has been appropriately linked to the head of chain
    mov byte [rbx + bufferHdr.driveNumber], dl
    mov byte [rbx + bufferHdr.bufferFlags], cl ;FAT/DIR/DATA
    mov qword [rbx + bufferHdr.bufferLBA], rax
    cmp cl, fatBuffer
    mov dl, 1   ;Default values if not fat buffer
    mov ecx, 0  ;Ditto!
    jne .rbNonFATbuffer
    mov dl, byte [rsi + dpb.bNumberOfFATs]
    mov ecx, dword [rsi + dpb.dFATlength]
.rbNonFATbuffer:
    mov byte [rbx + bufferHdr.bufFATcopy], dl
    mov dword [rbx + bufferHdr.bufFATsize], ecx
    mov qword [rbx + bufferHdr.driveDPBPtr], rsi
    mov byte [rbx + bufferHdr.reserved], 0
    add rbx, bufferHdr_size ;Point to the buffer now
    call readSector ;Carry the flag from the request
    jmp short .rbExitNoFlag

readSector:
;Reads a sector into a sector buffer
;Entry: rax = Sector Number
;       rbx = Pointer to buffer space
;        cl = Data type being read (FAT, DIR, Data) 
;       rsi = DPB of transacting drive
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;First make request to device driver
    push rax
    push rcx
    push rdx
    push rbp
;Build a request block in diskReqHdr
    mov rbp, rax    ;Move sector number into rbp
.rsRequest0:
    mov ch, 3  ;Repeat attempt counter
.rsRequest1:
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov qword [diskReqHdr + ioReqPkt.devptr], 0
    mov dword [diskReqHdr + ioReqPkt.tfrlen], 1 ;One sector
    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.cmdcde], drvREAD
    mov qword [diskReqHdr + ioReqPkt.strtsc], rbp   ;rbp has sector number
    mov qword [diskReqHdr + ioReqPkt.bufptr], rbx   ;rbx points to buffer
    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov al, byte [rsi + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov rdx, qword [rsi + dpb.qDriverHeaderPtr] ;Get pointer to driver header

    call [rdx + drvHdr.strPtr]
    call [rdx + drvHdr.intPtr]
    test word [diskReqHdr + ioReqPkt.status], 8000h  ;Test error bit
    jnz .rsFail
    mov rax, rbx ;Get the pointer to the sector buffer in rax
.rsExit:
    clc
.rsExitBad:
    pop rbp
    pop rdx
    pop rcx
    pop rax
    ret
.rsFail:
;Enter here only if the request failed
    dec ch
    jnz .rsRequest1 ;Try the request again!
;Request failed thrice, critical error call
    push rbx    ;Save the pointer to the data buffer area
    xor ax, ax
    mov bx, 1
    cmp cl, dosBuffer
    cmove ax, bx
    inc bx
    cmp cl, fatBuffer
    cmove ax, bx
    inc bx
    cmp cl, dirBuffer
    cmove ax, bx
    inc bx
    cmp cl, dataBuffer
    cmove ax, bx
    pop rbx
    shl ax, 1   ;Shift number into bits 1-2 and clear bit 0 (read operation)
    mov ah, al  ;Move into ah
    or ah, 30h  ;Set Retry and Ignore bits and bit 7 = 0 (msd device)
    mov al, byte [rsi + dpb.bDriveNumber]
    push rdi
    push rsi
    mov di, word [diskReqHdr + ioReqPkt.status]
    and di, 00FFh   ;Mask off upper byte
    mov rsi, rdx    ;Get ptr to device driver in rsi
    call criticalDOSError
    pop rsi
    pop rdi

    test al, al ;Ignore
    jz .rsExit  ;rbx contains the buffer to the data area
    test al, 1
    jnz .rsRequest0 ;Retry
    stc
    jmp .rsExitBad  ;Abort
flushBuffer:
;Flushes the data in a sector buffer to disk!
;Entry: rbx = Pointer to buffer header for this buffer
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;First make request to device driver
    push rax
    push rcx
    push rdx
    push rbp
    push rsi
    test byte [rbx + bufferHdr.bufferFlags], dirtyBuffer    ;Data modified?
    jz .fbExit  ;Skip write to disk if data not modified
;Build a request block in diskReqHdr
    mov rsi, qword [rbx + bufferHdr.driveDPBPtr]    ;Get dpbptr in rsi
.fbRequest0:
    mov ch, 3  ;Repeat attempt counter
.fbRequest1:
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov qword [diskReqHdr + ioReqPkt.devptr], 0
    mov dword [diskReqHdr + ioReqPkt.tfrlen], 1 ;One sector
    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.cmdcde], drvWRITE
    mov al, byte [verifyFlag]
    and al, 1   ;Only get the last bit
    add byte [verifyFlag], al   ;Change write into write/verify if needed
    mov rax, qword [rbx + bufferHdr.bufferLBA]
    mov qword [diskReqHdr + ioReqPkt.strtsc], rax

    lea rax, qword [rbx + bufferHdr_size]
    mov qword [diskReqHdr + ioReqPkt.bufptr], rax   ;rbx points to buffer

    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov al, byte [rsi + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov rdx, qword [rsi + dpb.qDriverHeaderPtr] ;Get pointer to driver header

    call [rdx + drvHdr.strPtr]
    call [rdx + drvHdr.intPtr]
    test word [diskReqHdr + ioReqPkt.status], 8000h  ;Test error bit
    jnz .fbFail
;Now check if the buffer was a FAT, to write additional copies
    test byte [rbx + bufferHdr.bufferFlags], fatBuffer ;FAT buffer?
    jz .fbExit  ;If not, exit
    dec byte [rbx + bufferHdr.bufFATcopy]
    jz .fbExit  ;Once this goes to 0, stop writing FAT copies
    mov eax, dword [rbx + bufferHdr.bufFATsize]
    add qword [rbx + bufferHdr.bufferLBA], rax ;Add the FAT size to the LBA
    jmp .fbRequest0 ;Make another request
.fbExit:
    clc
.fbExitBad:
    pop rsi
    pop rbp
    pop rdx
    pop rcx
    pop rax
    ret
.fbFail:
;Enter here only if the request failed
    dec ch
    jnz .fbRequest1 ;Try the request again!
;Request failed thrice, critical error call
    push rbx    ;Save the pointer to the data buffer area
    xor ax, ax
    mov bx, 1
    cmp cl, dosBuffer
    cmove ax, bx
    inc bx
    cmp cl, fatBuffer
    cmove ax, bx
    inc bx
    cmp cl, dirBuffer
    cmove ax, bx
    inc bx
    cmp cl, dataBuffer
    cmove ax, bx
    pop rbx
    shl ax, 1   ;Shift number into bits 1-2
    mov ah, al  ;Move into ah
    or ah, 31h  ;Set Retry and Ignore, bit 7 = 0 (msd device) and bit 0 (write)
    mov al, byte [rsi + dpb.bDriveNumber]
    push rdi
    push rsi
    mov di, word [diskReqHdr + ioReqPkt.status]
    and di, 00FFh   ;Mask off upper byte
    mov rsi, rdx    ;Get ptr to device driver in rsi
    call criticalDOSError
    pop rsi
    pop rdi

    test al, al ;Ignore
    jz .fbExit  ;rbx contains the buffer to the data area
    test al, 1
    jnz .fbRequest0 ;Retry
    stc
    jmp .fbExitBad  ;Abort
;-----------------------------------:
;        Interrupt routines         :
;-----------------------------------:
terminateProcess:   ;Int 40h

functionDispatch:   ;Int 41h Main function dispatcher
;ah = Function number, all other registers have various meanings
    cli ;Halt external interrupts
    cld ;Ensure all string ops occur in the right direction
    cmp ah, dispatchTableL/8    ;Number of functions
    ja .fdExitBad
    ;Cherry pick functions
    cmp ah, 33h ;CTRL+BREAK check
    jb .fsbegin   ;If below skip these checks
    je .ctrlBreakCheck
    cmp ah, 64h
    je .setDriverLookahead  ;Reserved, but avoids usual Int 41h spiel
    ja .fsbegin   ;If above, do usual Int41 entry
    cmp ah, 51h
    je .getCurrProcessID    ;This and below are exactly the same
    cmp ah, 62h
    je .getPSPaddr          ;Calls the above function
    cmp ah, 50h
    je .setCurrProcessID
.fsbegin:
    pushDOS ;Push the usual prologue registers
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
;Here, we want to save oldRSP in the callers PSP
    cmp byte [inDOS], 1 ;Check how many times we are in DOS
    jne .fsb1   ;If this is first entry, save rsp in callers PSP
    mov rax, qword [currentPSP] ;Get current PSP address
    mov qword [rax + psp.rspPtr], rsp    ;Save rsp on callers stack
.fsb1:
    pop rax     ;Get old rax back
    push rax    ;and push it back onto the stack
    lea rsp, critStakTop
    sti         ;Reenable interrupts

    mov byte [int48Flag], 1 ;Make it ok to trigger Int 48h

    mov qword [oldRBX], rbx ;Need to do this as I might switch stacks later
    movzx ebx, ah   ;Move the function number bl zero extended to rbx
    shl ebx, 3      ;Multiply the function number by 8 for offset into table
    push rax        ;Push rax onto the stack
    lea rax, qword [.dispatchTable]
    add rbx, rax    ;Add dispatch table offset into rbx
    pop rax
    mov rbx, qword [rbx]    ;Get the address from the dispatch table

    test ah, ah     ;Simple Terminate function?
    jz .fddiskOp
    cmp ah, 59h     ;Extended Error report?
    je .fdGoToFunction  ;Bypass code that clears the error report
    cmp ah, 0Ch     ;Are we a char function?
    ja .fddiskOp
;Char operations here
    test byte [critErrFlag], 1  ;Are we in critical error?
    jnz .fdGoToFunction         ;If we are, stay on Critical Error Stack
    lea rsp, IOStakTop          ;Otherwise, switch to IO stack
    jmp short .fdGoToFunction
.fddiskOp:
    ;Disk operations go here
    ;Clear up error info
    mov byte [errorLocus], 1    ;Reset to generic, unknown locus
    mov byte [critErrFlag], 0   ;Clear the Critical Error Flag
    mov byte [errorDrv], -1     ;Set the drive which caused the error to none

    mov byte [int48Flag], 0     ;Turn off the ability to trigger Int 48h
    lea rsp, DiskStakTop        ;Swap the stack to the Disk Transfer Stack
    test byte [breakFlag], -1   ;Test if set
    jz .fdGoToFunction
; HANDLE CTRL+BREAK HERE!
.fdGoToFunction:
    xchg rbx, qword [oldRBX]    ;Put the call addr in oldRBX and get oldRBX back
    ;Potentially point rbp to caller reg frame for easy access of registers 
    ;
    ;IF YOU USE RAX AND DONT NEED A RETURN VALUE IN AL, 
    ;ENSURE YOU READ AL FROM THE STACK FRAME BEFORE RETURNING TO PRESERVE AL!!!
    ;
    call qword [oldRBX]     ;Call the desired function, rax contains ret code
.fdExit:
    cli     ;Redisable interrupts
    ;???
    dec byte [inDOS]            ;Decrement the inDOS count
    mov rsp, qword [oldRSP]     ;Point rsp to old stack
    mov byte [rsp], al   ;Put the ret code into its pos on the register frame
    mov rax, qword [oldoldRSP]
    mov qword [oldRSP], rax
    popDOS  ;Pop the frame
    iretq
.fdExitBad:
    mov ah, 0
    iretq
.simpleTerminate:     ;ah = 00h
    ret
.stdinReadEcho:     ;ah = 01h
;Return char that has been read and echoed in al
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .stdinReadEchoBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    cmp byte [.stdinReadEchoBuffer], 00h
    jz .stdireexit
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .stdinReadEchoBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
.stdireexit:
    mov al, byte [.stdinReadEchoBuffer]
    ret
.stdinReadEchoBuffer    db 0
.stdoutWrite:       ;ah = 02h
;Bspace is regular cursor left, does not insert a blank
    mov byte [.stdoutWriteBuffer], dl
    lea rbx, charReqHdr ;Get the address of this request block
    lea rdx, .stdoutWriteBuffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    ret
.stdoutWriteBuffer db 0
.stdauxRead:        ;ah = 03h
.stdauxWrite:       ;ah = 04h
.stdprnWrite:       ;ah = 05h
.directCONIO:       ;ah = 06h
.waitDirectInNoEcho:;ah = 07h
;Return char in al
    lea rbx, charReqHdr ;Get the address of this request block
    lea rax, .function7buffer
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 04h   ;Read a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rax
    mov dword [rbx + ioReqPkt.tfrlen], 01
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]
    mov al, byte [.function7buffer]
    ret
.function7buffer    db 0
.waitStdinNoEcho:   ;ah = 08h
    ret
.printString:       ;ah = 09h
    xor ecx, ecx    ;Clear char counter
    mov eax, "$"    ;Terminating char
    mov rdi, rdx    ;Set up for scasb
.ps0:   ;Search for $ to get count of chars
    scasb
    je .ps1
    inc ecx
    jmp short .ps0
.ps1:   ;Use handle 
    lea rbx, charReqHdr ;Get the address of this request block
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], 08h   ;Write a byte
    mov word [rbx + ioReqPkt.status], 0 ;Zero status word
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    call qword [conHdr + drvHdr.strPtr]
    call qword [conHdr + drvHdr.intPtr]

    mov rbx, qword [oldRSP]
    mov al, byte [rbx+callerFrame.rax]      ;Gets al to preserve it
    ret
.buffStdinInput:    ;ah = 0Ah
.checkStdinStatus:  ;ah = 0Bh
.clearbuffDoFunc:   ;ah = 0Ch
.diskReset:         ;ah = 0Dh
.selectDisk:        ;ah = 0Eh
.openFileFCB:       ;ah = 0Fh
.closeFileFCB:      ;ah = 10h
.findFirstFileFCB:  ;ah = 11h
.findNextFileFCB:   ;ah = 12h
.deleteFileFCB:     ;ah = 13h
.sequentialReadFCB: ;ah = 14h
.sequentialWriteFCB:;ah = 15h
.createFileFCB:     ;ah = 16h
.renameFileFCB:     ;ah = 17h
                    ;ah = 18h unused
.getCurrentDisk:    ;ah = 19h, get current default drive
    mov al, byte [currentDrv]
    ret
.setDTA:            ;ah = 1Ah
;Called with:
;   rdx = Pointer to the new default DTA
    mov rbx, qword [oldRSP]
    mov rdx, qword [rbx + callerFrame.rdx]
    mov qword [currentDTA], rdx
    ret
.FATinfoDefault:    ;ah = 1Bh
.FatinfoDevice:     ;ah = 1Ch
                    ;ah = 1Dh unused
                    ;ah = 1Eh unused
.getCurrentDPBptr:  ;ah = 1Fh, simply calls int 41h ah = 32h with dl = 0
                    ;ah = 20h unused
.randomReadFCB:     ;ah = 21h
.randomWriteFCB:    ;ah = 22h
.getFileSizeFCB:    ;ah = 23h
.setRelRecordFCB:   ;ah = 24h
.setIntVector:      ;ah = 25h
;Called with:
;   rdx = Pointer to interrupt handler
;   al = Interrupt number
    mov ebp, eax ;al has interrupt number which we need to save
    and ebp, 0FFh   ;Zero everything but the bottom byte
;First call to get default BIOS segement selector and attribute word
    mov bl, al  ;Set interrupt number 
    mov eax, 0F007h ;Get the descriptor
    int 35h
    mov esi, eax    ;Move segment selector info to esi
    mov ecx, ebp    ;Get the interrupt number into cl
;dx preserves the attribute word
    mov rbp, qword [oldRSP]
    mov rbx, qword [rbp + callerFrame.rdx]  ;Pointer passed in rdx
    mov eax, 0F008h ;Set descriptor
    int 35h

    mov al, byte [rbp + callerFrame.rax]    ;Preserve low byte of rax
    ret
.createNewPSP:      ;ah = 26h
.randBlockReadFCB:  ;ah = 27h
.randBlockWriteFCB: ;ah = 28h
.parseFilenameFCB:  ;ah = 29h
.getDate:           ;ah = 2Ah
.setDate:           ;ah = 2Bh
.getTime:           ;ah = 2Ch
.setTime:           ;ah = 2Dh
.setResetVerify:    ;ah = 2Eh, turns ALL writes to write + verify
    mov byte [verifyFlag], al
    ret
.getDTA:            ;ah = 2Fh
    mov rdx, qword [oldRSP]
    mov rbx, qword [currentDTA] ;Get current DTA
    mov qword [rdx + callerFrame.rbx], rbx
    ret
.getDOSversion:     ;ah = 30h
    mov rdx, qword [oldRSP]
    xor ah, ah ;Continue the mainline PC-DOS identification line
    mov byte [rdx + callerFrame.rbx + 1], ah    ;Clear bh 
    mov ax, word [dosMajor] ;Major and minor version in al,ah resp.
    mov word [rdx + callerFrame.rax], ax    ;Save ax
    ret
.terminateStayRes:  ;ah = 31h
.getDeviceDPBptr:   ;ah = 32h
.ctrlBreakCheck:    ;ah = 33h
    test al, al
    jz .cbcget  ;Get the state
    mov byte [breakFlag], dl    ;Set the state
.cbcget:
    mov dl, byte [breakFlag]    ;Get the state
    ret
.getInDOSflagPtr:   ;ah = 34h
    lea rdx, inDOS
    mov rbx, qword [oldRSP]
    mov qword [rbx + callerFrame.rbx], rdx  ;save ptr in rbx
    ret
.getIntVector:      ;ah = 35h
;Called with:
;   al = Interrupt Number
;Returns:
;   rbx = Pointer to interrupt handler
    mov bl, al  ;Get the interrupt vector number into bl
    mov eax, 0F007h
    int 35h
    mov rdx, qword [oldRSP]
    mov qword [rdx + callerFrame.rbx], rbx  ;Save pointer in rbx
    mov al, byte [rdx + callerFrame.rax]    ;Get the low byte in al
    ret
.getDiskFreeSpace:  ;ah = 36h
.getsetSwitchChar:  ;ah = 37h, allows changing default switch from / to anything
.getsetCountryInfo: ;ah = 38h, localisation info
.makeDIR:           ;ah = 39h
.removeDIR:         ;ah = 3Ah
.changeCurrentDIR:  ;ah = 3Bh, changes directory for current drive
.createFileHdl:     ;ah = 3Ch, handle function
.openFileHdl:       ;ah = 3Dh, handle function
.closeFileHdl:      ;ah = 3Eh, handle function
.readFileHdl:       ;ah = 3Fh, handle function
.writeFileHdl:      ;ah = 40h, handle function
.deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
.movFileReadPtr:    ;ah = 42h, handle function, LSEEK
.changeFileModeHdl: ;ah = 43h, handle function, CHMOD
.ioctrl:            ;ah = 44h, handle function
.duplicateHandle:   ;ah = 45h, handle function
.forceDuplicateHdl: ;ah = 46h, handle function
.getCurrentDIR:     ;ah = 47h
.allocateMemory:    ;ah = 48h
.freeMemory:        ;ah = 49h
.reallocMemory:     ;ah = 4Ah
.loadExecChild:     ;ah = 4Bh, EXEC
.terminateClean:    ;ah = 4Ch, EXIT
.getRetCodeChild:   ;ah = 4Dh, WAIT, get ret code of subprocess
.findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
.findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
.setCurrProcessID:  ;ah = 50h, set current process ID (Set current PSP)
    mov qword [currentPSP], rbx ;Set the pointer
    ret
.getCurrProcessID:  ;ah = 51h, get current process ID (Get current PSP)
    mov rbx, qword [oldRSP]
    mov rdx, qword [currentPSP]
    mov qword [rbx + callerFrame.rbx], rdx   ;Set the caller pointer
    ret 
.getSysVarsPtr:     ;ah = 52h
    lea rdx, sysVarsPtr
    mov rbx, qword [oldRSP]
    mov qword [rbx + callerFrame.rbx], rdx
    ret
.createDPB:         ;ah = 53h, generates a DPB from a given BPB
;Only translates the data that can be garnered from a BPB to the DPB
;This is done so that the other fields of the DPB can be recycled
;Input: rsi = ptr to the BPB
;       rbp = ptr to the DPB
;bMediaDescriptor
    mov al, byte [rsi + bpb.media]
    mov byte [rbp + dpb.bMediaDescriptor], al
;bAccessFlag
    mov byte [rbp + dpb.bAccessFlag], -1    ;Not accessed
;dFirstFreeCluster
    mov dword [rbp + dpb.dFirstFreeCluster], 0  ;Start searching from start
;dNumberOfFreeClusters
    mov dword [rbp + dpb.dNumberOfFreeClusters], -1 ;Unknown
;bBytesPerSectorShift
    mov ax, word [rsi + bpb.bytsPerSec]
    mov cl, 7   ;Start with 128 byte sectors (not supported, min 512)
    shr ax, cl  ;Shift down
.cd0:
    shr ax, 1
    jz .cd1
    inc cl
    jmp short .cd0
.cd1:
    mov byte [rbp + dpb.bBytesPerSectorShift], cl
;bMaxSectorInCluster
    mov al, byte [rsi + bpb.secPerClus]
    dec al  ;Subtract one to get the max number of the last sector in a cluster
    mov byte [rbp + dpb.bMaxSectorInCluster], al
;bSectorsPerClusterShift
    inc al
    xor cl, cl
.cd2:
    shr al, 1
    jz .cd3
    inc cl
    jmp short .cd2
.cd3:
    mov byte [rbp + dpb.bSectorsPerClusterShift], cl
;dFAToffset, number of hidden sectors + number of reserved sectors
    movzx eax, word [rsi + bpb.revdSecCnt]
    add eax, dword [rsi + bpb.hiddSec] 
    mov dword [rbp + dpb.dFAToffset], eax
;bNumberOfFATs
    mov al, byte [rsi + bpb.numFATs]
    mov byte [rbp + dpb.bNumberOfFATs], al
;wNumberRootDirSectors
    movzx eax, word [rsi + bpb.rootEntCnt] ;Must be 0 on FAT 32
    shl eax, 5  ;Multiply by 32
    movzx ecx, word [rsi + bpb.bytsPerSec]
    dec ecx
    add eax, ecx
    xor edx, edx    ;Clear for divide
    div ecx ;Divide 0:eax by ecx, (e)ax has number of clusters
    mov word [rbp + dpb.wNumberRootDirSectors], ax  ;0 for FAT32
;dFATlength, get the FAT length
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax   ;If FATsz16 = 0, then use FATsz32
    cmovz eax, ebx  ;Only move 32bit value if sz16 was 0
    mov dword [rbp + dpb.dFATlength], eax
;Complex cases below...
;dClusterHeapOffset, start sector of the data area
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax
    cmovz eax, ebx
;eax = FATsz
    movzx ebx, word [rsi + bpb.totSec16]
    mov ecx, dword [rsi + bpb.totSec32]
    test ebx, ebx
    cmovz ebx, ecx 
;ebx = TotSec
    mov cl, byte [rsi + bpb.numFATs]
    xor edx, edx    ;Use edx = NumFATs * FATsz temporarily
.cd4:
    add edx, eax
    dec cl
    jnz .cd4
    mov eax, edx    ;Store product in eax
    movzx edx, word [rsi + bpb.revdSecCnt]  ;Get reserved sectors in volume
    add eax, edx
    movzx edx, word [rbp + dpb.wNumberRootDirSectors]
    add eax, edx    ;This adds nothing if FAT32
    ;eax = BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors
    ;aka eax = Start sector of the data area in volume
    mov dword [rbp + dpb.dClusterHeapOffset], eax
;dClusterCount
    sub ebx, eax    ;ebx = Number of sectors in the data area
    mov eax, ebx    ;Move number of sectors in data area into eax
    xor edx, edx
    mov ebx, 1
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl ebx, cl ;Get sectors per cluster
    div ebx ;Data area sector / sectors per cluster = cluster count
    inc eax ;Maximum valid cluster value is eax + 1
    mov dword [rbp + dpb.dClusterCount], eax    ;eax = Cluster count
;dFirstUnitOfRootDir
    cmp eax, fat16MaxClustCnt  ;If above, its FAT32
    mov eax, dword [rsi + bpb32.RootClus]   ;Just save this if FAT32
    ja .cd5
    ;Else, we need to find the first sector of the root directory
    ;Get the start sector of data area in volume 
    ; and sub the number of sectors in the root directory
    mov eax, dword [rbp + dpb.dClusterHeapOffset]
    movzx ebx, word [rbp + dpb.wNumberRootDirSectors]
    sub eax, ebx    ;eax now has start sector of root dir
.cd5:
    mov dword [rbp + dpb.dFirstUnitOfRootDir], eax
;Exit epilogue
    mov rbx, qword [oldRSP]
    mov al, byte [rbx + callerFrame.rax]        ;Return original al value 
    ret
.getVerifySetting:  ;ah = 54h
    mov al, byte [verifyFlag]   ;al is the return value in this case
    ret
.createPSP:         ;ah = 55h, creates a PSP for a program
.renameFile:        ;ah = 56h
.getSetFileDateTime:;ah = 57h
.getsetMallocStrat: ;ah = 58h
.getExtendedError:  ;ah = 59h
.createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
.createNewFile:     ;ah = 5Bh
.lockUnlockFile:    ;ah = 5Ch
.getCritErrorInfo:  ;ah = 5Dh
.networkServices:   ;ah = 5Eh, do nothing
.networkRedirection:;ah = 5Fh, do nothing
.trueName:          ;ah = 60h, get fully qualified name
                    ;ah = 61h, reserved
.getPSPaddr:        ;ah = 62h, gives PSP addr/Process ID
    mov rbx, qword [oldRSP]
    mov rdx, qword [currentPSP]
    mov qword [rbx + callerFrame.rbx], rdx  ;Save the current psp in rbx
    ret
                    ;ah = 63h, reserved
.setDriverLookahead:;ah = 64h, reserved
.getExtLocalInfo:   ;ah = 65h, Get Extended Country Info
.getsetGlobalCP:    ;ah = 66h, Get/Set Global Codepage, reserved
.setHandleCount:    ;ah = 67h
.commitFile:        ;ah = 68h, flushes buffers for handle to disk 
.getsetDiskSerial:  ;ah = 69h, get/set disk serial number
.return:
    ret


.dispatchTable:
    dq .simpleTerminate     ;AH = 00H, PROCESS MANAGEMENT
    dq .stdinReadEcho       ;AH = 01H, CHAR IO
    dq .stdoutWrite         ;AH = 02H, CHAR IO
    dq .stdauxRead          ;AH = 03H, CHAR IO
    dq .stdauxWrite         ;AH = 04H, CHAR IO
    dq .stdprnWrite         ;AH = 05H, CHAR IO
    dq .directCONIO         ;AH = 06H, CHAR IO
    dq .waitDirectInNoEcho  ;AH = 07H, CHAR IO
    dq .waitStdinNoEcho     ;AH = 08H, CHAR IO
    dq .printString         ;AH = 09H, CHAR IO
    dq .buffStdinInput      ;AH = 0AH, CHAR IO
    dq .checkStdinStatus    ;AH = 0BH, CHAR IO
    dq .clearbuffDoFunc     ;AH = 0CH, CHAR IO
    dq .diskReset           ;AH = 0DH, DISK MANAGEMENT
    dq .selectDisk          ;AH = 0EH, DISK MANAGEMENT
    dq .openFileFCB         ;AH = 0FH, FILE OPERATION       FCB
    dq .closeFileFCB        ;AH = 10H, FILE OPERATION       FCB
    dq .findFirstFileFCB    ;AH = 11H, FILE OPERATION       FCB
    dq .findNextFileFCB     ;AH = 12H, FILE OPERATION       FCB
    dq .deleteFileFCB       ;AH = 13H, FILE OPERATION       FCB
    dq .sequentialReadFCB   ;AH = 14H, RECORD OPERATION     FCB
    dq .sequentialWriteFCB  ;AH = 15H, RECORD OPERTAION     FCB
    dq .createFileFCB       ;AH = 16H, FILE OPERATION       FCB
    dq .renameFileFCB       ;AH = 17H, FILE OPERATION       FCB
    dq .return              ;AH = 18H, RESERVED
    dq .getCurrentDisk      ;AH = 19H, DISK MANAGEMENT
    dq .setDTA              ;AH = 1AH, RECORD OPERATION     F/H
    dq .FATinfoDefault      ;AH = 1BH, DISK MANAGEMENT
    dq .FatinfoDevice       ;AH = 1CH, DISK MANAGEMENT
    dq .return              ;AH = 1DH, RESERVED
    dq .return              ;AH = 1EH, RESERVED
    dq .getCurrentDPBptr    ;AH = 1FH, RESERVED INTERNAL, GET CURR DRIVE DPB PTR
    dq .return              ;AH = 20H, RESERVED
    dq .randomReadFCB       ;AH = 21H, RECORD OPERATION     FCB
    dq .randomWriteFCB      ;AH = 22H, RECORD OPERATION     FCB
    dq .getFileSizeFCB      ;AH = 23H, FILE OPERATION       FCB
    dq .setRelRecordFCB     ;AH = 24H, RECORD OPERATION     FCB
    dq .setIntVector        ;AH = 25H, MISC. SYS. FUNCTION
    dq .createNewPSP        ;AH = 26H, PROCESS MANAGEMENT
    dq .randBlockReadFCB    ;AH = 27H, RECORD OPERATION     FCB
    dq .randBlockWriteFCB   ;AH = 28H, RECORD OPERATION     FCB
    dq .parseFilenameFCB    ;AH = 29H, FILE OPERATION       FCB
    dq .getDate             ;AH = 2AH, TIME AND DATE
    dq .setDate             ;AH = 2BH, TIME AND DATE
    dq .getTime             ;AH = 2CH, TIME AND DATE
    dq .setTime             ;AH = 2DH, TIME AND DATE
    dq .setResetVerify      ;AH = 2EH, DISK MANAGEMENT
    dq .getDTA              ;AH = 2FH, RECORD OPERATION     F/H
    dq .getDOSversion       ;AH = 30H, MISC. SYS. FUNCTION
    dq .terminateStayRes    ;AH = 31H, PROCESS MANAGEMENT
    dq .getDeviceDPBptr     ;AH = 32H, RESERVED INTERNAL, GET DEVICE DPB PTR
    dq .ctrlBreakCheck      ;AH = 33H, MISC. SYS. FUNCTION
    dq .getInDOSflagPtr     ;AH = 34H, RESERVED INTERNAL, GET PTR TO INDOS FLAG
    dq .getIntVector        ;AH = 35H, MISC. SYS. FUNCTION
    dq .getDiskFreeSpace    ;AH = 36H, DISK MANAGEMENT
    dq .getsetSwitchChar    ;AH = 37H, RESERVED INTERNAL, CHANGE SWITCH CHAR
    dq .getsetCountryInfo   ;AH = 38H, MISC. SYS. FUNCTION
    dq .makeDIR             ;AH = 39H, DIRECTORY OPERATION
    dq .removeDIR           ;AH = 3AH, DIRECTORY OPERATION
    dq .changeCurrentDIR    ;AH = 3BH, DIRECTORY OPERATION
    dq .createFileHdl       ;AH = 3CH, FILE OPERATION       HANDLE
    dq .openFileHdl         ;AH = 3DH, FILE OPERATION       HANDLE
    dq .closeFileHdl        ;AH = 3EH, FILE OPERATION       HANDLE
    dq .readFileHdl         ;AH = 3FH, RECORD OPERATION     HANDLE
    dq .writeFileHdl        ;AH = 40H, RECORD OPERATION     HANDLE
    dq .deleteFileHdl       ;AH = 41H, FILE OPERATION       HANDLE
    dq .movFileReadPtr      ;AH = 42H, RECORD OPERATION     HANDLE
    dq .changeFileModeHdl   ;AH = 43H, FILE OPERATION       HANDLE
    dq .ioctrl              ;AH = 44H, MISC. SYS. FUNCTION
    dq .duplicateHandle     ;AH = 45H, FILE OPERATION       HANDLE
    dq .forceDuplicateHdl   ;AH = 46H, FILE OPERATION       HANDLE
    dq .getCurrentDIR       ;AH = 47H, DIRECTORY OPERATION
    dq .allocateMemory      ;AH = 48H, MEMORY MANAGEMENT
    dq .freeMemory          ;AH = 49H, MEMORY MANAGEMENT
    dq .reallocMemory       ;AH = 4AH, MEMORY MANAGEMENT
    dq .loadExecChild       ;AH = 4BH, PROCESS MANAGEMENT
    dq .terminateClean      ;AH = 4CH, PROCESS MANAGEMENT
    dq .getRetCodeChild     ;AH = 4DH, PROCESS MANAGEMENT
    dq .findFirstFileHdl    ;AH = 4EH, FILE OPERATION       HANDLE
    dq .findNextFileHdl     ;AH = 4FH, FILE OPERATION       HANDLE
    dq .setCurrProcessID    ;AH = 50H, RESERVED INTERNAL, SET CURRENT PROCESS ID
    dq .getCurrProcessID    ;AH = 51H, RESERVED INTERNAL, GET CURRENT PROCESS ID
    dq .getSysVarsPtr       ;AH = 52H, RESERVED INTERNAL, GET SYSVARS POINTER
    dq .createDPB           ;AH = 53H, RESERVED INTERNAL, TRANSLATE A BPB TO DPB
    dq .getVerifySetting    ;AH = 54H, DISK MANAGEMENT
    dq .createPSP           ;AH = 55H, RESERVED INTERNAL, CREATE A PSP
    dq .renameFile          ;AH = 56H, FILE OPERATION       HANDLE
    dq .getSetFileDateTime  ;AH = 57H, FILE OPERATION       HANDLE
    dq .getsetMallocStrat   ;AH = 58H, MEMORY MANAGEMENT
    dq .getExtendedError    ;AH = 59H, MISC. SYS. FUNCTION
    dq .createUniqueFile    ;AH = 5AH, FILE OPERATION       HANDLE
    dq .createNewFile       ;AH = 5BH, FILE OPERATION       HANDLE
    dq .lockUnlockFile      ;AH = 5CH, RECORD OPERATION     HANDLE
    dq .getCritErrorInfo    ;AH = 5DH, RESERVED INTERNAL, GET CRIT. ERROR DATA
    dq .networkServices     ;AH = 5EH, RESERVED NETWORK FUNCTION
    dq .networkRedirection  ;AH = 5FH, RESERVED NETWORK FUNCTION
    dq .trueName            ;AH = 60H, RESERVED INTERNAL, GET TRUE NAME
    dq .return              ;AH = 61H, RESERVED
    dq .getPSPaddr          ;AH = 62H, PROCESS MANAGEMENT
    dq .return              ;AH = 63H, RESERVED
    dq .setDriverLookahead  ;AH = 64H, RESERVED INTERNAL, DRIVER LOOKAHEAD
    dq .getExtLocalInfo     ;AH = 65H, MISC. SYS. FUNCTION
    dq .getsetGlobalCP      ;AH = 66H, MISC. SYS. FUNCTION
    dq .setHandleCount      ;AH = 67H, FILE OPERAITON       F/H
    dq .commitFile          ;AH = 68H, FILE OPERATION       HANDLE
    dq .getsetDiskSerial    ;AH = 69H, RESERVED INTERNAL, GET/SET DISK SER. NUM
dispatchTableL  equ $ - .dispatchTable 

terminateHandler:   ;Int 42h
ctrlCHandler:       ;Int 43h
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
    lea rdx, qword [.crlf]
    mov ah, 09h ;Print String
    int 41h     ;Call DOS to print CRLF part of message

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
    lea rdx, qword [.crlf]  ;Print new line
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
.crlf       db 0Ah, 0Dh, "$"
.abortmsg   db "Abort$" 
.ignoremsg  db "Ignore$"
.retrymsg   db "Retry$"
.failmsg    db "Fail$"
.betweenMsg db ", $"
.endMsg     db "? $"
.responses  db "IRAF"   ;Abort Retry Ignore Fail
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address
;ecx = Number of sectors to read (max 255 for now)
;rdx = Start LBA to read from
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 82h
    int 33h
    iretq
absDiskWrite:       ;Int 46h
    movzx rax, al   ;Zero extend DOS drive number 
    mov al, byte [msdDriver.msdBIOSmap + rax] ;Get translated BIOS num into al
    xchg rax, rcx
    xchg rcx, rdx
    mov ah, 83h
    int 33h
    iretq
terminateResident:  ;Int 47h
inDosHandler:       ;Int 48h
;Called when DOS idle
    iretq
fastOutput:         ;Int 49h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
passCommand:        ;Int 4Eh, hooked by COMMAND.COM
    iretq
multiplex:          ;Int 4Fh, kept as iretq for now
    iretq