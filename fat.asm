;-----------------------------------:
;       File System routines        :
;-----------------------------------:
name2Clust:
;Converts a file name to a first cluster number
;Converts the whole path subdirectory by subdirectory
;On entry: rbx = ASCIIZ string of file path
;On return: ebx = First cluster number for the file
;   On error: CF=CY, ax = Error code

;First ascertain path is valid path and get drive letter
    push rsi
    push rcx
    mov rsi, rbx
    xor al, al
    scasb
    sub rsi, rbx    ;Get # of chars in string
    dec rsi
    mov rcx, rsi
    mov rsi, rbx
    cmp byte [rsi + 1], ":" ;Check it is a colon for full path
    jne .cdsPath    ;Use CDS for current dir
.getSubDir:
    mov al, "\"
    mov 
.cdsPath:
    pop rcx
    pop rsi
    ret

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
;This function will return a pointer to the desired data sector OR 
; find the most appropriate buffer, flush and read the relevant data into the 
; buffer, returning a pointer to the sector buffer in rbx.
;Entry: rax = Sector to read
;        cl = Data type being read (DOS, FAT, DIR, Data) 
;       rsi = DPB of transacting drive
;Exit:  CF = NC : All ok!
;       rbx = Pointer to buffer header with valid data in buffer.
;       All other registers as before
;       CF = CY: Something went wrong, return error code or INT 44h
;       ch = 0 -> Data Not Flushed To Disk
;       ch = 1 -> Data Not Read From Disk
;       rbx = Pointer to buffer containing sector without valid data in buffer ;            (either unflushed or unread)
    push rdx
    mov dl, byte [rsi + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rbx, -1
    je .rbReadNewSector
.rbExit:
    clc
.rbExitNoFlag:
    pop rdx
    ret
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rbx
    xor ch, ch
    call flushBuffer
    jc .rbExitNoFlag    ;Exit in error
;rbx points to bufferHdr that has been appropriately linked to the head of chain
    push rcx
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
    pop rcx
    inc ch  ;If an error occurs, have the signature in ch
    call readSector ;Carry the flag from the request
    jmp short .rbExitNoFlag

readSector:
;Reads a sector into a built sector buffer
;Entry: rbx = Pointer to buffer header
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;       rbx pointing to buffer header
;First make request to device driver
    push rax
    push rcx
    push rdx
    push rsi
;Build a request block in diskReqHdr
    mov rsi, qword [rbx + bufferHdr.driveDPBPtr]    ;Get dpbptr in rsi
.rsRequest0:
    mov cl, 3  ;Repeat attempt counter
.rsRequest1:
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov qword [diskReqHdr + ioReqPkt.devptr], 0
    mov dword [diskReqHdr + ioReqPkt.tfrlen], 1 ;One sector
    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.cmdcde], drvREAD
    mov rax, qword [rbx + bufferHdr.bufferLBA]      ;Get sector num in rax
    mov qword [diskReqHdr + ioReqPkt.strtsc], rax   ;Save rax in request block
    lea rax, qword [rbx + bufferHdr_size]           ;Get addr of buffer in rax
    mov qword [diskReqHdr + ioReqPkt.bufptr], rax   ;Save rax in request block
    mov al, byte [rsi + dpb.bUnitNumber]
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov al, byte [rsi + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov rdx, qword [rsi + dpb.qDriverHeaderPtr] ;Get pointer to driver header

    push rbx
    lea rbx, diskReqHdr
    call qword [rdx + drvHdr.strPtr]  ;Call with ptr to the request block in rbx
    call qword [rdx + drvHdr.intPtr]
    pop rbx
    test word [diskReqHdr + ioReqPkt.status], 8000h  ;Test error bit
    jnz .rsFail
.rsExit:
    clc
.rsExitBad:
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret
.rsFail:
;Enter here only if the request failed
    dec cl
    jnz .rsRequest1 ;Try the request again!
;Request failed thrice, critical error call
    stc
    jmp .rsExitBad  ;Abort

flushBuffer:
;Flushes the data in a sector buffer to disk!
;Entry: rbx = Pointer to buffer header for this buffer
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;       rbx preserved pointing to data buffer
;First make request to device driver
    push rax
    push rcx
    push rdx
    push rsi
    test byte [rbx + bufferHdr.bufferFlags], dirtyBuffer    ;Data modified?
    jz .fbFreeExit  ;Skip write to disk if data not modified
;Build a request block in diskReqHdr
    mov rsi, qword [rbx + bufferHdr.driveDPBPtr]    ;Get dpbptr in rsi
.fbRequest0:
    mov cl, 3  ;Repeat attempt counter
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
    push rbx
    lea rbx, diskReqHdr
    call qword [rdx + drvHdr.strPtr] ;Call with ptr to the request block in rbx
    call qword [rdx + drvHdr.intPtr]
    pop rbx
    test word [diskReqHdr + ioReqPkt.status], 8000h  ;Test error bit
    jnz .fbFail
;Now check if the buffer was a FAT, to write additional copies
    test byte [rbx + bufferHdr.bufferFlags], fatBuffer ;FAT buffer?
    jz .fbFreeExit  ;If not, exit
    dec byte [rbx + bufferHdr.bufFATcopy]
    jz .fbFreeExit  ;Once this goes to 0, stop writing FAT copies
    mov eax, dword [rbx + bufferHdr.bufFATsize]
    add qword [rbx + bufferHdr.bufferLBA], rax ;Add the FAT size to the LBA
    jmp .fbRequest0 ;Make another request
.fbFreeExit:
;Free the buffer if it was flushed successfully
    mov byte [rbx + bufferHdr.driveNumber], -1
    clc
.fbExitBad:
    pop rsi
    pop rdx
    pop rcx
    pop rax
    ret
.fbFail:
;Enter here only if the request failed
    dec cl
    jnz .fbRequest1 ;Try the request again!
;Request failed thrice, critical error call
    stc
    jmp .fbExitBad  ;Abort