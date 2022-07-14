;This file contains FAT disk buffer related functions that
; dont fit anywhere else. These functions form a part of the FAT driver
;----------------------------------------------------
;           Externally referenced functions         :
;----------------------------------------------------

makeBufferMostRecentlyUsed: ;Int 4Fh AX=1207h
;Sets the buffer in rdi to the head of the chain
;Input: rdi = Buffer header to move to the head of the chain
;Output: Buffer header set to the head of the chain
    cmp qword [bufHeadPtr], rdi ;Is buffer already at the head?
    je .exit
    push rsi
    mov rsi, qword [bufHeadPtr] ;Go to the head of the pointer
.mainlp:
    cmp qword [rsi + bufferHdr.nextBufPtr], rdi ;Is the next buffer ours?
    je .fnd  ;Found the buffer as the next buffer in the chain
    mov rsi, qword [rsi + bufferHdr.nextBufPtr]   ;Goto next buffer
    jmp short .mainlp
.fnd:
    push rdi
    mov rdi, qword [rdi + bufferHdr.nextBufPtr] ;Get next buffer from rdi in rdi
    mov qword [rsi + bufferHdr.nextBufPtr], rdi ;Set prev buf to goto next buf
    pop rdi ;Get original buf back
    mov rsi, rdi    ;Save new head buf in rsi
    xchg qword [bufHeadPtr], rsi ;Set rsi to head, get new 2nd buf in rsi
    mov qword [rdi + bufferHdr.nextBufPtr], rsi ;Set 2nd buf to rsi
    pop rsi
.exit:
    return

flushAndFreeBuffer:         ;Int 4Fh AX=1209h
    call flushBuffer
    jnc .exit
    ;Free the buffer if it was flushed successfully (CF=NC)
    mov word [rdi + bufferHdr.driveNumber], 00FFh   ;Free buffer and clear flags
.exit:
    return

markBuffersAsUnreferenced:  ;Int 4Fh AX=120Eh
;Marks all buffers as unreferenced (clears the reference bit from all buffers)
;Output: rdi = First disk buffer
    mov rdi, [bufHeadPtr]
    push rdi
.lp:
    and byte [rdi + bufferHdr.bufferFlags], ~refBuffer
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    cmp rdi, -1 ;End of chain?
    jne .lp
    pop rdi
    return

makeBufferMostRecentlyUsedGetNext: ;Int 4Fh AX=120Fh
;Sets the buffer in rdi to the head of the chain and gets the 
; second buffer in the chain in rdi
;Input: rdi = Buffer header to move to the head of the chain
;Output: rdi = Second buffer in the chain
    call makeBufferMostRecentlyUsed
    mov rdi, qword [bufHeadPtr]
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    return

findUnreferencedBuffer: ;Int 4Fh AX=1210h
;Finds the first unreferenced buffer starting at the given buffer header.
;Input: rdi = Buffer header to start searching at
;Output: ZF=NZ => rdi = Unreferenced Buffer Header
;        ZF=ZE => No unreferenced buffer found
    test byte [rdi + bufferHdr.bufferFlags], refBuffer
    jnz .exit
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    cmp rdi, -1
    jne findUnreferencedBuffer  ;Check next buffer unless rdi = -1
.exit:
    return

flushBuffer:         ;Internal Linkage Int 4Fh AX=1215h
;Flushes the data in a sector buffer to disk!
;Entry: rdi = Pointer to buffer header for this buffer
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;First make request to device driver
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
    test byte [rdi + bufferHdr.bufferFlags], dirtyBuffer    ;Data modified?
    jz .fbFreeExit  ;Skip write to disk if data not modified
.fbRequest0:
    mov esi, 3  ;Repeat attempt counter
    test byte [rdi + bufferHdr.bufferFlags], fatBuffer
    jz .fbRequest1
    add esi, 2  ;FAT sectors have 5 attempts
.fbRequest1:
    mov al, byte [rdi + bufferHdr.driveNumber]
    mov ecx, 1  ;One sector to copy
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    mov rbx, qword [rdi + bufferHdr.dataarea]
    mov rbp, qword [rdi + bufferHdr.driveDPBPtr]
    call primReqWriteSetup  ;Setup request (preserves setup registers)
    call absDiskDriverCall    ;Make Driver Request
    jnz .fbFail
;Now check if the buffer was a FAT, to write additional copies
    test byte [rdi + bufferHdr.bufferFlags], fatBuffer ;FAT buffer?
    jz .fbFreeExit  ;If not, exit
    dec byte [rdi + bufferHdr.bufFATcopy]
    jz .fbFreeExit1  ;Once this goes to 0, stop writing FAT copies
    mov eax, dword [rdi + bufferHdr.bufFATsize]
    add qword [rdi + bufferHdr.bufferLBA], rax ;Add the FAT size to the LBA
    jmp .fbRequest0 ;Make another request
.fbFreeExit1:
    mov bl, byte [rbp + dpb.bNumberOfFATs]
    mov byte [rdi + bufferHdr.bufFATcopy], bl    ;Just in case, replace this
.fbFreeExit:
    clc
.fbExitFail:
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return
.fbFail:
;Enter here only if the request failed
    dec esi
    jnz .fbRequest1 ;Try the request again!
;Request failed thrice, critical error call
    mov byte [Int44bitfld], critWrite ;Set the initial bitfield to write req
    call diskDevErr ;Call with rdi = Buffer header and eax = Status Word
    cmp al, critRetry
    je .fbRequest0
    ;Else we fail (Ignore=Fail here)
    stc ;Set error flag to indicate fail
    jmp .fbExitFail


testDirtyBufferForDrive:    ;External linkage
;Searches the buffer chain for a dirty buffer for a given drive letter.
;Input: al = Drive number (0 based)
;Output: CF=CY => Dirty buffer found, CF=NC => No dirty buffer found
;Consequently, ZF=NZ => Dirty buffer found, ZF=ZE => No dirty buffer found
    push rbx
    mov rbx, qword [bufHeadPtr]
.tdbfdCheckBuffer:
    cmp byte [rbx + bufferHdr.driveNumber], al
    jne .tdbfdGotoNextBuffer
    test byte [rbx + bufferHdr.bufferFlags], dirtyBuffer
    jz .tdbfdGotoNextBuffer ;Bit not set, goto next buffer
    stc ;Else dirty buffer found, set carry flag
.tdbfdExit:
    pop rbx
    return
.tdbfdGotoNextBuffer:
    mov rbx, qword [rbx + bufferHdr.nextBufPtr]
    cmp rbx, -1     ;If rbx points to -1, exit (Also clears CF)
    je .tdbfdExit
    jmp short .tdbfdCheckBuffer

freeBuffersForDPB:
;Walks the buffer chain and sets ALL buffers with the given DPB 
; to have a drive number of -1, thus freeing it
;Given DPB is in rbp
    push rbx
    mov rbx, qword [bufHeadPtr]
.i0:
    cmp qword [rbx + bufferHdr.driveDPBPtr], rbp  ;Chosen DPB?
    jne .i1 ;If no, skip freeing
    mov word [rbx + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
.i1:
    mov rbx, qword [rbx + bufferHdr.nextBufPtr] ;goto next buffer
    cmp rbx, -1
    jne .i0
.exit:
    pop rbx
    return

getBuffer: ;External Linkage (dosPrim.asm, fat.asm)
;
;WHENEVER A DATA BUFFER IS NEEDED FOR SECTOR DATA, THIS IS THE FUNCTION
;TO CALL! WORKS ON THE WORKING DPB!
;Flushes if not in disk change mode. 
;If in disk change mode, will check to see if the selected buffer 
; has data referring a device using the same DPB as we are changing.
; If it is, skip flushing, if not, flush first!
;
;This function will return a pointer to the desired data sector OR 
; find the most appropriate buffer, flush and read the relevant data into the 
; buffer, again then returning a pointer to the sector buffer in rbx.
;Entry: rax = Sector to read
;        cl = Data type being read (DOS, FAT, DIR, Data)
;       qword [workingDPB] = DPB to use for transaction
;Exit:  CF=NC => 
;           Ptr to buffer header with valid data in buffer in rbx and [currBuf]
;       CF=CY => Critical Error returned Fail, rbx undefined
    push rcx
    push rdx
    push rsi
    push rdi
    mov rsi, qword [workingDPB]  ;Get DPB of transacting device
    mov dl, byte [rsi + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rdi, -1
    je .rbReadNewSector
.rbExit:
    clc
.rbExitNoFlag:
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    mov rbx, qword [currBuff]   ;Get current buffer
    return
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rdi
    cmp byte [diskChange], -1 ;Are we in disk change?
    jne .flush  ;We are not, flush buffer
    cmp rsi, qword [rdi + bufferHdr.driveDPBPtr]    ;If yes...
    je .skipFlush   ;Avoid flushing if same DPB being used
.flush:
    call flushAndFreeBuffer
    jc .rbExitNoFlag    ;Preserve the set carry flag
.skipFlush:
;rdi points to bufferHdr that has been appropriately linked to the head of chain
    mov byte [rdi + bufferHdr.driveNumber], dl
    mov byte [rdi + bufferHdr.bufferFlags], cl ;FAT/DIR/DATA
    mov qword [rdi + bufferHdr.bufferLBA], rax
    cmp cl, fatBuffer
    mov dl, 1   ;Default values if not fat buffer
    jne .rbNonFATbuffer
    mov dl, byte [rsi + dpb.bNumberOfFATs]
.rbNonFATbuffer:
    mov byte [rdi + bufferHdr.bufFATcopy], dl
    mov edx, dword [rsi + dpb.dFATlength]
    mov dword [rdi + bufferHdr.bufFATsize], edx
    mov qword [rdi + bufferHdr.driveDPBPtr], rsi
    mov byte [rdi + bufferHdr.reserved], 0
    call readSectorBuffer ;Carry the flag from the request
    jmp short .rbExitNoFlag

readSectorBuffer:   ;Internal Linkage
;Reads a sector into a built sector buffer
;Entry: rdi = Pointer to buffer header
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
;       rbx pointing to buffer header
;First make request to device driver
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
.rsRequest0:
    mov esi, 3  ;Repeat attempt counter
    test byte [rdi + bufferHdr.bufferFlags], fatBuffer
    jz .rsRequest1
    add esi, 2  ;FAT sectors have 5 attempts
.rsRequest1:
    mov al, byte [rdi + bufferHdr.driveNumber]
    mov ecx, 1  ;One sector to copy
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    mov rbx, qword [rdi + bufferHdr.dataarea]
    mov rbp, qword [rdi + bufferHdr.driveDPBPtr]
    call primReqReadSetup  ;Setup request (preserves setup registers)
    call absDiskDriverCall    ;Make Driver Request
    jnz .rsFail
.rsExit:
    clc
.rsExitFail:
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return
.rsFail:
;Enter here only if the request failed
    dec esi
    jnz .rsRequest1 ;Try the request again!
;Request failed thrice, critical error call
    mov byte [Int44bitfld], critRead    ;Set the initial bitfield to read req
    call diskDevErr
    cmp al, critRetry
    je .rsRequest0
    ;Else we fail (Ignore=Fail here)
    stc ;Set error flag to indicate fail
    jmp .rsExitFail
    
findLRUBuffer: ;Internal Linkage
;Finds first free or least recently used buffer, links it and returns ptr to it 
; in rbx and the currBuff variable
;Input: Nothing
;Output: rdi = Pointer to the buffer hdr to use
;       [currBuff] = Pointer to the buffer hdr to use
    push rdx
    mov rdi, qword [bufHeadPtr]
    cmp byte [rdi + bufferHdr.driveNumber], -1  ;Check if 1st entry is free
    je .flbExit 
    cmp qword [rdi + bufferHdr.nextBufPtr], -1  ;Check if 1st entry is last
    je .flbExit
.flbWalk:
    mov rdx, rdi    ;Save a ptr to the previous buffer header
    mov rdi, qword [rdx + bufferHdr.nextBufPtr] ;Get next buffer header ptr
    cmp byte [rdi + bufferHdr.driveNumber], -1
    je .flbFreeLink ;If free, link to head, and xlink prev and next buffs
    cmp qword [rdi + bufferHdr.nextBufPtr], -1 ;Check if at LRU buffer
    jne .flbWalk   ;If not LRU, keep walking, else process
    mov qword [rdx + bufferHdr.nextBufPtr], -1  ;Make prev node the LRU node
.flbHeadLink:
    mov rdx, qword [bufHeadPtr]    ;Now copy old MRU buffer ptr to rdx
    mov qword [bufHeadPtr], rdi    ;Sysvars to point to new buffer
    mov qword [rdi + bufferHdr.nextBufPtr], rdx
.flbExit:
    pop rdx
    mov qword [currBuff], rdi   ;Save in variable too
    return
.flbFreeLink:
    push rcx
    mov rcx, qword [rdi + bufferHdr.nextBufPtr]
    mov qword [rdx + bufferHdr.nextBufPtr], rcx  ;Point prev buff past rdi
    pop rcx
    jmp short .flbHeadLink

findSectorInBuffer:     ;Internal linkage
;Finds the Buffer for a sector
;If the sector is not in a buffer, returns with a -1
;Input: rax = Sector number
;        dl = Drive number
;Output: rdi = Buffer hdr pointer or -1
    mov rdi, qword [bufHeadPtr]
.fsiCheckBuffer:
    cmp byte [rdi + bufferHdr.driveNumber], dl
    jne .fsiGotoNextBuffer
    cmp qword [rdi + bufferHdr.bufferLBA], rax
    jne .fsiGotoNextBuffer
.fsiExit:
    return
.fsiGotoNextBuffer:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    cmp rdi, -1     ;If rdi points to -1, exit
    je .fsiExit
    jmp short .fsiCheckBuffer
