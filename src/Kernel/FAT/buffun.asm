;This file contains FAT disk buffer related functions that
; dont fit anywhere else. These functions form a part of the FAT driver
;----------------------------------------------------
;           Externally referenced functions         :
;----------------------------------------------------

makeBufferMostRecentlyUsed: ;Int 2Fh AX=1207h
;Sets the buffer in rdi to the head of the chain
;Input: rdi = Buffer header to move to the head of the chain
;Output: Buffer header set to the head of the chain
    cmp qword [bufHeadPtr], rdi ;Is buffer already at the head?
    rete
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
    return

markBuffersAsUnreferencedWrapper:
;Marks all buffers as unreferenced (clears the reference bit from all buffers)
; and preserves rdi
    push rdi
    call markBuffersAsUnreferenced
    pop rdi
    return

markBuffersAsUnreferenced:  ;Int 2Fh AX=120Eh
;Marks all buffers as unreferenced (clears the reference bit from all buffers)
;Output: rdi = First disk buffer
    mov rdi, qword [bufHeadPtr]
    push rdi
.lp:
    and byte [rdi + bufferHdr.bufferFlags], ~refBuffer
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    cmp rdi, -1 ;End of chain?
    jne .lp
    pop rdi
    return

makeBufferMostRecentlyUsedGetNext: ;Int 2Fh AX=120Fh
;Sets the buffer in rdi to the head of the chain and gets the 
; second buffer in the chain in rdi
;Input: rdi = Buffer header to move to the head of the chain
;Output: rdi = Second buffer in the chain
    push rdx
    mov rdx, qword [rdi + bufferHdr.nextBufPtr] ;Save next ptr in rdx
    call makeBufferMostRecentlyUsed ;Make the buffer most recently used
    mov rdi, rdx    ;Continue searching from where we left off
    pop rdx
    return

findUnreferencedBuffer: ;Int 2Fh AX=1210h
;Finds the first unreferenced buffer starting at the given buffer header.
;Input: rdi = Buffer header to start searching at
;Output: ZF=NZ => rdi = Unreferenced Buffer Header
;        ZF=ZE => No unreferenced buffer found
    cmp rdi, -1 ;Start by checking rdi is not at the end of the list already :)
    rete    ;Return preserving ZF is so
    test byte [rdi + bufferHdr.bufferFlags], refBuffer
    jz .exit    ;Unreferenced buffer found!
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    jmp short findUnreferencedBuffer  ;Check next buffer
.exit:
    push rax
    or eax, 1   ;Clear ZF
    pop rax
    return

flushAllBuffersForDPB:  ;External linkage
;Wrapper to allow calls to the below functions from the workingDPB
    push rax
    mov rax, qword [workingDPB]
    movzx eax, byte [rax + dpb.bDriveNumber]
    call flushAllBuffersForDrive
    pop rax
    return

flushAllBuffersForDrive:    ;External linkage (2 - diskReset/exit)
; Flushes and resets the dirty bit for all dirty bufs in buffer chain.
; Used also to allow flushing all buffers
; Input: al = 0-based physical drive number we are xacting on
; Returns: CF=NC => All is well, buffers flushed and dirty bits cleaned
;          CF=CY => A Buffer failed to flush, it was lost.
    push rdi
    push rax
    mov rdi, qword [bufHeadPtr]
    mov ah, -1  ;Set to ignore no buffers
.mainLp:
    cmp rdi, -1 ;When we get to the end of the buffer chain, exit
    je .exit   
    call flushAndCleanBuffer    ;Flush this buffer if it is on the DPB we want
    push rax
    movzx eax, byte [rdi + bufferHdr.driveNumber]
    cmp al, byte [errorDrv] ;Was this a buffer on the error drive?
    pop rax
    je .errDrv    ;If not, goto next buffer
.nextBuffer:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr] ;Goto next buffer
    jmp short .mainLp
.errDrv:
    mov byte [rdi + bufferHdr.driveNumber], -1  ;Free the buffer if caused error
    jmp short .nextBuffer
.exit:
    pop rax
    pop rdi
    test byte [Int24Fail], -1   ;Did we xlat error?
    retz
    stc ;If so, return CF=CY
    return

flushAndCleanBuffer:   ;Internal Linkage Int 2Fh AX=1215h
;Flushes and cleans the dirty bit from the buffer
;Input: AH = Drives to ignore flush for
;       AL = Drive to flush for. If AL=AH, take this buffer unless ignored!
;       rdi -> Buffer in question
    cmp ah, byte [rdi + bufferHdr.driveNumber]  ;Is this an ignorable drv number?
    rete
    cmp ah, al  ;Do we ignore ourselves?
    je .takeThisBuffer
    cmp al, byte [rdi + bufferHdr.driveNumber] ;Is this an acceptable buffer?
    clc
    retne
.takeThisBuffer:
    test byte [rdi + bufferHdr.bufferFlags], dirtyBuffer   ;Is it dirty?
    retz
    push rax    ;Save the drive number
    movzx eax, word [rdi + bufferHdr.wDrvNumFlg]
    push rax    ;Save the buffer flags
    call flushAndFreeBuffer
    pop rax     ;Get back the flags
    jc .exit
    and ah, ~dirtyBuffer    ;Clean the dirty bit
    mov word [rdi + bufferHdr.wDrvNumFlg], ax  ;And return the drv/flags!
.exit:  
    pop rax     ;Get back the drive number
    return

flushAndFreeBuffer:    ;Int 2Fh AX=1209h 
;Frees, then attempts flushes the data in a sector buffer to disk!
;Entry: rdi = Pointer to buffer header for this buffer
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rbp
;If the buffer is freed, skip flushing to avoid issues
    mov eax, freeBuffer
    xchg ax, word [rdi + bufferHdr.driveNumber] ;Free the buffer, get flags
    ;ah = Flags, al = Drive number
    cmp al, -1  ;-1 means free buffer
    je .fbFreeExit  ;If it was free, exit
    test ah, dirtyBuffer    ;Data modified?
    jz .fbFreeExit  ;Skip write to disk if data not modified
    cmp al, byte [errorDrv] ;Was this drive the error drive?    
    je .fbFreeExit  ;Skip write if this disk has caused an error
    mov byte [Int24bitfld], critWrite | critRetryOK | critFailOK
    test ah, dataBuffer
    jz .fbWriteSetup
    or byte [Int24bitfld], critIgnorOK  ;If this is a data buffer, we can ignore too
.fbWriteSetup:
    mov esi, 3  ;Repeat attempt counter
    test ah, fatBuffer
    jz .fbWriteNotFat
    add esi, 2  ;FAT sectors have 5 attempts
.fbWriteNotFat:
    movzx ecx, byte [rdi + bufferHdr.bufFATcopy]   ;And FAT copies (if FAT sector)
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    lea rbx, qword [rdi + bufferHdr.dataarea]
    mov rbp, qword [rdi + bufferHdr.driveDPBPtr]
.fbWriteDisk:
    push rax    ;Save the drive number and flags
    push rbx    ;Has pointer to buffer data area
    push rcx    ;# of FAT sectors
    push rdx    ;The LBA of the buffer that we are writing
    push rsi    ;Error counter (5 for FAT sectors, 3 otherwise) 
    push rbp    ;DPB ptr for drive
    mov ecx, 1  ;One sector to copy
    call primReqWriteSetup  ;Setup request (preserves setup registers)
    call absDiskDriverCall    ;Make Driver Request
    pop rbp
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    ;Don't pop rax here to carry the error code if error!
    jnz .fbFail
;Now check if the buffer was a FAT, to write additional copies
    pop rax ;Now pop the drive number and flags off the stack
    test ah, fatBuffer ;FAT buffer?
    jz .fbFreeExit  ;If not, exit
    dec ecx
    jz .fbFreeExit  ;Once this goes to 0, stop writing FAT copies
    push rax
    mov eax, dword [rdi + bufferHdr.bufFATsize]
    add rdx, rax ;Add the FAT size to the LBA (rdx has LBA number)
    pop rax
    jmp short .fbWriteDisk ;Make another request for the other FAT copy
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
    jz .fbHardError ;Once we have tried it a number of times, fail!
    pop rax     ;Else pop back the drive number and flags
    jmp short .fbWriteDisk ;Try the request again!
.fbHardError:
;Request failed thrice, critical error call
;At this point, ax = Error code, rbp -> DPB, rdi -> Buffer code
    or byte [Int24bitfld], critWrite ;Set the initial bitfield to write req
    call diskIOError ;Call with rdi = Buffer header and eax = Status Word
    cmp al, critRetry
    pop rax     ;Now pop back the drive number and flags from the stack!
    je .fbWriteSetup   ;If we retry, we rebuild the stack, values possibly trashed
    ;Else we fail (Ignore=Fail here)
    stc ;Set error flag to indicate fail
    jmp short .fbExitFail

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

freeBuffersForDrive:  ;External Linkage (Before Get BPB in medchk)
;Walks the buffer chain and sets ALL buffers with the given DPB 
; to have a drive number of -1, thus freeing it
;Given Drive number is in al
    push rdi
    mov rdi, qword [bufHeadPtr]
.i0:
    cmp rdi, -1
    je .exit
    cmp byte [rdi + bufferHdr.driveNumber], al  ;Chosen Drive?
    jne .i1 ;If no, skip freeing
    mov word [rdi + bufferHdr.driveNumber], freeBuffer | (refBuffer << 8)
    call makeBufferMostRecentlyUsedGetNext
    jmp short .i0
.i1:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr] ;goto next buffer
    jmp short .i0
.exit:
    pop rdi
    return

markBufferDirty:
    push rbp
    pushfq
    mov rbp, qword [currBuff]
    or byte [rbp + bufferHdr.bufferFlags], dirtyBuffer
    popfq
    pop rbp
    return

getBuffer: ;Internal Linkage ONLY
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
;       rsi = DPB to use for transaction
;Exit:  CF=NC => 
;           Ptr to buffer header with valid data in buffer in rbx and [currBuf]
;       CF=CY => Critical Error returned Fail, rbx undefined
    push rcx
    push rdx
    push rsi
    push rdi
    mov dl, byte [rsi + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rdi, -1 ;Get in rdi the buffer ptr
    je .rbReadNewSector
    mov qword [currBuff], rdi   ;Save the found buffer ptr in the variable
    call makeBufferMostRecentlyUsed
    clc
.rbExit:
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pushfq
    mov rbx, qword [currBuff]   ;Get current buffer
    or byte [rbx + bufferHdr.bufferFlags], refBuffer    ;Mark as referenced!
    popfq
    return
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rdi
    call flushAndFreeBuffer
    jc .rbExit    ;Preserve the set carry flag
;rdi points to bufferHdr that has been appropriately linked to the head of chain
    ;If the sector is to be lost or has been successfully flushed, then it
    ; is no longer owned by that File so we mark the owner as none
    mov byte [rdi + bufferHdr.driveNumber], dl
    mov byte [rdi + bufferHdr.bufferFlags], cl ;FAT/DIR/DATA and NOT dirty
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
    jmp short .rbExit   ;Jump preserving the carry flag

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
    lea rbx, qword [rdi + bufferHdr.dataarea]
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
;First free the buffer if we failed to read data into it. 
;We free this buffer to free the resource if the user aborts.
;This function is called in a critical section so the buffer pointer
; is under no thread of being reallocated.
;At this point, ax = Error code, rbp -> DPB, rdi -> Buffer code
    mov word [rdi + bufferHdr.driveNumber], freeBuffer ;Free buffer
    mov byte [Int24bitfld], critRead | critFailOK | critRetryOK
    call diskIOError    ;Returns rbp -> DPB and rdi -> Buffer, al = Action code
    cmp al, critRetry
    jne .fail   ;Else we fail
    movzx eax, byte [rbp + dpb.bDriveNumber]    ;Get drv num to put back
    mov byte [rdi + bufferHdr.driveNumber], al ;Put it back (buffer type bits set)
    jmp short .rsRequest0
.fail:
    stc ;Set error flag to indicate fail
    jmp .rsExitFail
    
findLRUBuffer: ;Internal Linkage
;Finds first free or least recently used buffer, links it and returns ptr to it 
; in rdi and the currBuff variable
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
;Finds the Buffer for a sector belonging to a particular dpb device
;If the sector is not in a buffer, returns with a -1
;Input: rax = Sector number
;       dl = Drive number
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

;-----------------------------------------------------------------------------
;SPECIAL BUFFER FUNCTIONS
;Buffer functions for sectors associated to file handles and specific purposes
; ALL sector types need to setup [workingDPB] to make the transfer
;-----------------------------------------------------------------------------
getBufForDOS:
;Returns a buffer to use for DOS sector(s) in rbx
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dosBuffer
    jmp short getBufCommon
getBufForFat:
;Returns a buffer to use for fat data in rbx
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, fatBuffer
    jmp short getBufCommon
getBufForDir:
;Returns a buffer to use for disk directory data in rbx
;Input: [workingDPB] = File to manipulate
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dirBuffer
    jmp short getBufCommon
getBufForData:
;Returns a buffer to use for disk data in rbx
;Requires a File Handle.
;Input: [workingDPB] = File to manipulate
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dataBuffer
getBufCommon:
    push rsi
    push rdi
    mov rsi, qword [workingDPB] ;Get working DPB 
.makeReq:
    call getBuffer  ;Gives the buffer ptr in rbx
    pop rdi
    pop rsi
    pop rcx
    return
