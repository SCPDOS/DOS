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
    je .errDrv      ;If not, goto next buffer
.nextBuffer:        ;This handles the case if the user aborts or ignores.
    mov rdi, qword [rdi + bufferHdr.nextBufPtr] ;Goto next buffer
    jmp short .mainLp
.errDrv:
;Free the buffer if it caused an error in this DOS call and was aborted
; or ignored.
    mov byte [rdi + bufferHdr.wDrvNumFlg], freeBuffer
    jmp short .nextBuffer
.exit:
    pop rax
    pop rdi
    test byte [Int24Fail], -1   ;Did we return fail at any point?
    retz
    stc     ;If so, carry the CF=CY
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
;Flushes the data in a sector buffer to disk, then frees it!
;Entry: rdi = Pointer to buffer header for this buffer
;Exit:  CF=NC : Success
;       CF=CY : Fail, terminate the request
    push rax
    push rbx
    push rcx
    push rdx
    push rsi
    push rdi
    push rbp
;If the buffer is freed, empty or on an erroring disk, skip flushing
    mov eax, freeBuffer
    xchg ax, word [rdi + bufferHdr.wDrvNumFlg] ;Get flags and drive number
    ;ah = Flags, al = Drive number
    cmp al, -1  ;-1 means free buffer
    je .fbExit  ;If it was free, exit
    test ah, dirtyBuffer    ;Data modified?
    jz .fbExit  ;Skip write to disk if data not modified since last write
    cmp al, byte [errorDrv] ;Was this drive the error drive?
    je .fbExit  ;Skip write if this disk has caused an in-process error
;Now setup the error bitfields for this request.
    movzx ecx, ah   ;Get the buffer flags here
    lea rbx, qword [rdi + bufferHdr.dataarea]
    movzx esi, byte [rdi + bufferHdr.bufFATcopy]
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    mov rbp, qword [rdi + bufferHdr.driveDPBPtr]
    call writeSectorBuffer
.fbExit:
    pop rbp
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    return

writeSectorBuffer:  ;Internal Linkage
;Writes a sector from a buffer.
;Input: 
; rbx = qword ptr: Buffer area to write from.
; rcx = byte: Buffer flags
; rdx = qword: Sector to write
; rsi = byte: Number of copies to write.
; rbp = qword ptr: DPB pointer
;Exit:  CF=NC : Success (at least one write succeeded)
;               rdx = Sector actually read in.
;       CF=CY : Fail, all writes failed. Terminate the request
;               rdi = Nmuber of successful writes.
;       All other regs except rax, rsi and rdi preserved.
    mov byte [Int24bitfld], critWrite | critRetryOK | critFailOK
    test cl, dataBuffer
    jz .wsWriteSetup
    or byte [Int24bitfld], critIgnorOK
.wsWriteSetup:
    xor edi, edi    ;Successful write count
.wsWriteDisk:
    push rbx    ;# copies to write
    push rcx    ;Buffer flags
    push rdx    ;The LBA of the buffer that we are writing
    mov ecx, 1  ;One sector to write
    call primReqWriteSetup      ;Setup request (preserves setup registers)
    call absDiskDriverCall      ;Make Driver Request. Always returns CF=NC.
    pop rdx
    pop rcx
    pop rbx
    jnz .wsHardError
    inc edi         ;One successful write!
.wsCheckNext:
    mov eax, dword [rbp + dpb.dFATlength]
    add rdx, rax    ;Add offset to the next copy to write (FAT only) 
    dec esi         ;One less copy to write
    jnz .wsWriteDisk ;Jump if we gotta write another copy!
    test edi, edi   ;Return CF=NC if we had any successful writes.
    retnz
    stc             ;Return CF=CY if all writes failed.
    return
.wsHardError:
;Request failed, call hard error handler. If the user aborts, data NOT lost.
;At this point, ax = Error code, rbp -> DPB, cl = Buffer flags
    call diskIOError ;Returns al = Action code. Other regs preserved.
    cmp al, critRetry
    je writeSectorBuffer    ;Needed regs are preserved so go again.
;Else, check if we have to write any more sectors. In all cases, except FAT
; this propagates FAIL. In the case of a first FAT, this will force an attempt
; at writing the subsequent FATs. If even one of them passes, then we convert
; the fail into a success.
    jmp short .wsCheckNext  

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
;       rbp = DPB to use for transaction
;Exit:  CF=NC => 
;           Ptr to buffer header with valid data in buffer in rbx and [currBuf]
;       CF=CY => Critical Error returned Fail, rbx undefined
    mov dl, byte [rbp + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rdi, -1 ;Get in rdi the buffer ptr
    je .rbReadNewSector
    mov qword [currBuff], rdi   ;Save the found buffer ptr in the variable
    call makeBufferMostRecentlyUsed
    clc
.rbExit:
    pushfq
    mov rbx, qword [currBuff]   ;Get current buffer
    or byte [rbx + bufferHdr.bufferFlags], refBuffer    ;Mark as referenced!
    popfq
    return
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rdi
;At this point, qword [currBuff] has the same pointer as rdi.
    call flushAndFreeBuffer ;Preserves all DOS regs.
    jc .rbExit
;rdi points to bufferHdr that has been appropriately linked to the head of chain
;Thus we have a free buffer to work with.
    mov rdx, rax    ;Move the sector number into rdx
    lea rbx, qword [rdi + bufferHdr.dataarea]
;Here, regs are setup for the function we call below.
    call readSectorBuffer ;Carry the flag from the request
    jc .rbExit
;Need to write the correct data into the buffer header if OK read.
    mov rdi, qword [currBuff]   ;Get current buffer
    mov qword [rdi + bufferHdr.driveDPBPtr], rbp
    mov qword [rdi + bufferHdr.bufferLBA], rdx
    mov byte [rdi + bufferHdr.bufferFlags], cl
    movzx eax, byte [rbp + dpb.bDriveNumber]
    mov byte [rdi + bufferHdr.driveNumber], al
;Now we do the FAT specific adjustments.
    xor edx, edx    ;FAT size
    mov ebx, edx    
    inc ebx         ;FAT copies
    movzx eax, byte [rbp + dpb.bNumberOfFATs]
    test cl, fatBuffer
    cmovz eax, ebx  ;If not fat buffer, set number of FATs to 1
    mov dword [rdi + bufferHdr.bufFATcopy], eax
    mov eax, dword [rbp + dpb.dFATlength]
    cmovz eax, edx  ;If not FAT, store zero.
    mov dword [rdi + bufferHdr.bufFATsize], eax
    jmp short .rbExit   ;Jump preserving the carry flag

readSectorBuffer:   ;Internal Linkage
;Reads a sector into a buffer. 
;Entry: 
; rbx = qword ptr: Buffer area to read into.
; rcx = byte: Buffer flags.
; rdx = qword: Sector to read
; rsi = byte: Number of read attempts. (multiple on FAT sectors)
; rbp = qword ptr: DPB pointer
;Exit:  CF=NC : Success
;               rdx = Sector actually read in.
;       CF=CY : Fail, terminate the request
;       All other regs except rax preserved.
    mov byte [Int24bitfld], critRead | critFailOK | critRetryOK
.rsDoReq:
    push rbx
    push rcx
    push rdx
    mov ecx, 1              ;One sector to read
    call primReqReadSetup   ;Setup request (preserves setup registers)
    call absDiskDriverCall  ;Make Driver Request (rsi preserved)
    pop rdx
    pop rcx
    pop rbx
    retz        ;CF=NC if exiting here
;Enter here only if the request failed. al has driver error code.
    dec esi         ;Dec FAT counter (is 1 if not a FAT)
    jz .rsHardErr   ;If no more FAT's, fail. Else, add to the buffer LBA
    mov eax, dword [rbp + dpb.dFATlength]
    add rdx, rax    ;Move rdx to the sector we now want to try and read.
    jmp short .rsDoReq
.rsHardErr:
;Driver reported error.
;At this point, ax = Error code, rbp -> DPB, cl = Buffer flags
    call diskIOError    ;Returns al = Action code. Other regs preserved.
    cmp al, critRetry
    je short .rsDoReq
    stc ;Set error flag to indicate fail
    return
    
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
    push rdx
    push rdi
    push rbp
    mov rbp, qword [workingDPB] ;Get working DPB 
    call getBuffer  ;Gives the buffer ptr in rbx
    pop rbp
    pop rdi
    pop rdx
    pop rcx
    return
