;This file contains FAT disk buffer related functions that
; dont fit anywhere else. These functions form a part of the FAT driver
;----------------------------------------------------
;           Externally referenced functions         :
;----------------------------------------------------

flushAndFreeBuffers:
; Walk through buffer chain, flushing and freeing the buffers for a given drive.
; Input: al = Drive number (or -1 for all buffers)
; Output: All drives flushed and freed for that drive letter
    push rdi
    mov rdi, qword [bufHeadPtr] ;Get head ptr
.mainLp:
    cmp al, -1
    je .skipDrvCheck
    cmp byte [rdi + bufferHdr.driveNumber], al
    jne .nextBuffer ;If not equal, skip this buffer
.skipDrvCheck:
    call flushAndFreeBuffer ;Flush and free this buffer
    jc .exit    ;Abort and carry CF if something went wrong
.nextBuffer:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr] ;Goto next header
    cmp rdi, -1
    jne .mainLp
.exit:
    pop rdi
    return


writeThroughBuffers:
;Goes through the buffer chain, flushing all buffers for the workingDrv. If 
; the drive is removable, it also frees them. If we cannot discern if 
; the drive is removable, it frees the buffers. Else, the buffers remain
; in situ. Thus, hard disks get better performance cross requests than
; removable devices but since I cannot guarantee that a removable device,
; will be the same across calls (or even within a call), we must free them,
; once we are done. Consider revising this.
; 
;Input: byte [workingDrv] = Drive to search for
;       qword [workingDPB] = Current DPB
;Output: Flushes (and optionally frees), all dirty sectors belonging to 
;        this drive.
    push rbx
    push rdx
    push rsi
    push rdi
    mov rdx, qword [workingDPB]
    mov rsi, qword [rdx + dpb.qDriverHeaderPtr] ;Get ptr to the driver hdr
    movzx eax, word [rsi + drvHdr.attrib]   ;Get the attribute word
    test eax, devDrvHdlCTL  ;Does this driver support the RemDev request?
    lea rbx, flushAndFreeBuffer ;If we cant tell, flush and free buffer
    jz .removable
    ;If so, now we test the device for removability
    ;Use primReqHdr
    movzx eax, byte [workingDrv]
    lea rbx, primReqHdr
    mov byte [rbx + remMediaReqPkt.hdrlen], remMediaReqPkt_size
    mov byte [rbx + remMediaReqPkt.unitnm], al
    mov byte [rbx + remMediaReqPkt.cmdcde], drvREMMEDCHECK
    mov word [rbx + remMediaReqPkt.status], 0
    ;Busy Bit (bit 9) will be set if removable
    call goDriver
    test word [rsi + remMediaReqPkt.status], drvBsyStatus
    lea rbx, flushAndFreeBuffer
    lea rsi, flushBuffer
    cmovnz rbx, rsi
.removable:
;rbx contains function to call
;Flush all dirty buffers back
    movzx eax, byte [workingDrv]    ;Get working drive in al
    mov rdi, qword [bufHeadPtr] ;Get head of buffer chain
.lp:
    cmp byte [rdi + bufferHdr.driveNumber], al
    je .validFound
.nextBuffer:
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    jmp short .lp
.validFound:
    call rbx    ;Call selected function
    jnc .nextBuffer ;If no error, goto next buffer
    ;Else pass through the fail!
.exit:
    pop rsi
    pop rdx
    pop rbx
    return

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
;1 External reference
;Input: rdi = Buffer header to flush and free
    call flushBuffer
    jc .exit
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
    push rdx
    mov rdx, qword [rdi + bufferHdr.nextBufPtr] ;Save next ptr in rdx
    call makeBufferMostRecentlyUsed ;Make the buffer most recently used
    mov rdi, rdx    ;Continue searching from where we left off
    pop rdx
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
;If the buffer is freed, skip flushing to avoid issues
    cmp byte [rdi + bufferHdr.driveNumber], -1  ;-1 means free buffer
    je .fbFreeExit  ;If it is free exit
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
    lea rbx, qword [rdi + bufferHdr.dataarea]
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

setBufferDirty:
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
.rbExit:
    or byte [rdi + bufferHdr.bufferFlags], refBuffer
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
    je .skipFlush   ;Avoid flushing if same DPB being used. Lose the sector
.flush:
    call flushAndFreeBuffer
    jc .rbExitNoFlag    ;Preserve the set carry flag
.skipFlush:
;rdi points to bufferHdr that has been appropriately linked to the head of chain
    ;If the sector is to be lost or has been successfully flushed, then it
    ; is no longer owned by that File so we mark the owner as none
    mov qword [rdi + bufferHdr.owningFile], -1
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
    jc .rbExitNoFlag
    jmp short .rbExit

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
;First free the buffer if we failed to read data into it 
    movzx ecx, word [rdi + bufferHdr.driveNumber]   ;Save drv num for retry
    mov word [rdi + bufferHdr.driveNumber], 00FFh ;Free buffer and clear flags
    mov byte [Int44bitfld], critRead    ;Set the initial bitfield to read req
    call diskDevErr
    cmp al, critRetry
    jne .fail   ;Else we fail (Ignore=Fail here)
    mov word [rdi + bufferHdr.driveNumber], cx ;Put drvNm + flgs if trying again
    jmp short .rsRequest0
.fail:
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
; DOS and FAT sectors need to setup [workingDPB] to make the transfer
; DIR and DATA sectors need to setup [currentSFT] to make the transfer
;FCB requests use FCBS (or SDA SFT if FCBS=0)
;Since they are just SFT entries on a separate list, this logic still holds
;The only difference is if an FCBS may need to be recycled; Then all buffers 
; belonging to that FCBS get flushed before freeing the FCBS.
;Buffer owningFile pointers get set to -1 if they are successfully freed
; or they don't belong to a file (i.e. FAT or DOS sectors)
;OwningFile is only referenced for handle/FCB sectors (DIR and Data sectors)
;-----------------------------------------------------------------------------
getBufForDataNoFile:
;Returns a buffer to use for disk data in rbx
;Requires a File Handle.
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dataBuffer
    push rsi
    push rdi
    mov rsi, qword [workingDPB] ;Get working DPB 
    call getBuffer  ;Gives the buffer ptr in rbx
    jc getBufCommon.exit
    mov qword [rbx + bufferHdr.owningFile], -1  ;Set owner to none
    jmp short getBufCommon.exit
getBufForDirNoFile:
;Returns a buffer to use for disk dir data in rbx
;Requires a File Handle.
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dirBuffer
    push rsi
    push rdi
    mov rsi, qword [workingDPB] ;Get working DPB 
    call getBuffer  ;Gives the buffer ptr in rbx
    jc getBufCommon.exit
    mov qword [rbx + bufferHdr.owningFile], -1  ;Set owner to none
    jmp short getBufCommon.exit
getBufForFat:
;Returns a buffer to use for fat data in rbx
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, fatBuffer
    jmp short getBufCommon2
getBufForDOS:
;Returns a buffer to use for DOS sector(s) in rbx
;Input: [workingDPB] = DPB to transact on
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dosBuffer
getBufCommon2:
    push rsi
    push rdi    ;Push rdi to preserve it
    mov rsi, qword [workingDPB] ;Get working DPB 
    jmp short getBufCommon.makeReq
getBufForDir:
;Returns a buffer to use for disk directory data in rbx
;Input: [currentSFT] = File to manipulate
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dirBuffer
    jmp short getBufCommon
getBufForData:
;Returns a buffer to use for disk data in rbx
;Requires a File Handle.
;Input: [currentSFT] = File to manipulate
;       rax = Sector to transfer
;Output: rbx = Buffer to use or if CF=CY, error rbx = Undefined
    push rcx
    mov cl, dataBuffer
getBufCommon:
    push rsi
    push rdi
    mov rdi, qword [currentSFT]
    mov rsi, qword [rdi + sft.qPtr] ;Get DPB
.makeReq:
    call getBuffer  ;Gives the buffer ptr in rbx
    jc .exit    ;Don't change SFT field if the request FAILED.
    ;That would be very bad as it would potentially cause faulty data to be 
    ; flushed to the file!
    ;Only set the SFT field if Data or DIR sectors, as getBuffer
    ; will always set the owningFile field to -1 if the data was successfully
    ; flushed or deemed ok to lose (thus completing setup for dos/fat buffers).
    test cl, dosBuffer | fatBuffer
    jnz .exit
    mov qword [rbx + bufferHdr.owningFile], rdi ;Set owner for the data
.exit:
    pop rdi
    pop rsi
    pop rcx
    return

flushFile:
;We search the chain for buffers with the currentSFT = owning file and ALL
; FAT/DOS buffers to flush
; We flush and free, and set to head of chain before continuing to search
;Input: rdi = is the file (sft) we wish to flush
;Output: CF=NC => All ok
;        CF=CY => A sector failed, exit. 
    push rdi
    push rsi
    ;First check if the file has been written to?
    test word [rdi + sft.wDeviceInfo], blokFileNoFlush
    jnz .exitNoFlush ;Exit without flushing if set
    mov rsi, rdi    ;Move the currentSFT to rsi
    mov rdi, qword [bufHeadPtr]
.ffLoop:
    cmp rdi, -1
    je .exit
    test byte [rdi + bufferHdr.bufferFlags], fatBuffer | dosBuffer | dirBuffer
    jnz .found  ;Flush if either bit is set
    cmp qword [rdi + bufferHdr.owningFile], -1  ;If owning file is -1, flush too
    je .found
    cmp qword [rdi + bufferHdr.owningFile], rsi
    je .found
    mov rdi, qword [rdi + bufferHdr.nextBufPtr]
    jmp short .ffLoop
.exit:
    ;Here we undo the disk file to be flushed bit in the SFT
    or word [rsi + sft.wDeviceInfo], blokFileNoFlush  ;Set that bit again!
.exitNoFlush:
    pop rsi
    pop rdi
    return
.found:
;Here we take the old next buffer, then flush and free the current buffer
; then return the old next buffer into rdi and go back to ffLoop
    call flushAndFreeBuffer ;Flush and free buffer
    jc .exitNoFlush    ;Exit preserving CF
    ;If the sector has been successfully flushed, then it
    ; is no longer owned by that File so we mark the owner as none
    mov qword [rdi + bufferHdr.owningFile], -1
    call makeBufferMostRecentlyUsedGetNext  ;Return in rdi the next buffer
    jmp short .ffLoop