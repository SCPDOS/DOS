;This file contains FAT disk buffer related functions that
; dont fit anywhere else. These functions form a part of the FAT driver
;----------------------------------------------------
;           Externally referenced functions         :
;----------------------------------------------------
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
    ret
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
    ret


freeBuffersForDrive:
;Walks the buffer chain and sets ALL buffers with the given drive number 
; to have a drive number of -1, thus freeing it
;Given drive number is in al
    push rbx
    mov rbx, qword [bufHeadPtr]
.i0:
    cmp byte [rbx + bufferHdr.driveNumber], al  ;Chosen drive?
    jne .i1 ;If no, skip freeing
    mov word [rbx + bufferHdr.driveNumber], 00FFh  ;Free buffer and clear flags
.i1:
    mov rbx, qword [rbx + bufferHdr.nextBufPtr] ;goto next buffer
    cmp rbx, -1
    jne .i0
.exit:
    pop rbx
    ret


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
;Exit:  CF = NC : All ok!
;       rbx = Pointer to buffer header with valid data in buffer.
;       All other registers as before
;       CF = CY: Something went wrong, return error code or INT 44h
;       ch = 0 -> Data Not Flushed To Disk
;       ch = 1 -> Data Not Read From Disk
;       rbx = Pointer to buffer containing sector without valid data in buffer ;            (either unflushed or unread)
    push rdx
    push rsi
    push rdi
    mov rsi, qword [workingDPB]  ;Get DPB of transacting device
    mov dl, byte [rsi + dpb.bDriveNumber]
    call findSectorInBuffer ;rax = sector to read, dl = drive number
    cmp rbx, -1
    je .rbReadNewSector
.rbExit:
    clc
.rbExitNoFlag:
    pop rdi
    pop rsi
    pop rdx
    ret
.rbReadNewSector:
    call findLRUBuffer  ;Get the LRU or first free buffer entry in rbx
    mov rdi, rbx
    xor ch, ch
    cmp byte [diskChange], -1 ;Are we in disk change?
    jne .flush  ;We are not, flush buffer
    cmp rsi, qword [rdi + bufferHdr.driveDPBPtr]    ;If yes...
    je .skipFlush   ;Avoid flushing if same DPB being used
.flush:
    call flushBuffer
    jc .rbExitNoFlag    ;Exit in error
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
    inc ch  ;If an error occurs, have the signature in ch
    call readSectorBuffer ;Carry the flag from the request
    jmp short .rbExitNoFlag

;----------------------------------------------------
;           Internally referenced functions         :
;----------------------------------------------------

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
.rsRequest0:
    mov esi, 3  ;Repeat attempt counter
.rsRequest1:
    mov al, byte [rdi + bufferHdr.driveNumber]
    mov ecx, 1  ;One sector to copy
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    mov rbx, qword [rdi + bufferHdr.dataarea]
    call absDiskRead    ;Call INT 45h
    jc .rsFail
.rsExit:
    clc
.rsExitBad:
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.rsFail:
;Enter here only if the request failed
    dec esi
    jnz .rsRequest1 ;Try the request again!
;Request failed thrice, critical error call
    stc
    jmp .rsExitBad  ;Abort

flushBuffer:    ;Internal Linkage
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
    test byte [rdi + bufferHdr.bufferFlags], dirtyBuffer    ;Data modified?
    jz .fbFreeExit  ;Skip write to disk if data not modified
.fbRequest0:
    mov esi, 3  ;Repeat attempt counter
.fbRequest1:
    mov al, byte [rdi + bufferHdr.driveNumber]
    mov ecx, 1  ;One sector to copy
    mov rdx, qword [rdi + bufferHdr.bufferLBA]
    mov rbx, qword [rdi + bufferHdr.dataarea]
    call absDiskWrite    ;Call INT 46h
    jc .fbFail
;Now check if the buffer was a FAT, to write additional copies
    test byte [rdi + bufferHdr.bufferFlags], fatBuffer ;FAT buffer?
    jz .fbFreeExit  ;If not, exit
    dec byte [rdi + bufferHdr.bufFATcopy]
    jz .fbFreeExit  ;Once this goes to 0, stop writing FAT copies
    mov eax, dword [rdi + bufferHdr.bufFATsize]
    add qword [rdi + bufferHdr.bufferLBA], rax ;Add the FAT size to the LBA
    jmp .fbRequest0 ;Make another request
.fbFreeExit:
;Free the buffer if it was flushed successfully
    mov word [rdi + bufferHdr.driveNumber], 00FFh   ;Free buffer and clear flags
    clc
.fbExitBad:
    pop rsi
    pop rdx
    pop rcx
    pop rbx
    pop rax
    ret
.fbFail:
;Enter here only if the request failed
    dec esi
    jnz .fbRequest1 ;Try the request again!
;Request failed thrice, critical error call
    stc
    jmp .fbExitBad  ;Abort
    
findLRUBuffer: ;Internal Linkage
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

findSectorInBuffer:     ;Internal linkage
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


findDirtyBufferForDrive:    ;No Use
;Searches the buffer chain for a dirty buffer for a given drive letter.
;Input: dl = Drive number
;Output: rbx = Pointer to dirty buffer for drive letter if exists or -1 if not
    mov rbx, qword [bufHeadPtr]
.fdbfdCheckBuffer:
    cmp byte [rbx + bufferHdr.driveNumber], dl
    jne .fdbfdGotoNextBuffer
    test byte [rbx + bufferHdr.bufferFlags], dirtyBuffer
    jz .fdbfdGotoNextBuffer ;Bit not set, goto next buffer
.fdbfdExit:
    ret
.fdbfdGotoNextBuffer:
    mov rbx, qword [rbx + bufferHdr.nextBufPtr]
    cmp rbx, -1     ;If rbx points to -1, exit
    je .fdbfdExit
    jmp short .fdbfdCheckBuffer