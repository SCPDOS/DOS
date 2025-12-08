;-----------------------------------------------------------------------------
;All utility functions go here, in order.
;-----------------------------------------------------------------------------
; critEnter -> Enters a DOS 1 critical section
; critExit -> Exits a DOS 1 critical section
; lockPreamble -> Sets up the registers for set/clear flock
; checkLockOverlap -> Checks if a lock with the given bounds exists
; getFreeLock -> Removes a free lock from the free lock list
; closeByID -> Closses all files according to IDs
; closeSFT -> Wrapper for Close SFT
; closeHandlesForSFT -> Closes handles referencing an SFT
; freeMFT -> Frees an MFT from the MFT arena
; removeSFTfromMFT -> Removes an SFT from an MFT's SFT chain
; freeLocks -> Frees all file locks associated to a SFT
; addSFTtoMFT -> Adds an SFT to its MFT chain.
; getMFT -> Gets an MFT for a given filename. Creates if it has to.
; defragMFTArena -> Defragments the MFT arena
; findFreeMFT -> Finds a free MFT for a given size
; findMFT -> Find an MFT for a given filename
; getNameMeta -> Computes the length and checksum of a filename
; closeJFTentries -> Closes all JFT entries pointing to a 
;                       particular SFT for a process.
; errPrintAndHalt -> Prints a formatted error message and halts the machine
;-----------------------------------------------------------------------------


critEnter:
    push rax
    mov eax, 8001h  ;Enter DOS Level 1 critical section
    int 2ah
    pop rax
    return
critExit:
    push rax
    mov eax, 8101h  ;Exit DOS Level 1 critical section
    int 2ah
    pop rax
    return

lockPreamble:
;Sets up the regs for the lock/unlock operation
;Input: rdi -> SFT for file
;       ecx:edx = Start offset
;       esi:eax = Length of region
;Output:    CF=NC: Ok to proceed with locking
;               rax = Start byte of lock
;               rdx = End byte of lock
;               rbp -> MFT we are adding the lock to
;               rbx = rdi = Trashed
;           CF=CY: Error!
;               ZF=ZE: Lock equals a previous lock
;                   rbx -> Lock we matched to
;                   rdi -> Lock pointing to rbx
;               ZF=NZ: Lock partially overlaps or of 0 length
;           No register guarantee in this error condition.
;ecx and esi trashed. All other registers ok.
    shl rsi, 32     ;Move the high bits of length high
    or rax, rsi     ;Stick the length bits together w/o overlap
    jz .badExit     ;If the length is zero, fail the operation
    shl rcx, 32     ;Move the high bits of the start byte high
    or rdx, rcx     ;Stick the start byte bits together w/o overlap
    add rax, rdx    ;Add the start byte to the length to get first byte free
    dec rax         ;And now point rax to the last byte in the lock
    xchg rdx, rax   ;And swap em
;rax -> Start byte of lock
;rdx -> End byte of lock
;rsi is now free
;rcx is now free
    mov rbp, qword [rdi + sft.pMFT] ;Set rbp to the mft to inspect
    xor ecx, ecx
    inc ecx     ;Check all locks for overlap
    jmp short checkLockOverlap  ;Return through here to save bytes
.badExit:
;We return CF=CY and ZF=NZ if the length was zero! Hence setting the 
; error code to have a guaranteed non-zero value to test to clear ZF.
    mov eax, errLokVio
    test eax, eax   ;Clear the ZF flag
    stc             ;And set CF
    return

checkLockOverlap:
;Input: rax = Start byte to check
;       rdx = End byte to check
;       rbp -> MFT to search on
;       ecx = 0: Do not check locks belonging to currentSFT, dMID, qPID
;           = 1: Check all locks
;Output: CF=NC: No overlap!
;           rbx = Trashed (Null)
;           rdi = Trashed 
;        CF=CY: Overlap!
;           ZF=ZE: Lock region matched exactly
;           ZF=NZ: Lock region not matched exactly
;           rbx -> Lock we matched to
;           rdi -> Lock pointing to rbx
;All other regs preserved
    push rsi
    mov rbx, qword [rbp + mft.pLock]
    lea rdi, qword [rbp + mft.pLock - fileLock.pNext]
.lockLp:
    test rbx, rbx
    jz .exit
    test ecx, ecx   ;Check if we must do this lock?
    jnz .doLock
;Now we check that this lock isnt the same as the current SFT, qPID and dMID.
;If any are not equal, we do the lock. If they are all the same, skip.
;Check currentSFT
    mov rsi, qword [r8 + currentSFT]
    cmp rsi, qword [rbx + fileLock.pSFT]
    jne .doLock
;Check qPID
    mov rsi, qword [rbx + fileLock.qPID]    
    cmp rsi, qword [r8 + qPID]
    jne .doLock
;Check dMID
    mov rsi, qword [rbx + fileLock.pSFT]
    mov esi, dword [rsi + sft.dMID]
    cmp esi, dword [r8 + dMID]
    jne .doLock
;If all three were equal, skip this lock!
.nextLock:
    mov rdi, rbx    ;Save ptr to the trailing lock in rdi
    mov rbx, qword [rdi + fileLock.pNext]  ;Go to next lock in chain
    jmp short .lockLp
.doLock:
;Input: rbx -> filelock to check on
;       rax -> Start byte to check
;       rdx -> End byte to check
    cmp rax, qword [rbx + fileLock.qEnd]
    ja .nextLock    ;If start is past the end, immediately check next!
    cmp rdx, qword [rbx + fileLock.qStart]
    jb .nextLock    ;If end is below the start, immediately check next!
;Thus there must be some overlap! Check if exact
    cmp rax, qword [rbx + fileLock.qStart]
    jne .ovlpExit
    cmp rdx, qword [rbx + fileLock.qEnd]    ;Keeps ZF set if equal  
.ovlpExit:
    stc
.exit:
    pop rsi
    return

getFreeLock:
;Gets a free lock and removes it from the free lock list.
;Does not allocate it yet though and it remains in rsi.
;Output: CF=NC: rsi -> Lock
;        CF=CY: No more locks
    mov rsi, qword [pFreeLock]
    test rsi, rsi
    jz .noMoreLocks
    push qword [rsi + fileLock.pNext]   ;Move next entry of free list...
    pop qword [pFreeLock]   ;... onto the head of the list.
    clc
    return
.noMoreLocks:
    mov eax, errShrFul
    stc
    return


closeByID:
;Closes all files by IDs.
;Input: rcx = -1 if we are not to use the qPID for searches, else 0.
;       SDA has qPID and dMID set
;Searches through the MFT heap for allocated MFTs and within them,
; frees each SFT which meets the ID requirements
;Trashes everything except r8.
    push r8
    mov r8, qword [pDosseg]
;Start by getting dMID and, optionally, qPID from SDA
    mov edx, dword [r8 + dMID]
    or rcx, qword [r8 + qPID]   ;If rcx=0, sets rcx to qPID. Else, stays at -1
    mov rdi, qword [pMftArena]
    call .mftAdvNext    ;Set rsi and rdi appropriately
    cmp byte [rsi + mft.bSig], mftFree
    jne .mftGo  ;If not free, it can't be end as first is either alloc or free
;Here if the first block was free. rdi -> First non-free block.
;Let it fall through. If rdi points to the end block, we have no allocd MFTs
; so we exit ok! Else, start from rdi, setup rsi and move rdi to next 
; mft after it!
.mftNext:
    cmp byte [rdi + mft.bSig], mftEnd   ;Is this MFT an end MFT?
    je .end
;We advance rdi before processing rsi. The advance must be done
; before any possible close as the close might invalidate the pointer in rsi.
    call .mftAdvNext    ;Return rdi -> Next Non-free MFT
.mftGo:
;Enter here with:
;rdi -> Next MFT in MFT block
;rsi -> MFT we will be closing SFTs on
;Now we go down rsi's SFT chain freeing the SFTs as specified.
    mov rbx, qword [rsi + mft.pSFT] ;Point rbx to first SFT in mft's sft chain
.sftLp:
    test rbx, rbx   ;Once we are at the end of the sft chain, goto next MFT
    jz .mftNext
;Here we have an SFT for investigation. Set it to current and push the
; next SFT on the stack
    mov qword [r8 + currentSFT], rbx    ;Set the current SFT as rbx
    push qword [rbx + sft.pNextSFT] ;Save the ptr to next SFT in chain
;Now check do we need to close this SFT
    cmp dword [rbx + sft.dMID], edx
    jne .sftSkip
    cmp rcx, -1 ;If -1, then just equal dMID suffices. Close this SFT
    je .sftGo
    cmp qword [rbx + sft.qPID], rcx
    jne .sftSkip
.sftGo:
;Here we have an sft to close. The one in rbx. 
;Set its count to 1, to make closeSFT clean up properly as we will
; close all jft entries across all processes. 
    mov word [rbx + sft.wNumHandles], 1
    push rcx    ;Save the qPID (or -1 signature) for checking
    push rdx    ;Save the dMID for checking
    push rsi    ;Save the current MFT we are processing
    push rdi    ;Save the next MFT
;These two require currentSFT set
    call closeHandlesForSFT ;Close all JFT entries pointing to this SFT
    call closeSFT   ;This might delete the current MFT... 
    pop rdi     ;Get the next MFT
    pop rsi     ;Get the current MFT (might be invalid now)
    pop rdx     ;Get the dMID
    pop rcx     ;Get the qPID (or -1 signature)
.sftSkip:
    pop rbx     ;Get ptr to the next SFT in the chain (might be null)
    jmp short .sftLp
.end:
;Else, the MFT was the end MFT so we are done! Exit!
    pop r8
    return
.mftAdvNext:
;Advances rdi to the next non-free MFT (stops at mftEnd).
;Input: rdi -> MFT to start advancing from.
;Output: rdi -> Next non-free MFT
;        rsi -> MFT we started at
    mov rsi, rdi
.manLp:
    mov eax, dword [rdi + mft.dLen]
    add rdi, rax  
    cmp byte [rdi + mft.bSig], mftFree  ;Do we keep advancing?
    je .manLp  ;If rdi points to a free MFT, yes!
    return


closeSFT:
;Closes current SFT
;Input: qword [currentSFT] = SFT to close
    mov eax, 1201h ;Close the currentSFT
    int 2Fh
    return

closeHandlesForSFT:
;Closes all handles referencing the current SFT
;Input: r8 -> DOSSEG
;       qword [currentSFT] = SFT to close handles for
;rdx, rbp and r8 saved. All other regs trashed.
    mov rsi, qword [r8 + currentSFT]
    xor ebx, ebx
.sftIndxLp:
;This loop looks like it will infinitely loop but since we are working
; on the current SFT, which is an entry in the SFT tables, it is guaranteed 
; that the pointer will return us a valid index.
    push rbx
    mov eax, 1216h  ;Get SFT Ptr from SFT Index into rdi
    int 2Fh
    pop rbx
    inc ebx
    cmp rsi, rdi    ;Was this index correct?
    jne .sftIndxLp
    dec ebx         ;Decrement it back, we are looking for this number
;Now we walk the Arena Chain to find every task ID and close the
; JFT entry for this file if the SFTIndex exists in that task's JFT.
;We use rax to hold the previous PID to avoid repeatedly hitting the same PSP
    xor eax, eax    ;Zero to give a PID that will not be at the start
    mov rdi, qword [r8 + mcbChainPtr]   ;Get the MCB chain header in rdi
    jmp short .checkMCB ;Make sure this MCB is a valid MCB
.mcbLp:
    cmp rax, qword [rdi + mcb.owner]    ;If new PID same as before, skip
    je .getNextMCB
    mov rax, qword [rdi + mcb.owner]    ;Else, get the new PID
    call closeJFTEntries    ;Close all JFT entries for this process
.getNextMCB:
    cmp byte [rdi + mcb.marker], mcbMarkEnd ;If we processed end block, exit
    rete
;Else, goto next block
    mov ecx, dword [rdi + mcb.blockSize]    ;Get size of block in paras
    shl rcx, 4      ;Turn into bytes
    add rcx, mcb_size
    add rdi, rcx    ;Go to next MCB
.checkMCB:
    mov cl, byte [rdi + mcb.marker] ;Now check the new MCB is valid
    cmp cl, mcbMarkEnd
    je .mcbLp
    cmp cl, mcbMarkCtn
    je .mcbLp
    call errPrintAndHalt    ;Wasn't a end or cont block, error!
    db "MCB CORRUPTION DETECTED",CR,LF,NUL

freeMFT:
;Frees the MFT if it is safe to do so. Crashes otherwise. Then combines
; all fragmented free space in the MFT arena
;Input: rbx -> MFT to free
;Output: MFT freed. Any adjacent free space is absorbed by this MFT too.
;Trashes rax, rbx and rsi
    mov rax, qword [rbx + mft.pSFT]
    or qword [rbx + mft.pLock], rax ;If pLock or pSFT is non-zero, hard error
    jnz .crash
    mov byte [rbx + mft.bSig], mftFree
;Now start combining all fragmented free space in the MFT arena space
    mov rbx, qword [pMftArena]
.lp:
    cmp byte [rbx + mft.bSig], mftFree
    retl            ;If MFT end, finish combining
    mov esi, dword [rbx + mft.dLen] ;Else, get length of base MFT
    jne .gotoNext   ;If MFT our not free, skip this MFT
    cmp byte [rbx + rsi + mft.bSig], mftFree    ;Is the next MFT free?
    jne .gotoNext   ;If not, leave this MFT block alone and goto next MFT
    mov esi, dword [rbx + rsi + mft.dLen]   ;Else, get length of next MFT
    add dword [rbx + mft.dLen], esi         ;And add to length of base MFT
    jmp short .lp
.gotoNext:
;esi always contains the current, updated, length of the base MFT when we
; jump here.
    add rbx, rsi    ;Add size of the current MFT to the MFT ptr to get next
    jmp short .lp
.crash:
    call errPrintAndHalt
    db "SFT LCK fields not 0",CR,LF,NUL


removeSFTfromMFT:
;Removes the SFT from the MFT's SFT chain.
;Input: rdi -> SFT we are closing.
;Output: ZF=ZE: No more files on MFT
;        ZF=NZ: More files on MFT.
;           rbx -> MFT for this file
;Trashes rax and rsi.
    mov rbx, qword [rdi + sft.pMFT] ;Get the MFT pointer
;Point rsi such that it is one sft.pNextSFT away from the pointer to
; the next SFT
    lea rsi, qword [rbx + mft.pSFT - sft.pNextSFT]
.lp:
    test rsi, rsi   ;If we dont find rdi, something has gone terribly wrong
    jz .crash
    cmp qword [rsi + sft.pNextSFT], rdi    ;Is the next SFT us?
    je .found
    mov rsi, qword [rsi + sft.pNextSFT] ;Go to next SFT
    jmp short .lp
.found:
;rsi -> Points to the previous SFT in the chain 
    mov rax, qword [rdi + sft.pNextSFT] ;Get our next SFT value
    mov qword [rsi + sft.pNextSFT], rax ;And set the prev SFT to link over us
    mov qword [rdi + sft.pMFT], 0   ;Set pMFT to zero to mean we are done!
    cmp qword [rbx + mft.pSFT], 0   ;Is the MFT SFT chain ptr 0?
    return
.crash:
    call errPrintAndHalt
    db "SFT NOT IN SFT LIST",CR,LF,NUL


freeLocks:
;Frees all file locks associated to this rdi. 
;Input: rdi -> SFT for this file
;       edx = Flag for if we should free locks for process
;             -1 -> Free for process (on 5D04h calls)
;              0 -> Dont bother (otherwise)
;Output: All file locks owned by this process are delinked from the 
;        file chain and added to the free lock chain.
;Trashes rax, rbx, rsi.
    mov rbx, qword [rdi + sft.pMFT]     ;Get MFT ptr in rbx
    mov rsi, qword [rbx + mft.pLock]    ;Get the first lock of MFT in rsi 
    add rbx, mft.pLock  ;Point rbx to the link field of the previous lock
.lp:
    test rsi, rsi   ;End of list? 
    retz            ;Exit if so
;A lock is ours if the SFT pointer of the lock matches our SFT pointer in rdi
    cmp qword [rsi + fileLock.pSFT], rdi    ;Is the SFT ours?
    jne .nextLock   ;If not, skip
    test edx, edx   ;If it is but we dont care for process ID, skip this
    jz .skipProcess
;Now check this lock belongs to this process too
    push r8
    mov r8, qword [pDosseg]
    mov rax, qword [r8 + qPID]  ;Get the process id
    cmp qword [rsi + fileLock.qPID], rax    ;And compare them
    pop r8
    jne .nextLock
.skipProcess:
;Here we delink the flock. We move it from the MFT list to the Free list
; and cross link.
;Registers on entry
;rdi -> Our SFT
;rsi -> Lock
;rbx -> The previous lock in the file chain (only first qword valid)
;edx = Flag, do not use.
    mov rax, qword [rsi + fileLock.pNext]   ;Unlink rsi by linking the prev...
    mov qword [rbx + fileLock.pNext], rax   ;... flock to the flock after rsi
    mov rax, qword [pFreeLock]              ;Get head of the free lock list
    mov qword [rsi + fileLock.pNext], rax   ;Link rsi to the head of free list
    mov qword [pFreeLock], rsi              ;Make rsi the head of the free list
    mov rsi, rbx    ;Start from previous lock in the list again.
.nextLock:
    mov rbx, rsi    ;Point rsi to this filelock
    mov rsi, qword [rsi + fileLock.pNext]   ;Get next file lock
    jmp short .lp

addSFTtoMFT:
;Adds an SFT to the MFT chain and finishes filling in it's share fields. 
;Checks for sharing conflicts before adding.
;Input: rsi -> SFT to add
;       rbx -> MFT to add SFT to
;       r8 -> DOSSEG
;Output: CF=NC: SFT Added to list
;        CF=CY: Sharing violation. eax has error code
    cmp qword [rsi + sft.pMFT], 0   ;This SFT better not already be on a chain
    jne .crash
;Add the networking ids now as we need them for permissions checking
    mov rax, qword [r8 + qPID]
    mov qword [rsi + sft.qPID], rax
    mov eax, dword [r8 + dMID]
    mov dword [rsi + sft.dMID], eax
;Now we check if there are any sharing conflicts
    call checkPermissions
    retc
    mov qword [rsi + sft.pMFT], rbx
;Now add this SFT to the front of the list.
    mov rdi, qword [rbx + mft.pSFT] ;Get the head of the SFT list
    mov qword [rsi + sft.pNextSFT], rdi ;Link new SFT to previous head
    mov qword [rbx + mft.pSFT], rsi ;Put new SFT at head of MFT list
    return
.crash:
    call errPrintAndHalt
    db "SFT ALREADY IN USE",CR,LF,NUL

checkPermissions:
;Checks for sharing conflicts of the new SFT against all
; other SFTs in the chain. 
;If both new SFT and SFT on chain are compatibility 
; then check machine id. If they are equal, goto next SFT on chain.
;Else do table logic as described below.
;
;Input: rsi -> New SFT, with machine id field set
;       rbx -> MFT for this file.
;Output: CF=NC: No sharing conflicts.
;        CF=CY: Sharing conflict, eax has error code
; Preserves rbx and rsi.
    push rbx
    mov rdi, rsi        ;Save new SFT ptr in rdi
    call getOpenMode    ;Get adjusted sharing mode in eax for new file in rdi
    mov edx, eax        ;Use edx as the marker for if compat or not
    and edx, 0F0h       ;Are the share bits 0?
    jz .compat
    mov edx, 0F0h       ;Set to indicate no compat mode
.compat:
    call getTableIndex  ;Get table index in eax for new SFT. Use as row index
    shl eax, 1          ;Multiply by two, each row is a word
    lea rsi, .tbl
    movzx ebp, word [rsi + rax] ;Get the table entry (bit row) into ebp
    mov rsi, qword [rbx + mft.pSFT] ;Get the first SFT in chain
.lp:
    test rsi, rsi       ;Are we at the end of the chain?
    jz .exit            ;If so, all ok!
    call getOpenMode    ;Else, get the adjusted open mode in eax for this sft
    mov ecx, eax
    and ecx, 0Fh        ;Get the share bits
    or ecx, edx         ;Check if both new and old files are in compat mode
    jnz .notBothCompat
;Both in compat mode means we do machine id check instead.
;If they match, we skip.
    mov ebx, dword [rsi + sft.dMID]
    cmp dword [rdi + sft.dMID], ebx
    je .gotoNext
.notBothCompat:
    call getTableIndex  ;Get table index but use as a column (bit) index
    lea ecx, dword [eax + 2]    ;+2 to drop the x bit and push our bit into CF
    mov eax, ebp        ;Move row we are interested in into eax
    shr eax, cl         ;Shift the bit we are interested into CF to check
    jc .exitBad
.gotoNext:
    mov rsi, qword [rsi + sft.pNextSFT] ;Goto next SFT
    jmp short .lp
.exitBad:
;We come here with CF=CY
    mov eax, errShrVio
.exit:
    mov rsi, rdi        ;Point rsi back to the new SFT
    pop rbx             ;Get back MFT pointer
    return

.tbl:          
;Each word is a packed entry of 15 meaningful bits with the 
; lower most bit always set and meaningless.
;That is, each word is 5 lots of 3(share mode) bits.
;The bits of each word are interpreted as follows:
;     0NNNRRRWWWAAACCCxb 
; where: 
; NNN = Deny None permissions
; RRR = Deny Read permissions 
; WWW = Deny Write permissions
; AAA = Deny All permissions
; CCC = Compat permissions
; x   = Set and ignored.
;In each triple:
;   0LMNb 
; the bits are interpreted as:
; L = What to do on Read/Write access. 
; M = What to do on Write access. 
; N = What to do on Read access.
;If a bit is set, it means this combination is not allowed.
;
;We get the word we are interested in based on the open mode of
; the new SFT. This gives us a row. We then use the open mode of
; every SFT on the MFT's SFT chain, to get a bit offset into this word.
;If for every element of the SFT chain the bit in the word is 0,
; we are ok. If ever this bit is 1, we have a sharing violation.
;
;The values of this table obtained from the matrix of the the DOS 3.3 
; programmers guide (p. 6-135), where N=1 and Y=0, with the following changes
; to make implementation easier (as the numbers are ordered as so):
;1) We swap the order of the openRWAcc and openWrAcc columns/rows
;    putting RW always at the left/bottom most part. 
;2) Each row of the table below is for "subsequent IO" where each
;    bit of the entry is the column.
;3) We duplicate Deny all as for Compat

;Table annotated as follows: 
;N=Deny none
;R=Deny read
;W=Deny write
;A=Deny all
;C=Compat
;B=Open Read/Write (both)
;O=Open Write (output)
;I=Open Read (input)
;
;        Initial Open modes
;       N   R   W   A   C  x    ;Subsequent opens
;      BOI BOI BOI BOI BOI x    
    dw 111_111_111_111_111_1b   ;I        openCompat|openRdAcc
    dw 111_111_111_111_111_1b   ;O C      openCompat|openWrAcc
    dw 111_111_111_111_111_1b   ;B        openCompat|openRWAcc

    dw 111_111_111_111_111_1b   ;I      openDenRWShr|openRdAcc
    dw 111_111_111_111_111_1b   ;O A    openDenRWShr|openWrAcc
    dw 111_111_111_111_111_1b   ;B      openDenRWShr|openRWAcc

    dw 110_111_110_111_111_1b   ;I      openDenWrShr|openRdAcc
    dw 110_110_111_111_111_1b   ;O W    openDenWrShr|openWrAcc
    dw 110_111_111_111_111_1b   ;B      openDenWrShr|openRWAcc

    dw 101_111_101_111_111_1b   ;I      openDenRdShr|openRdAcc
    dw 101_101_111_111_111_1b   ;O R    openDenRdShr|openWrAcc
    dw 101_111_111_111_111_1b   ;B      openDenRdShr|openRWAcc

    dw 000_111_000_111_111_1b   ;I      openDenNoShr|openRdAcc
    dw 000_000_111_111_111_1b   ;O N    openDenNoShr|openWrAcc
    dw 000_111_111_111_111_1b   ;B      openDenNoShr|openWRAcc

getTableIndex:
;Turns the adjusted open mode into a table index value
;Input: eax = Adjusted open mode
;Output: eax = Index into the table (Number 0-14)
;        ecx = Share group (0-4) [Never used, do not rely on!]
;All other regs preserved
    mov ecx, eax
    and eax, 0Fh    ;Isolate the open mode bits only
    and ecx, 0F0h   ;Isolate the share bits only
    cmp ecx, openNetFCBShr  ;Is this a net FCB share?
    jne .notNetFCB
    xor ecx, ecx    ;Compatibility share in this case
.notNetFCB:
;We want ecx <- (ecx >> 4)*3 to get the share group
    shr ecx, 4      ;Get the share group
    push rdx
    mov edx, ecx    ;Save it in edx
    shl edx, 1      ;Multiply by 2
    add edx, ecx    ;Add to get it multiplied by three
    add eax, edx    ;Add share group*3 to the offset into the group
    pop rdx
    return

getOpenMode:
;Gets the adjusted open mode.
;--------------------------------------------------------------------
;Input: rsi -> SFT to get adjusted open mode for
;Output: eax = Adjusted open mode.
;All other registers preserved.
;--------------------------------------------------------------------
;If not an FCB of any kind, then everything is ok. 
;If an SFTFCB then we put it into openCompat | openRW.
;If a NETFCB then we put it into openCompat.
;If RO file opened in openCompat, make into openDenWrShr | openRdAcc.
    push rbx
    movzx eax, word [rsi + sft.wOpenMode]
    test ax, openSFTFCB ;Is this a local FCB?
    jz .notFCB
    mov eax, openCompat | openRWAcc  ;FCBs have r/w access in compat mode
.notFCB:
    mov ebx, eax
    and ebx, 0F0h           ;Isolate the share bits
    cmp ebx, openNetFCBShr  ;Is this a net FCB?
    jne .notNetFCB
    and eax, 0Fh            ;Clear the share bits, turn into compat share
.notNetFCB:
;Now al has the real open mode for this file. Do RO check now
    mov ebx, eax
    and ebx, 0F0h   ;Get actual share bits to check if in compat share
    pop rbx
    retnz           ;If not zero, not in compat mode
    test byte [rsi + sft.bFileAttrib], attrFileRO
    retz            ;If zero, RO bit not set
    mov eax, openDenWrShr | openRdAcc 
    return


getMFT:
;Search for an MFT for a file. If one is not found, it creates it.
;Input: rsi -> Filename to search for
;Output: CF=NC: rbx -> MFT for this file.
;        CF=CY: eax = Error code.
;Start by working out the string length and checksum values
    call findMFT        ;Sets dl for checksum and ecx for string length
    retnc
;We get here if this file has no MFT.
;Create an MFT entry. dl and ecx are always preserved in functions below
    add ecx, mft_size   ;ecx now has the full MFT entry size
    call findFreeMFT
    jnc .buildMFT
    call defragMFTArena ;Returns rbx -> Free MFT, eax = Free MFT size
    cmp eax, ecx        ;Do we fit in this free space?
    jnc .buildMFT
;Here if CF=CY. Return Sharing buffer full error!
    mov eax, errShrFul
    return
.buildMFT:
;Here we have the following values:
;rsi -> String to copy in
;rbx -> Space to allocate the MFT in. rbx points to a free MFT.
;ecx = New MFT full size
;dl = String checksum 

;Work out if we want to split space into two MFTs or keep it as one.
;Check if the MFT we are pointing at has enough space for a minimum mft and
; our MFT. If there is more than enough space, we split into two MFTs. 
;Else, we just allocate the whole space and waste the few extra bytes.
    mov eax, dword [rbx + mft.dLen] ;Get the current size of the free MFT
    sub eax, ecx        ;Remove our own allocation from the free MFT size
    cmp eax, MIN_MFT_SIZE
    jbe .alloc   ;If this is beq, we just use this mft entry as is.
;eax has the size of the new free block
    lea rdi, qword [rbx + rcx]  ;Point rdi past the end of our new mft
    mov byte [rdi + mft.bSig], mftFree  ;Make the new free block
    mov dword [rdi + mft.dLen], eax     ;Set the new free block's size
    mov dword [rbx + mft.dLen], ecx     ;Set the newly allocated block's size
.alloc:
;Finally, allocate the MFT. The size, if modified, is done already.
    mov byte [rbx + mft.bSig], mftAlloc
    mov qword [rbx + mft.pLock], 0
    mov qword [rbx + mft.pSFT], 0
    mov byte [rbx + mft.bCheckSum], dl  ;Set the checksum immediately.
    sub ecx, mft_size   ;Now get just the string length
    lea rdi, qword [rbx + mft.sName] 
    rep movsb   ;Move the string over
    clc
    return

defragMFTArena:
;Defragments the MFT arena. Moves all allocated MFTs to the start.
;Creates a single large free MFT from all the free space.
;Input: Nothing
;Output: MFT arena defragged.
;        rbx -> New free MFT
;        eax = New Free MFT size
; All other regs preserved.
    push rcx
    push rdx
    push rsi
    push rdi
;rsi = Read pointer
;rdi = Write pointer
;edx = Free space accumulator
    mov rsi, qword [pMftArena]
    mov rdi, rsi
    xor edx, edx
.lp:
;If we encounter an allocated mft, we move it over, moving the 
; the read and write pointers. If we encounter a free mft, we add its
; size and move the read pointer.
    cmp byte [rsi + mft.bSig], mftFree
    jl .exit    ;At this point, all compression is done. Goto end
    je .free
;Here we copy over. Before we do, we walk down the SFT chain
; updating the MFT pointer in each SFT.
;rsi -> Allocated MFT
;rdi -> Where this MFT will be moved to
    mov rbx, qword [rsi + mft.pSFT]     ;Point rbx to the first SFT in chain.
.sftLp:
    test rbx, rbx   ;End of SFT chain?
    jz .sftEnd
    mov qword [rbx + sft.pMFT], rdi     ;Update the SFT's MFT pointer
    mov rbx, qword [rbx + sft.pNextSFT]
    jmp short .sftLp
.sftEnd:
;All SFTs updated, now we copy the MFT up.
    mov ecx, dword [rsi + mft.dLen] ;Get the length of this MFT
    rep movsb   ;Moves both pointers to the end of the MFT
;Now rdi points to the next write space and rsi points to the next MFT
    jmp short .lp
.free:
;Dont worry about overflow. Max dLen is 1Mb, way less than 32 bit max.
    mov eax, dword [rsi + mft.dLen] ;Get the length
    add edx, eax    ;Add to the free space accumulator
    add rsi, rax    ;Move the read pointer past this free space
    jmp short .lp
.exit:
;Now we must make a single free MFT at the write pointer with the 
; correct size and assert it is correct
;rdi -> Where to make this MFT
;edx = Accumulated free space
    mov byte [rdi + mft.bSig], mftFree
    mov dword [rdi + mft.dLen], edx
    mov eax, edx    ;Get the free MFT size into eax
    mov rbx, rdi    ;Get the free MFT pointer into rbx
    add rdi, rdx    ;Now check the MFT chain is not corrupted
    cmp byte [rdi + mft.bSig], mftEnd
    jne .crash
    pop rdi
    pop rsi
    pop rdx
    pop rcx
    return
.crash:
    call errPrintAndHalt    ;No return. Use call to push string ptr to stack
    db "MFT DEFRAG FAILURE",CR,LF,NUL

findFreeMFT:
;Searches for a free MFT of a particular size or greater.
;Input: ecx = Minimum MFT size 
;Output: CF=NC: rbx -> MFT of appropriate size
;        CF=CY: No MFT found of this size. rbx -> End MFT
;Trashes rax. All other regs preserved.
    mov rbx, qword [pMftArena]
.lp:
    cmp byte [rbx + mft.bSig], mftFree
    jl findMFT.noMFT    ;If below, must be mftEnd. Exit error!
    jne .next           ;If not equal, must be mftAlloc. Goto next MFT
    cmp dword [rbx + mft.dLen], ecx ;Is this MFT ok sizewise?
    retnc   ;Not carry means dLen is geq eax, as required
.next:
    mov eax, dword [rbx + mft.dLen]
    add rbx, rax    ;Point to the next MFT
    jmp short .lp

findMFT:
;Searches for an MFT for the filename in rsi 
;Input: rsi -> Filename to search for
;       
;Output: ecx = String length
;        dl = Filename checksum
;        CF=NC: rbx -> MFT for file
;        CF=CY: No MFT found for this file. rbx -> End MFT
;Trashes rax. All other regs preserved.
    call getNameMeta    ;Sets ecx and dl
    mov rbx, qword [pMftArena]
.lp:
    cmp byte [rbx + mft.bSig], mftFree  
    jl .noMFT   ;If below, bSig must be -1. End of arena!
    je .next    ;If this MFT is free, goto next entry
    cmp byte [rbx + mft.bCheckSum], dl  ;Compare checksums
    jne .next   ;If not equal, skip entry
    push rdi
    lea rdi, qword [rbx + mft.sName] ;Point to mft filename
    mov eax, 121Eh  ;Compare strings in rsi and rdi
    int 2Fh         ;Doesnt modify string pointers
    pop rdi
    retz    ;If ZF=ZE (also CF=NC), we have found the MFT! Return to caller.
.next:
    mov eax, dword [rbx + mft.dLen]
    add rbx, rax    ;Point to the next MFT
    jmp short .lp
.noMFT:
    stc
    return

getNameMeta:
;Gets the string length and checksum value
;Input: rsi -> String to search on
;Output: ecx = String length
;        edx = Checksum value
;Trashes eax.
    push rsi
    xor ecx, ecx    ;Use for string length (zero inclusive)
    xor edx, edx    ;Use for checksum value (dl)
.metalp:
    lodsb
    add dl, al
    adc dl, 0       ;Add 1 if this rolls over
    inc ecx
    test al, al
    jnz .metalp
    pop rsi
    return

closeJFTEntries:
;Closes all JFT entries pointing to a particular SFT for a process.
;Input: bl = SFTIndex
;       rax = PID for the task
;Output: Closes all JFT entries.
;Preserves all registers
    push rax
    call .getPSPfromPID ;If PID is special, just exit. Else, get pPSP in rax
    je .cjeExit2
    push rcx
    push rdi
    lea rdi, qword [rax + psp.jobFileTbl]   ;Point rdi to JFT or pJFT
    movzx ecx, word [rax + psp.jftSize]     ;Get the size of the JFT
    cmp ecx, dfltJFTsize
    cmova rdi, qword [rdi]  ;If above normal, pull the JFTPtr
    mov eax, ebx    ;Get the SFTIndx to scan for in eax (al)
.cjeLp:
    repne scasb     ;DOS sets direction so we dont worry
    jne .cjeExit
    mov byte [rdi - 1], -1  ;Free the entry
    test ecx, ecx  ;If we are out of entries, dont reenter loop
    jnz .cjeLp
.cjeExit:
    clc
    pop rdi
    pop rcx
.cjeExit2:
    pop rax
    return
.getPSPfromPID:
;Certain PIDs have special meaning and must be ignored.
;If free or a hole, ignore.
;Else, we return the PSP address for the PID.
;Input: rax = PID to check
;Output: ZF=ZE: Special PID, don't operate on it
;        ZF=NZ: rax contains pPSP for the PID.
    cmp rax, mcbOwnerFree   ;Bona-fide must be ignored
    rete
    cmp rax, mcbOwnerHole   ;Bona-fide must be ignored 
    rete
    cmp rax, mcbOwnerNewDOS ;None should ever exist from the POV of share.exe
    je .i1
    cmp rax, mcbOwnerDOS
    retne
.i1:
    test eax, eax   ;eax = 8 or 9 so anding it with itself will clear ZF
    mov rax, qword [r8 + dosPSP]    ;Get the actual DOS PSP value
    return
    

;Critical error handling
errPrintAndHalt:
;********************************************
;This function is called such that the return address points to the 
; null terminated string to print. We never return from this.
;********************************************
;Input: TOS -> Ptr to string to print after header.
;Output: Never return.
    lea rdi, .sHdr
    call .doWrite
    pop rdi         ;Get the passed in string
    call .doWrite   ;Write the passed in ASCIIZ string.
.halt:
    pause           ;Hint the CPU to power down
    jmp short .halt ;Infinite loop, await CTRL+ALT+DEL
.doWrite:
;We write directly to the console driver completely bypassing any 
;redirection, without resorting to using BIOS functions.
;We use the PSP as the buffer for the ioReqPkt.
;We obviously do no error checking or Int 24 invokation if things
; go wrong with this. 
;Input: rdi -> String to print
;Output: Hopefully, string printed.
    mov eax, 1212h  ;Do dos strlen
    int 2Fh         ;Get length of string pointed to by rdi in ecx
    dec ecx ;Drop the terminating null from the count
    retz    ;If this is zero, forget the error
;1) Build request packet
    mov rbx, qword [pPSP]   ;Get the ptr to our PSP
    mov byte [rbx + ioReqPkt.cmdcde], drvWRITE
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov word [rbx + ioReqPkt.status], 0
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    mov qword [rbx + ioReqPkt.bufptr], rdi
;2) Get the driver pointer
    mov rsi, qword [pDosseg]    
    mov rsi, qword [rsi + vConPtr]  ;Get the console driver ptr
;Go driver routine here
    mov eax, 8002h  ;SPECIAL: Enter DOS Level 2 critical section
    int 2ah
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    mov eax, 8102h  ;SPECIAL: Exit DOS Level 2 critical section
    int 2ah
    return
.sHdr:
    db CR,LF,LF
    db "        SCP/DOS EXCEPTION DETECTED!",CR,LF,LF
    db "    SCP/DOS SYSTEM STOP: SHARE INTERNAL ERROR : ",NUL