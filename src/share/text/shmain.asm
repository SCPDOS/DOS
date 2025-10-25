;-------------------------------------------------------------------------
;This file contains the all the functions that have external linkage.
;
; Int 2Fh handler
; open           
; close          
; closeAllByMachine      
; closeAllByProcess      
; closeAllByName    
; lockFile       
; unlockFile     
; checkRegionLock  
; getMFTInfo   
; updateFCB 
; getFirstClusterFCB   
; closeNetworkFiles   
; closeRenDel
; dirUpdate      
;-------------------------------------------------------------------------

i2fHandler:
    cmp ah, 10h ;Is this call for us?
    jne .gotoNext
    test al, al ;Do we test presence of share?
    jnz .exit   ;If not, probably badly behaved code. Let it return silently
    mov al, -1  ;Else, indicate we are already installed
.exit:
    iretq
.gotoNext:
    jmp qword [pOldI2Fh]

open:           
;Called on file create/open. Creates/Finds an MFT entry for the file
; being opened and adds the new SFT to the MFT chain.
;----------------------------------------------------------------------------
;Input: qword [fname1Ptr] -> Fully qualified pathname to open
;       qword [currentSFT] -> SFT that was just created/opened
;           This SFT needs the following fields filled in:
;               .wOpenMode
;               .bFileAttrib
;               .dMID
;               .qPID
;               .pMFT
;       qword [qPID] -> Process ID of the requesting task
;       dword [dMID] -> Machine ID of the requesting task
;Output: CF=NC: Proceed happily with SFT linked into the MFT for the file.
;        CF=CY: eax = Error code for request. Abort, free SFT.
;Use r8 as DOSSEG base
;----------------------------------------------------------------------------
    call critEnter
    push r8
    mov r8, qword [pDosseg]
    mov rsi, qword [r8 + fname1Ptr] ;Get the filename pointer
    call getMFT
    jc .exit
;rbx -> MFT for this file here.
    mov rsi, qword [r8 + currentSFT]    ;Get the current SFT now
    call addSFTtoMFT    ;Bubble CF
.exit:
    pop r8
    call critExit
    return

close:          
;Called on file close. Frees all sharing information associated with
; a file.
;Input: rdi -> SFT we are closing.
    call critEnter
    mov rbx, qword [rdi + sft.pMFT]
    test rbx, rbx   ;If this is an SFT from before Share loaded, do nothing!
    jz .exit
    movsx eax, word [rdi + sft.wNumHandles] ;Get the count
    test eax, eax   ;If count is zero, free all sharing data
    jz .goClose
    inc eax         ;If this handle is -1, also free any sharing data.
    jnz .exit       ;If not -1 or 0, then inc eax > 0 and so we just exit
.goClose:
    call freeLocks  ;Free all locks associated to this SFT
    call removeSFTfromMFT ;Delink this SFT from the SFT link
    jnz .exit       ;If not last SFT in MFT chain, dont free MFT
    call freeMFT    ;If no more SFTs, free the MFT. Do small GC.
.exit:
    call critExit
    return

closeAllByMachine:      
;Close all files for a machine
    stc
    return
closeAllByProcess:      
;Close all files for a task
    stc
    return
closeAllByName:      
;Close file by name
    stc
    return
lockFile:       
;Lock a file region
    stc
    return
unlockFile:     
;Unlock file region
    stc
    return
checkRegionLock:  
;Check file region locked
    clc
    return
getMFTInfo:   
;Get MFT information about file
    stc
    return
updateFCB: 
;UNUSED: Update FCB from the SFT
    stc
    return
getFirstClusterFCB:   
;UNUSED: Get first cluster of FCB
    stc
    return
closeNetworkFiles:   
;Close a newly created SFT-FCB handle for a procedure.
;Network SFT-FCBs handles are all collapsed into one SFT.
;----------------------------------------------------------------------------
;Input: rsi -> Newly created SFT
;       ax = SFTNdx for this newly created file
;Output: CF=NC: ax = SFTNdx for the SFT (Same as on input if not a SFT-FCB)
;        CF=CY: No SFT found
;----------------------------------------------------------------------------
;If
;   rsi -> SFT that is a network SFT-FCB (i.e. bit openNetFCBShr set 
;   in openmode), then we close any duplicate handles using a single
;   SFT to maintain the state (if there are none then we use this SFT
;   as the SFT-FCB). We increment the new main SFT refcount instead of 
;   having multiple SFTs.
;----------------------------------------------------------------------------
    push r8
    mov r8, qword [pDosseg]
    movzx eax, word [rsi + sft.wOpenMode]
    and eax, 0F0h    ;Save the share bits
    cmp eax, openNetFCBShr
    jne .exitNormal
    xor ebx, ebx
.ptrLp:
    push rbx
    mov eax, 1216h  ;getSFTPtrfromSFTNdx -> Get in rdi the SFT for index bx
    int 2Fh
    pop rbx
    jc .exit
    cmp rdi, rsi    ;Skip ourselves
    je .ptrNext
    cmp word [rdi + sft.wNumHandles], 0 ;Handle must have empty count
    je .ptrNext
    movzx eax, word [rsi + sft.wOpenMode]
    cmp ax, word [rdi + sft.wOpenMode]
    jne .ptrNext
    mov eax, dword [rsi + sft.dMID]
    cmp dword [rdi + sft.dMID], eax
    jne .ptrNext
    mov rax, qword [rsi + sft.qPID]
    cmp qword [rdi + sft.qPID], rax
    jne .ptrNext
    mov rax, qword [rsi + sft.pMFT]
    cmp qword [rdi + sft.pMFT], rax
    je .ptrFnd  ;If all this, then we have a duplicate net handle
.ptrNext:
    inc ebx
    jmp short .ptrLp
.ptrFnd:
;rsi is a duplicate of rdi for this process
    mov word [rsi + sft.wNumHandles], 0 ;Free the handle we just allocated
    push rdi    ;rdi is the master FCB SFT 
    push rbx    ;Save the sftindex
    mov rdi, rsi
    call close  ;Close the current SFT now
    pop rax     ;Get the sftindex into eax
    pop rdi
    inc word [rdi + sft.wNumHandles]    ;Add one now
    xor ebx, ebx
.jftlp:
    push rax
    mov eax, 1220h  ;getJFTPtr -> Get ptr to this jft entry in psp in rdi
    int 2Fh
    pop rax
    jc .exit
    cmp byte [rdi], al
    je .jftFnd
    inc ebx
    jmp short .jftlp
.jftFnd:
    mov byte [rdi], -1
    mov eax, ebx
    jmp short .exit
.exitNormal:
    movzx eax, word [r8 + currentHdl]   ;Get the handle value to return
.exit:
    pop r8
    return

closeRenDel:
;On rename/delete/setattr, this function is called to check we can proceed
; with the operation.
;Finds MFT based on the filename in fname1Ptr in the dosseg
;----------------------------------------------------------------------------
;Input: fname1Ptr -> Filename to do check on
;Output: Nothing
;----------------------------------------------------------------------------
;If 
;   the file is not open, then we return ok.
;Else if 
;   the file is opened by us uniquely and is open in compatibility mode,
;   then close the file and return ok.
;Else, we fail.
;----------------------------------------------------------------------------
    push r8
    mov r8, qword [pDosseg]
    call critEnter
.again:
    mov rsi, qword [r8 + fname1Ptr] ;Get the filename pointer
    call findMFT    ;Looks for this file
    jc .exit    ;If the file doesn't exist, exit
;File MFT found. rbx -> MFT. 
; Now check that all the files on this chain have the mid and pid of 
; the requester and (are in compatibility mode or a net SFT).
    mov rsi, qword [rbx + mft.pSFT] ;This cant be empty so it must be an SFT
    mov rdi, rsi    ;Save this SFT pointer in rdi
.scanLp:
    mov eax, dword [r8 + dMID]
    cmp eax, dword [rsi + sft.dMID]
    jne .exit
    mov rax, qword [r8 + qPID]
    cmp rax, qword [rsi + sft.qPID]
    jne .exit
    movzx eax, word [rsi + sft.wOpenMode]
    and eax, 0F0h    ;Get the sharing mode
;If compatibility mode or net fcb, proceed with close
    cmp eax, openNetFCBShr
    je .next
    cmp eax, openCompat
    je .next
.exit:
    call critExit
    pop r8
    return
.next:
    mov rsi, qword [rsi + sft.pNextSFT]
    test rsi, rsi
    jnz .scanLp
;Here we must close all the handles referring to this SFT
;rdi -> First SFT 
    mov qword [r8 + currentSFT], rdi    ;Set this SFT as current SFT
    mov rsi, rdi
    xor ebx, ebx
.sftIndxLp:
    push rbx
    mov eax, 1216h  ;Get SFT Ptr from SFT Index into rdi
    int 2Fh
    pop rbx
    inc ebx
    cmp rsi, rdi    ;Was this index correct?
    jne .sftIndxLp
    dec ebx         ;Decrement it back, we are looking for this number
;Now we walk the Arena Chain to find every task ID and close the
; JFT entry for this file if the SFTIndex exists in that task's JFT
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
    je .mcbEnd
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
.mcbEnd:
    mov eax, 1201h ;Close the currentSFT
    int 2Fh
    jmp .again  ;Now do this again until no more files

dirUpdate:      
;Update dir info across all SFTs for a file. 
;----------------------------------------------------------------------------
;Input: rdi -> SFT to update dir entry from
;       eax = 0: Update the date/time fields only
;           = 1: Update size fields for growth
;           = 2: Update size fields for shrink (i.e. truncate/open calls)
;           = 3: Update all fields
;Output: Nothing, all SFTs in MFT chain updated.
;        rax and rcx trashed.
;----------------------------------------------------------------------------
; If rdi is a chardev or a redir file, return
; Get MFT pointer. 
; If 
;   MFT pointer is null, return.
; Else
;   Get first SFT pointer from MFT into rsi   
; If eax = 0:
;   Walk the SFT chain along rsi updating date/time fields except for us.
; Else if eax = 1:
;   Walk the SFT chain along rsi updating dFileSize and dStartClust fields 
;   except for us.
; Else if eax = 2:
;   Walk the SFT chain along rsi updating dFileSize, dStartClust and 
;   setting dRelClust to 0 and dAbsClust to dStartClust except for us.
; Else if eax = 3:
;   Update date/time and dFileSize, dStartClust, dRelClust and dAbsClust 
;   fields for us from the oldest SFT in the SFT chain for this MFT. 
;   
; Return.
;**** Note ****
;In open and shrink we reset dRelClust to 0 and dAbsClust to dStartClust
;to signify that all file IO should start from the start of the file.
;This is a future optimisation as right now, the DOS kernel never depends 
;on these values. These values are however correctly synchronised on each
;read/write so in the future, we can relatively easily add a check to make
;use of these values, unless they are at the start of the file at which
;case we use the current algorithms.
;----------------------------------------------------------------------------
    test word [rdi + sft.wDeviceInfo], devCharDev | devRedir
    retnz
    push rcx
    push rsi
    mov rsi, qword [rdi + sft.pMFT] ;Get MFT pointer in rsi
    test rsi, rsi   ;If this is a null pointer, return
    retz
    call critEnter
    mov rsi, qword [rsi + mft.pSFT] ;Get first SFT of MFT in rsi
    mov ecx, eax
    test ecx, ecx   ;Is it 0?
    jnz .fileCheck
;Here we do date/time update only.
    mov eax, dword [rdi + sft.dTimeDate]
.dtlp:
    mov dword [rsi + sft.dTimeDate], eax
    call .gotoNextSFT
    jnz .dtlp
.exit:
    call critExit
.exitNoCrit:
    pop rsi
    pop rcx
    return
.fileCheck:
    cmp ecx, 3
    je .open
;If not 3 or 0, must be 1 or 2
.gsLp:
;Always update filesize and start cluster info in rsi for growth/shrink
; to the filesize and start cluster of rdi
    mov eax, dword [rdi + sft.dFileSize]
    mov dword [rsi + sft.dFileSize], eax
    mov eax, dword [rdi + sft.dStartClust]
    mov dword [rsi + sft.dStartClust], eax
    cmp ecx, 2      ;Was this a shrink call?
    je .gsDoShrink  ;Reset the cluster info if so
;Here if we are a grow call. Check if the SFT in rsi was newly created.
;If not, skip the reset below. Else, we set the absolute cluster now to
; the start cluster to ensure that the SFTs all correctly have the same
; cluster info.  
    cmp dword [rsi + sft.dAbsClust], 0 ;Is this sft just created?
    jne .gsNextFile   ;If not, and a grow call, skip the below
.gsDoShrink:
;Set the cluster information back to the start of the file.
    mov dword [rsi + sft.dAbsClust], eax
    mov dword [rsi + sft.dRelClust], 0  ;Reset the file rel cluster ptr
.gsNextFile:
    call .gotoNextSFT
    jnz .gsLp
    jmp short .exit 

.open:
;Here we handle new file opens! Copies data from the 
; topmost (earliest opened) SFT (rsi) of the SFT chain into 
; the newly opened SFT (rdi)
    mov eax, dword [rsi + sft.dTimeDate]
    mov dword [rdi + sft.dTimeDate], eax
    mov eax, dword [rsi + sft.dFileSize]
    mov dword [rdi + sft.dFileSize], eax
    mov eax, dword [rsi + sft.dStartClust]
    mov dword [rdi + sft.dStartClust], eax
    mov dword [rdi + sft.dAbsClust], eax
    mov dword [rdi + sft.dRelClust], 0
    jmp short .exit

.gotoNextSFT:
;Returns in rsi the next SFT entry.
;Input: rdi -> SFT we are updating from.
;       rsi -> SFT we just updated.
;Output: ZF=NZ: rsi -> Next SFT in the chain.
;        ZF=ZE: End of SFT chain.
    mov rsi, qword [rsi + sft.pNextSFT]
    test rsi, rsi
    retz
    cmp rsi, rdi
    je .gotoNextSFT
    return