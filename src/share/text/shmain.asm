;All the main bodies of the share hook calls go here

openShare:           
;Called on file create/open
closeShare:          
;Called on file close
closeCompShare:      
;Close all files for a machine
closeTaskShare:      
;Close all files for a task
closeNameShare:      
;Close file by name
lockFileShare:       
;Lock a file region
unlockFileShare:     
;Unlock file region
checkFileLockShare:  
;Check file region locked
openFileListShare:   
;Get MFT information about file
updateFCBfromSFTShr: 
;UNUSED: Update FCB from the SFT
fstClstOfFCBShare:   
;UNUSED: Get first cluster of FCB
closeDupNetShare:   
;Close a newly created SFT-FCB handle for a procedure.
;Network SFT-FCBs handles are all collapsed into one SFT.
;----------------------------------------------------------------------------
;Input: rsi -> Newly created SFT
;       ax = SFTNdx for this newly created file
;Output: ax = SFTNdx for the file (Same as on input if not a SFT-FCB)
;----------------------------------------------------------------------------
;If
;   rsi -> SFT that is a network SFT-FCB (i.e. bit openNetFCBShr set 
;   in openmode), then we close any duplicate handles using a single
;   SFT to maintain the state (if there are none then we use this SFT
;   as the SFT-FCB). We increment the new main SFT refcount instead of 
;   having multiple SFTs.
;----------------------------------------------------------------------------


renDelCloseShare:
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
updateDirShare:      
;Update dir info across all SFTs for a file. 
;----------------------------------------------------------------------------
;Input: rdi -> SFT to update dir entry from
;       eax = 0: Update the date/time fields only
;           = 1: Update size fields for growth
;           = 2: Update size fields for shrink (i.e. truncate calls)
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
;   Walk the SFT chain along rsi updating dFilesize and dStartClust fields 
;   except for us.
; Else if eax = 2:
;   Walk the SFT chain along rsi updating dFilesize, dStartClust and 
;   setting dRelClust to 0 and dAbsClust to dStartClust except for us.
; Else if eax = 3:
;   Update date/time and dFilesize, dStartClust, dRelClust and dAbsClust 
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
    cmp eax, 3  ;Sanity check but the DOS kernel never puts such a val in
    reta
    test word [rdi + sft.wDeviceInfo], devCharDev | devRedir
    retnz
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
    return
.fileCheck:
    cmp ecx, 3
    je .open
;If not 3 or 0, must be 1 or 2
.gsLp:
;Always update filesize and start cluster info for growth/shrink
    mov eax, dword [rsi + sft.dFilesize]
    mov dword [rdi + sft.dFilesize], eax
    mov eax, dword [rsi + sft.dStartClust]
    mov dword [rdi + sft.dStartClust], eax
    cmp ecx, 1  ;Was this a growth call?
    je .growth
;Here if shrink. Set the cluster information back to the start of the file.
    mov dword [rdi + sft.dAbsClust], eax
    mov dword [rsi + sft.dRelClust], 0  ;Reset the file rel cluster ptr
.growth:
    call .gotoNextSFT
    jnz .gsLp
    jmp short .exit 

.open:
;Here we handle new file opens! Copies data from the 
; topmost (earliest opened) SFT of the SFT chain into the newly opened SFT
    mov eax, dword [rsi + sft.dTimeDate]
    mov dword [rdi + sft.dTimeDate], eax
    mov eax, dword [rsi + sft.dFilesize]
    mov dword [rdi + sft.dFilesize], eax
    mov eax, dword [rsi + sft.dStartClust]
    mov dword [rdi + sft.dStartClust], eax
    mov dword [rdi + sft.dAbsClust], eax
    mov eax, dword [rdi + sft.dRelClust], 0
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