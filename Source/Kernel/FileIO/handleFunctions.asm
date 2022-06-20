;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
    ret
openFileHdl:       ;ah = 3Dh, handle function
    ret
closeFileHdl:      ;ah = 3Eh, handle function
    ret
readFileHdl:       ;ah = 3Fh, handle function
    lea rsi, readBytes
.common:
    call getSFTPtr ;Get SFT ptr in rdi (if file is r/w-able from machine)
    jb .error
    call setCurrentSFT  ;Set the current SFT (from rdi)
    push qword [currentDTA] ;Save the current Disk Transfer Area
    call rsi
    pop qword [currentDTA]
    jb .errorFromDataTransfer
    call getUserRegs
    mov dword [rsi + callerFrame.rax], ecx  ;Put actual number of bytes tfrd
    and byte [rsi + callerFrame.flags], 0FEh    ;Clear CF
    ret
;Temporary Error handler, simply return with CF set
.error:
.errorFromDataTransfer:
    call getUserRegs
    or byte [rsi + callerFrame.flags], 1    ;Set CF
    ret
writeFileHdl:      ;ah = 40h, handle function
    lea rsi, writeBytes
    jmp readFileHdl.common
deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
    ret
lseekHdl:          ;ah = 42h, handle function, LSEEK
;New pointer passed in edx! ecx will be DOCUMENTED as having to be 0
    call getSFTPtr
    jnc .sftValid
    ;Error code and exit
    ;al (eax) has error code for bad file handle
    mov word [errorExCde], ax
.exitBad:
    mov byte [errorLocus], eLocUnk  ;Unknown Locus
    mov byte [errorAction], eActUsr ;Reinput data
    mov byte [errorClass], eClsNotFnd
    call getUserRegs    ;Get user regs in rsi
    or byte [rsi + callerFrame.flags], 1    ;Set CF
    ret
.sftValid:
    cmp al, 3
    jb .validFunction
    ;Error code and exit
    mov ax, errInvFnc
    jmp short .exitBad
.validFunction:
    cmp al, 1
    ja .seekend
    jb .seekset
;Here we are at seekcur, seek from current (signed)
    add edx, dword [rdi + sft.dCurntOff]    ;Get offset from current
.seekset:
;Seek from the start (unsigned)
    mov dword [rdi + sft.dCurntOff], edx ;Store the new offset
    call getUserRegs    ;Get user regs in rsi
    mov dword [rsi + callerFrame.rdx], edx
    xor al, al  ;Return OK!
    ret
.seekend:
;Here we are at seekend, seek from end (signed)
    add edx, dword [rdi + sft.dFileSize]    ;Add to file size
    jmp short .seekset
changeFileModeHdl: ;ah = 43h, handle function, CHMOD
ioctrl:            ;ah = 44h, handle function
duplicateHandle:   ;ah = 45h, handle function
forceDuplicateHdl: ;ah = 46h, handle function
findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
renameFile:        ;ah = 56h
createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
createNewFile:     ;ah = 5Bh
lockUnlockFile:    ;ah = 5Ch
setHandleCount:    ;ah = 67h
commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ret
;-----------------------------------:
;       Main File IO Routines       :
;-----------------------------------:
readBytes:
;Reads the bytes into the user buffer
    call getCurrentSFT  ;Get current SFT in rdi
    movzx eax, word [rdi + sft.wOpenMode]
    and al, 0Fh ;Eliminate except access mode
    cmp al, WriteAccess
    jne .readable
    mov eax, errAccDen
    mov word [errorExCde], ax
    stc
    ret ;Exit with error code 
.readable:
writeBytes:
;Writes the bytes from the user buffer
    call getCurrentSFT  ;Get current SFT in rdi
    movzx eax, word [rdi + sft.wOpenMode]
    and al, 0Fh ;Eliminate except access mode
    cmp al, ReadAccess
    jne .writeable
    mov eax, errAccDen
    mov word [errorExCde], ax
    stc
    ret ;Exit with error code 
.writeable:
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:
setCurrentSFT:
;Set the pointer in rdi as current SFT 
    mov qword [currentSFT], rdi
    ret
getCurrentSFT:
;Get the current SFT pointer in rdi
    mov rdi, qword [currentSFT]
    ret
    
getSFTPtrfromSFTNdx:    ;Int 4Fh AX=1216
;Return a pointer to the SFT entry in rdi
;Input: rbx = Valid SFT ndx number (word)
;Output: rdi = SFT pointer
    mov rdi, qword [sftHeadPtr] ;Get head of SFT pointer
.walk:
    cmp bx, word [rdi + sfth.wNumFiles]
    jb .thisTable
    sub bx, word [rdi + sfth.wNumFiles] ;Subtract
    mov rdi, qword [rdi + sfth.qNextSFTPtr] ;Goto next table
    cmp rdi, -1
    jne .walk
    stc
    ret
.thisTable:
    push rax
    push rdx
    mov eax, sft_size
    mul ebx
    add rdi, rax    ;Shift rdi to go to SFT entry in current table
    pop rdx
    pop rax
    add rdi, sfth_size  ;Go past the header
    ret

getSFTNdxFromHandle:    ;Int 4Fh AX=1220h
;Return a zero extended value in rdi for the SFT entry
;Input: bx = JFT handle
;Output: CF=NC => rdi = SFT ndx
;        CF=CY => al = Error code, Fail
;rbx destroyed
    movzx ebx, bx   ;Ensure we zero extended
    cmp bx, word [maxHndls] ;0-19 acceptable ONLY!
    jb .ok
    mov al, errBadHdl
    stc
    ret
.ok:
    mov rdi, qword [currentPSP]
    movzx rdi, byte [rdi + psp.jobFileTbl + rbx] ;Use rbx as index in tbl
    clc
    ret
getSFTPtr:
;This gets the SFT pointer and checks it was opened by this machine
;Input: bx = JFT handle
;Output: CF=NC: rdi = SFT pointer
;        CF=CY: Error, ax=Error code
    call derefSFTPtr
    jnc .ok
    ret ;Error return with CF=CY
.ok:
    push rax
    movzx eax, word [machineNum]    ;Get the machine number from SDA
    cmp ax, word [rdi + sft.wMachNum]   ;Compare to SFT machine number
    pop rax
    je .exit    ;If the file belongs to this machine, proceed!
    mov al, errBadHdl   ;Error code
    stc ;Reset CF
.exit:
    ret
derefSFTPtr:
;Walk the whole way from a handle to SFT pointer (for the current process)
;Input: bx = File handle
;Output: CF=NC: rdi = SFT pointer
;        CF=CY: Error, ax=Error code
    call getSFTNdxFromHandle    ;Get the ptr to the value in rdi
    jb .fail
    cmp byte [rdi], -1  ;Is this JFT entry unassigned?
    jne .ok
.fail:
    mov al, errBadHdl
    stc
    ret
.ok:
    push rbx    ;Preserve the JFT handle
    movzx ebx, byte [rdi]  ;Get byte entry into rbx
    call getSFTPtrfromSFTNdx    ;Get SFT pointer in rdi
    pop rbx 
    ret

setCurrentJFTandHdl:
; Set the following vars currentJFT, currentHdl
;Input: bx = JFT Handle number
    mov word [currentHdl], bx
    push rdi
    call getSFTNdxFromHandle
    mov qword [curJFTNum], rdi
    pop rdi

copySFTtoSDA:
;Called with rsi pointing to SFT structure
;Prepares the scratch SFT in SDA for use
    lea rdi, scratchSFT
    mov rsi, qword [currentSFT]   ;Get current SFT
    jmp short copySScommon
copySDAtoSFT:
    lea rsi, scratchSFT
    mov rdi, qword [currentSFT]   ;Get current SFT
copySScommon:
    push rcx
    mov ecx, sft_size
    rep movsb   ;Copy
    pop rcx
    ret
getBytesTransferred:
    mov ecx, dword [tfrCntr]   ;Get bytes left to transfer
    neg ecx ;Multiply by -1
    add ecx, dword [tfrLen]     ;Add total bytes to transfer
    ret ;Return bytes transferred in ecx
updateCurrentSFT:
;Updates the Current SFT fields before returning from a file handle operation
    push rsi
    push rax
    mov rsi, qword [currentSFT]
    mov eax, dword [currByteA]
    mov dword [rsi + sft.dCurntOff], eax
    mov eax, dword [currClustA]
    mov dword [rsi + sft.dAbsClusr], eax
    mov eax, dword [currClust]
    mov dword [rsi + sft.dRelClust], eax
    pop rax
    pop rsi
    ret
readBytesASCII:
;Input: ecx = number of bytes to read in ASCII mode
writeBytesASCII:
;Input: ecx = number of bytes to write in ASCII mode
    ret
readWriteBytesBinary:
;Input: ecx = number of bytes to read in Binary mode
;       rdi = Points to where in caller buffer to place bytes
;       rsi = Points to where in DOS buffer to place pointer
;xchg rdi and rsi if rwFlag is set (i.e. a write operation)
;Preserve rcx so we know how many bytes transferred
;Update the currByteA variable
;Returns (rsi and rdi) + (ecx on entry)
    push rcx
    test byte [rwFlag], -1   ;Is this a write operaiton
    jz .noSwap
    xchg rdi, rsi
.noSwap:
    rep movsb
    pop rcx
    add dword [currByteA], ecx  ;Move file pointer by ecx bytes
    sub dword [tfrCntr], ecx   ;Subtract from the number of bytes left
    ret
oldReadHdl: ;Kept for backup and reference
    mov byte [rwFlag], 0    ;Read
    ;bx has file handle, ecx has number of bytes to read
    ;Set the following vars: currentSFT, currentJFT, currentHdl
    call getSFTPtr  ;Get SFT ptr in var in rdi
    jc lseekHdl.exitBad ;If file handle not good, recycle error (in al)
    ;Now check if we have permissions to read from file
    movzx eax, word [rdi + sft.wOpenMode]   ;Get handle permissions in ax
    and al, 0Fh ;Save low nybble
    cmp al, WriteAccess ;Was this file opened with write access only?
    jne .readable
    mov eax, errAccDen
    mov word [errorExCde], ax
    ;Set CF on caller stack
    call getUserRegs
    or byte [rsi + callerFrame.flags], 1
    ret ;Exit with error code in al
.readable:
    mov eax, dword [rdi + sft.dCurntOff]
    mov dword [currByteA], eax
    ;If the file is readable, check if it is a disk or char device
    movzx ebx, word [rdi + sft.wDeviceInfo]
    test ebx, devRedirDev | devCharDev  ;Either of these get handled separately
    jnz .notDiskDev
    mov rbp, qword [rdi +sft.qPtr]  ;Get DPB pointer
    call setWorkingDPB  ;Set the DPB pointer as working
    mov bl, byte [rbp + dpb.bDriveNumber]
    mov byte [workingDrv], bl
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    ;Here we divide
    xor edx, edx
    div ebx ;Divide the number of bytes by bytes per sector
    ;eax has sector number in file
    ;edx has offset in sector
    mov dword [currByte], edx
    mov edx, eax    ;Save sector number in edx
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shr eax, cl ;Divide eax by sectors/cluster (to get cluster number)
    mov dword [currClust], eax    ;Save rounded down value (cluster number)
    shl eax, cl     ;Go up again
    sub edx, eax    ;Get the sector offset INTO the cluster in eax
    mov byte [currSect], dl ;Save this number
;Now we need to find the absolute cluster number
    mov edx, dword [currClust]
    mov eax, dword [rdi + sft.dStartClust]  ;Get the start cluster for the file
    xor ecx, ecx    ;If fail, have 0 bytes ready
.clusterSearch:
    cmp eax, -1
    je .exitSetFlag ;This is a fail condition. The handle is past the EOF
    call walkFAT    ;eax has next cluster
    dec edx
    jnz .clusterSearch
;eax should have the absolute cluster of the file pointer
    mov dword [currClustA], eax
    call getStartSectorOfCluster    ;Get start sector of cluster
    movzx ecx, byte [currSect]  ;Get sector offset into cluster 
    add rax, rcx    ;Get starting sector number
    mov qword [currSectA], rax  ;Save it!
;Update SFT with entries
    call updateCurrentSFT
;Now enact data transfer
    call getDataSector  ;Gets sector in [currSectA] in [currBuff]
    jc .exitFail
;Now load rbx with function to call
    lea rbx, readWriteBytesBinary
    lea rdx, readBytesASCII
    test byte [rdi + sft.wDeviceInfo], devBinary
    cmovz rbx, rdx  ;Move only if bit not set i.e. in ASCII mode
    mov qword [dosReturn], rbx ;Save the function to call in this var
    call getUserRegs
    mov rdi, qword [rsi + callerFrame.rdx]  ;Get Read Destination
    mov ecx, dword [rsi + callerFrame.rcx]  ;Get number of bytes to transfer
    mov dword [tfrLen], ecx ;Set user requested transfer length in var
    call getCurrentSFT  ;Set rsi to current SFT
    ;Check if the transfer length is possible
    ;If not, then transfer the max length possible
    mov ecx, dword [rsi + sft.dFileSize]    ;When file ptr == ecx, EOF
    sub ecx, dword [currByteA]  ;Get the bytes left in the file
    mov edx, dword [tfrLen]
    cmp edx, ecx ;If userRequest > bytesLeftInFile, swap 
    cmova edx, ecx
    mov dword [tfrLen], edx 
    
    movzx edx, word [rbp + dpb.wBytesPerSector]
    movzx eax, byte [currByte]  ;Get current byte in the sector
    sub edx, eax    ;edx has the remaining bytes to read in this sector
    mov rsi, qword [currBuff]
    or byte [rsi + bufferHdr.bufferFlags], refBuffer ;Set referenced bit
    lea rsi, qword [rsi + bufferHdr.dataarea]    ;Goto the data area
    add rsi, rax    ;Go to the current byte in the sector
    ;Now we check to see if we have less than a partial sector's worth of 
    ; data to transfer.
    mov ecx, dword [tfrLen] ;Get the current settled length of transfer
    cmp ecx, edx    ;edx has the bytes left in the sector
    cmova ecx, edx  ;if transferLength > bytes left in sector, swap
    mov dword [tfrLen], ecx
    ;Here, tfrLen is settled, so set tfrCntr too
    mov dword [tfrCntr], ecx   ;Populate the counter
.mainReadLoop:
    call qword [dosReturn]  ;Call the tfr func, ecx rets num. bytes transferred
    jz .exit   ;If we return with ZF=ZE then we are done!
    ;Else we must goto the next sector and repeat
    call getNextSectorOfFile    ;Increments the cluster and sector vars
    jc .exitFailInTfr
    call getDataSector  ;Get data buffer
    jc .exitFailInTfr
    call updateCurrentSFT   ;Ensure the SFT is up to date after a transfer
    movzx ecx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    mov edx, dword [tfrCntr]    ;Get bytes left to transfer in edx
    cmp edx, ecx    ;If bytes left > bytes in sector, dont swap!
    cmovb ecx, edx
    ;Reposition rsi again
    mov rsi, qword [currBuff]
    or byte [rsi + bufferHdr.bufferFlags], refBuffer ;Set referenced bit
    lea rsi, qword [rsi + bufferHdr.dataarea]    ;Goto the data area
    jmp short .mainReadLoop
.exit:
    call getCurrentSFT
    test word [rsi + sft.wDeviceInfo], blokDevDTSet ;Should I set the time/date?
    ;For now do nothing, but eventually, make CLOCK$ request
    call getBytesTransferred    ;Gets bytes transferred in ecx
    call getUserRegs
    mov dword [rsi + callerFrame.rcx], ecx
    and byte [rsi + callerFrame.flags], 0FEh    ;Clear CF
    xor al, al  ;No error
    ret
.exitSetFlag:
    call getUserRegs
    mov dword [rsi + callerFrame.rcx], ecx
    or dword [rsi + callerFrame.flags], 1    ;Set CF
    ret
.exitFailInTfr:
    call getBytesTransferred    ;Gets bytes transferred in ecx
.exitFail:
;Exit on Int 44h
    mov eax, errFI44
    mov word [errorExCde], ax
    jmp short .exitSetFlag
.notDiskDev:    ;qPtr here is a device driver
;Here only char devices. Redirector is not yet implemented!
;Arrive here with ebx = wDeviceInfo for device
    test ebx, devRedirDev  ;Test for network redirector drive
    jz .charDev
    mov al, errNoNet    ;No network pls
    xor ecx, ecx    ;No bytes transferred
    jmp short .exitSetFlag
.charDev:
;When reading from a char device, if it is the console, we must give it
; special treatment if the handle is in ASCII mode.