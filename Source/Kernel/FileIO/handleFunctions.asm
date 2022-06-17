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
    call invalidFilePermissions
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
    movzx ebx, word [rdi + sft.wDeviceInfo]
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
writeFileHdl:      ;ah = 40h, handle function
    ret
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
;        File Handle routines       :
;-----------------------------------:
invalidFilePermissions:
    mov eax, errAccDen
    mov word [errorExCde], ax
    xor ecx, ecx    ;To allow for using jrcxz to exit
    stc
    ret

setCurrentSFT:
;Set the pointer in rsi as current SFT 
    mov qword [currentSFT], rsi
    ret
getCurrentSFT:
;Get the current SFT pointer in rsi
    mov rsi, qword [currentSFT]
    ret

getSFTPtr:
;Gets the SFT pointer for a given file handle from the calling application
;On entry:
;   bx = File handle from JFT for calling application
;On exit: CF=NC, SFT found.
; Set the following vars: currentSFT, currentJFT, currentHdl
; Set rdi = currentSFT entry
;         CF=CY, SFT not found, abort! al=BadHdl errorcode
    push rax
    push rbx
    push rsi
    cmp bx, word [maxHndls]  ;current max number of file handles
    jnb .gspFail
    mov rsi, qword [currentPSP]
    mov eax, ebx    ;Save handle number in eax
    movzx rbx, bx
    lea rbx, qword [rsi + psp.jobFileTbl + rbx] 
    mov [currentJFT], rbx   ;Save a pointer to the JFT entry
    mov bl, byte [rbx]   ;Use jft entry to get sft num
    cmp bl, -1  ;Non-existant SFT reference?
    je .gspFail
    mov word [currentHdl], ax   ;Current handle number saved here
    xor eax, eax
    mov rdi, qword [sftHeadPtr]
.gsp0:
    add ax, word [rdi + sfth.wNumFiles]
    cmp al, bl  ;Check if the file header block contains the entry
    jbe .gsp1   ;IF bl is below or equal to al then it does
    cmp rdi, -1 ;End of list
    je .gspFail   ;If we have a number greater than the last entry, fail
    mov rdi, qword [rdi + sfth.qNextSFTPtr] ;Walk the chain
    jmp short .gsp0 ;Search again
.gsp1: 
    ;Now point to the right entry
    sub al, bl  ;Subtract the number from the total so far to get offset
    movzx eax, al
    add rdi, sfth_size  ;Point to first file in table
    test al, al ;Check if rdi points to the first file in this block
    jz .gsp12   ;Skip walking down the sft blocks
.gsp11:
    add rdi, sft_size
    dec al
    jnz .gsp11  ;Keep adding one until al is zero
.gsp12:
    mov rsi, rdi
    call setCurrentSFT ;Set Current SFT pointer to rsi value
    clc
.gspExit:
    pop rsi
    pop rbx
    pop rax
    ret
.gspFail:
    mov eax, errBadHdl  ;al, zero rest of it
    stc
    jmp short .gspExit

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