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
setupVarsForTransfer:
;Computes the actual bytes to be transferred and 
; sets up internal variables for the transfer. 
;Works for both reading and writing
;Input: ecx = User desired Bytes to transfer
;       rdi = SFT pointer for the file
;Output: ecx = Actual Bytes that will be transferred 
;
;Note: Does not account for a FULL disk. When writing,
; if the disk will get full as a result of the write,
; stop at the last byte before the transfer. If the 
; file pointer is past the last free byte, write 0
    mov rsi, qword [rdi + sft.qPtr] ;Get qPtr in rsi
    mov qword [qPtr], rsi ;Save whatever pointer here
    mov eax, dword [rdi + sft.dCurntOff]    ;Get current offset into file
    mov dword [currByteF], eax  ;Save Current byte in the file
    test word [rdi + sft.wDeviceInfo], devRedirDev | devCharDev ;If not disk...
    jz setupVarsForDiskTransfer
    clc
    ret ;Else just exit here
setupVarsForDiskTransfer:
;Extension of the above, but for Disk files only
;Input: ecx = User desired Bytes to transfer
;       rdi = SFT pointer for the file
;Output: ecx = Actual Bytes that will be transferred 
    mov rbp, qword [workingDPB] ;Get the workingDPB (the same as qPtr)
    mov bl, byte [rbp + dpb.bDriveNumber]
    mov byte [workingDrv], bl   ;Set working drive number
    mov eax, dword [currByteF]  ;Get current byte in file
    movzx ebx, word [rbp + dpb.wBytesPerSector] ;Get bytes per sector
    xor edx, edx    ;Zero rdx
    div ebx ;Divide current byte in file by bytes per sector
    ;eax has 0 based file relative sector (cannot grow beyond 03FFFFFh)
    ;edx has the offset into that sector
    mov dword [currSectF], eax
    mov word [currByteS], dx ;CurrbyteS is a word!

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
    mov eax, dword [currByteF]
    mov dword [rsi + sft.dCurntOff], eax
    mov eax, dword [currClustD]
    mov dword [rsi + sft.dAbsClusr], eax
    mov eax, dword [currClustF]
    mov dword [rsi + sft.dRelClust], eax
    pop rax
    pop rsi
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
    add dword [currByteF], ecx ;Move file pointer by ecx bytes
    sub dword [tfrCntr], ecx   ;Subtract from the number of bytes left
    ret