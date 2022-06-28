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
    call rsi    ;Get back in ecx the bytes transferred!
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
;Reads the bytes into the user buffer for the setup SFT (currentSFT)
;Input: ecx = Number of bytes to read
;Output: ecx = Number of bytes read
;Number of bytes read 
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
    call setupVarsForTransfer
    jecxz .exitOk  ;If ecx = 0 (number of bytes to transfer = 0), exit
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .notRedir
    mov eax, 1108h  ;Call Redir Read Bytes function
    int 4fh ;Call redir (tfr buffer in DTA var, ecx has bytes to tfr)
    ret
.exitOk:
    clc
    ret
.notRedir:
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz readCharDev
    call dosCrit1Enter
    call readDiskFile
    call dosCrit1Exit
    ret
readCharDev:
;rdi points to sft for char dev to read
;ecx has the number of bytes to transfer
;Vars have been set up and DTA has the transfer address
;Returns in ecx, the actual bytes transferred
    mov byte [errorLocus], eLocChr  ;Error is with a char device operation
    mov rdi, qword [currentDTA] ;Get the DTA for this transfer in rdi
    mov bx, word [rdi + sft.wDeviceInfo]    ;Get dev info
    test bl, charDevNoEOF   ;Does our device NOT generate EOF's on reads?
    jz rwExitOk    ;If it does, jump to exit as if EOF has been hit
    test bl, charDevNulDev  ;Is our device the NUL device?
    jnz .notNul
    ;If it is the NUL device, we can simply return unsucessfully!
    ;NUL never transfers bytes 
    xor eax, eax    ;Set ZF so the next read causes EOF!
    jmp rwExitOk    ;Goto exit
.notNul:
    test bl, charDevBinary
    jnz .binary
    ;Here if the device is in ASCII mode
    test bl, charDevConIn   ;Is this device STDIN?
    jz .generalASCII    ;If not, goto generalASCII, else fallthru
.consoleInput:
    ;Console input here
    call swapVConDriver    ;Prepare CON Useage!
    

.binary:
    ;Setup registers for transfer
    mov rbx, rdi    ;Transfer the buffer pointer into rbx
    xor rbp, rbp    ;Indicate Char device to the function
    ;ecx has the number of bytes to transfer directly
    call primReqReadSetup   ;Setup req hdr for read and get hdr addr in rbx 
    mov rsi, qword [workingDD]  ;Get the working device driver
    call goDriver   ;Make the request
    mov rdx, rdi    ;Save transfer buffer in rdx
    movzx edi, word [primReqHdr + ioReqPkt.status] ;Get status word in di
    test edi, drvErrStatus  ;Did an error occur?
    jz .binNoError
    ;ERROR HERE! Prepare for Int 44h (if SFT allows us to issue Int 44h)
    mov ah, 86h ;Char device, data error signature
    call binaryCharDevErr   ;ah = has part of the error 
    ;al now has the response
    ;Cannot return Abort as Abort returns to command interpreter through DOS
    cmp al, critIgnore
    je .binNoError ;Simply proceed as normal
    mov rdi, rdx    ;Get back the buffer if it is a retry operation
    cmp al, critFail
    jne .binary ;If not fail, re-try the operation (ecx isn't touched)
    ;Fallthrough here for fail!
.failExit:
    
.binNoError:
    ;Get number of bytes transferred into 
    mov eax, dword [primReqHdr + ioReqPkt.tfrlen]   ;Get bytes transferred
    neg eax ;make it into -eax
    lea ecx, dword [ecx + eax]  ;ecx has bytes to transfer, -eax has bytes trfrd
    ;ecx now has bytes left to transfer
    push rax    ;Save value on stack
    xor eax, eax ;Set ZF
    inc eax ;Clear ZF
    pop rax ;Get back the original value
    jmp rwExitOk    ;GoExit with ecx=Bytes left to read
.generalASCII:
    ;ecx has bytes to transfer here
    ;Setup registers for transfer
    mov rbx, rdi    ;Move the DTA address into rbx for readSetup
    push rcx
    mov ecx, 1  ;Get one char
    xor rbp, rbp    ;Indicate a char device
    call primReqReadSetup   ;Setup request
    pop rcx
    ;rbx now has request header ptr
    mov rsi, qword [workingDD]  ;Get device driver header ptr in rsi
.asciiReadChar:
    mov rdx, rdi    ;Save the current buffer pointer position in rdx
    call checkBreakOnCon    ;Check we don't have a ^C pending on CON
    call goDriver   ;If no ^C found (which exits DOS) Make request!
    movzx edi, word [primReqHdr + ioReqPkt.status] ;Get status word in di
    test edi, drvErrStatus  ;Did an error occur?
    jz .asciiNoError
    call asciiCharDevErr    ;Call Int 44h
    ;Now setup number of bytes to transfer to 1 if the user requests retry
    mov dword [primReqHdr + ioReqPkt.tfrlen], 1
    mov rdi, rdx    ;Get the buffer position back into rdi
    cmp al, critFail
    je .failExit
    cmp al, critRetry
    je .asciiReadChar
    ;Ignore here, pretend NULL CHAR was read
    xor al, al
    jmp short .asciiIgnoreEP
.asciiNoError:
;Now process the char, add 1 to the transfer buffer (and rdi->BufferPtr)
; and dec 1 from ecx (tfrCntr is dealt with later)
;Preserve RBX, RSI
;Check EXACTLY 1 char was transferred. Any other value => exit from request
    mov rdi, rdx    ;Get the buffer position back into rdi
    cmp dword [primReqHdr + ioReqPkt.tfrlen], 1
    jne rwExitOk    ;Exit request if more than 1 char was tranferred (ZF=NZ)
    mov al, byte [rdi]  ;Get byte just input from driver in al
.asciiIgnoreEP:
    inc qword [primReqHdr + ioReqPkt.bufptr]   ;Goto next char position
    inc rdi ;Also advance register pointer
    cmp al, EOF ;Was this char EOF?
    je rwExitOk
    cmp al, CR  ;Was this char CR?
    loopne .asciiReadChar   ;dec rcx, jnz .asciiReadChar
    ;Fallthrough also if al = CR (i.e ZF=ZE)
    inc al  ;make ZF=NZ
    jmp rwExitOk    ;Called with ecx = Number of bytes LEFT to transfer
    
readDiskFile:
    mov byte [errorLocus], eLocDsk  ;Error is with a disk device operation

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
    call setupVarsForTransfer

rwExitOk:
;Input: ecx = Number of bytes left to transfer!
;       ZF=ZE => clear bit 6 of deviceInfo Word ZF=NZ => preserve bit 6
    mov dword [tfrCntr], ecx    ;Update bytes left to transfer
    jnz .skipbitClear
    call getCurrentSFT  ;Get currentSFT in rdi
    ;The disk transfer must've flushed by now. 
    and byte [rdi + sft.wDeviceInfo], ~(blokDevNotFlush|charDevNoEOF) ;OR
    ;Next char dev read should give EOF.
.skipbitClear:  ;Or skip that entirely
    call updateCurrentSFT   ;Return with CF=NC and ecx=Bytes transferred
    ret
rwExitBad:
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
updateCurrentSFT:
;Updates the Current SFT fields before returning from a file handle operation
;Return: ecx = Actual bytes transferred and CF=NC
    push rdi
    mov rdi, qword [currentSFT]
    mov ecx, dword [tfrLen]     ;Get bytes to transfer
    sub ecx, dword [tfrCntr]    ;Subtract bytes left to transfer
    ;ecx has bytes transferred
    test word [rdi + sft.wDeviceInfo], devCharDev   ;Char dev?
    jnz .exit
    push rax
    mov eax, dword [currClustD]
    mov dword [rdi + sft.dAbsClusr], eax
    mov eax, dword [currClustF]
    mov dword [rdi + sft.dRelClust], eax
    pop rax
    jecxz .exit ;Skip this if ecx = 0
    add dword [rdi + sft.dCurntOff], ecx    ;Add to the current offset in file
.exit:
    pop rdi
    clc
    ret
setupVarsForTransfer:
;Computes the actual bytes to be transferred and 
; sets up internal variables for the transfer. 
;Works for both reading and writing
;Input: ecx = User desired Bytes to transfer
;       rdi = SFT pointer for the file
;Output: ecx = Actual Bytes that will be transferred 
;Setup BOTH: tfrLen, tfrCntr, qPtr 
;      DISK: workingDPB, workingDrv, currByteF/S, currSectF/C, currClustF
;
;Note: Does not account for a FULL disk. When writing,
; if the disk will get full as a result of the write,
; stop at the last byte before the transfer. If the 
; file pointer is past the last free byte, write 0
    mov rsi, qword [rdi + sft.qPtr] ;Get qPtr in rsi
    mov qword [qPtr], rsi ;Save whatever pointer here (workingDD OR workingDPB)
    mov eax, dword [rdi + sft.dCurntOff]    ;Get current offset into file
    mov dword [currByteF], eax  ;Save Current byte in the file
    mov dword [tfrLen], ecx ;Save the number of bytes to transfer
    mov dword [tfrCntr], ecx    ;Save the bytes left to transfer
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
    mov edx, eax    ;Save file relative sector in edx
    and al, byte [rbp + dpb.bMaxSectorInCluster]   ;Works with max 64k clusters
    mov byte [currSectC], al    ;Save sector in cluster value in var
    mov eax, ecx    ;Save bytes to tfr in eax
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shr edx, cl ;Convert file relative sector to file relative cluster
    mov dword [currClustF], edx ;Save in var
    mov ecx, eax    ;Return the bytes to tfr in eax
    clc
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
    retc    ;Return if carry
    push rax
    movzx eax, word [machineNum]
    cmp ax, word [rdi + sft.wMachNum]
    pop rax
    rete
    mov al, errBadHdl   ;Error code
    stc         ;Reset CF
    return

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

getBytesTransferred:
    mov ecx, dword [tfrCntr]   ;Get bytes left to transfer
    neg ecx ;Multiply by -1
    add ecx, dword [tfrLen]     ;Add total bytes to transfer
    ret ;Return bytes transferred in ecx

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