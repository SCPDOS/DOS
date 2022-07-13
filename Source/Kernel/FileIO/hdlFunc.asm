;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
    return 
openFileHdl:       ;ah = 3Dh, handle function
    return 
closeFileHdl:      ;ah = 3Eh, handle function
    return 
readFileHdl:       ;ah = 3Fh, handle function
    lea rsi, readBytes
.common:
    call getSFTPtr  ;Get SFT ptr in rdi (if file is r/w-able from machine)
    jc extErrExit   ;Error code in al and exit
    call setCurrentSFT  ;Set the current SFT (from rdi)
    or ecx, ecx ;Clear upper bits of RCX if they are NOT clear just in case
    push qword [currentDTA] ;Save the current Disk Transfer Area
    mov qword [currentDTA], rdx ;Set the user buffer as the currentDTA
    call rsi    ;Get back in ecx the bytes transferred!
    pop qword [currentDTA]
    jc extErrExit   ;Error code in al and exit
    call getUserRegs
    mov dword [rsi + callerFrame.rax], ecx  ;Put actual number of bytes tfrd
    and byte [rsi + callerFrame.flags], 0FEh    ;Clear CF
    return 
writeFileHdl:      ;ah = 40h, handle function
    lea rsi, writeBytes
    jmp readFileHdl.common
deleteFileHdl:     ;ah = 41h, handle function, delete from specified dir
    return 
lseekHdl:          ;ah = 42h, handle function, LSEEK
;New pointer passed in edx! ecx will be DOCUMENTED as having to be 0
    call getSFTPtr
    jnc .sftValid
    ;al (eax) has error code for bad file handle
    jmp extErrExit ;Error code and exit
.sftValid:
    cmp al, 3
    jb .validFunction
    mov eax, errInvFnc       ;Error code and exit
    jmp extErrExit
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
.seekExit:
    xor al, al  ;Return OK!
    return 
.seekend:
;Here we are at seekend, seek from end (signed)
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jnz .netCheck
.proceedDisk:
    add edx, dword [rdi + sft.dFileSize]    ;Add to file size
    jmp short .seekset
.netCheck:
    test word [rdi + sft.wOpenMode], FCBopenedFile  ;Is this a FCB opened file?
    jnz .proceedDisk
    movzx eax, word [rdi + sft.wOpenMode]   ;Get the open mode
    ;Check it's share mode
    and eax, 0F0h    ;Isolate share bits
    cmp eax, denyNoneShare  ;Don't deny? Proceed
    je .netSeek
    cmp eax, denyReadShare
    jne .proceedDisk
.netSeek:
    mov eax, 1121h  ;Make net seek from end request
    int 4fh
    jnc .seekExit ;If the request returns with CF clear, there was no error
    jmp extErrExit
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
    return 
;-----------------------------------:
;       Main File IO Routines       :
;-----------------------------------:
readBytes:
;Reads the bytes into the user buffer for the setup SFT (currentSFT)
;Input: ecx = Number of bytes to read
;Output: ecx = Number of bytes read
;Number of bytes read 
;If CF=CY, return with error code in ax
    call getCurrentSFT  ;Get current SFT in rdi
    movzx eax, word [rdi + sft.wOpenMode]
    and al, 0Fh ;Eliminate except access mode
    cmp al, WriteAccess
    jne .readable
    mov eax, errAccDen
    xor ecx, ecx    ;Zero chars tfrred
    stc
    return
.readable:
    call setupVarsForTransfer
    jecxz .exitOk  ;If ecx = 0 (number of bytes to transfer = 0), exit
    test word [rdi + sft.wDeviceInfo], devRedirDev
    jz .notRedir
    mov eax, 1108h  ;Call Redir Read Bytes function
    int 4fh ;Call redir (tfr buffer in DTA var, ecx has bytes to tfr)
    return 
.exitOk:
    clc
    return 
.notRedir:
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz readCharDev
    call dosCrit1Enter
    call readDiskFile
    call dosCrit1Exit
    return 
readCharDev:
;rdi points to sft for char dev to read
;ecx has the number of bytes to transfer
;Vars have been set up and DTA has the transfer address
;Returns in ecx, the actual bytes transferred
;If CF=CY, return with error code in ax
    mov byte [errorLocus], eLocChr  ;Error is with a char device operation
    mov bx, word [rdi + sft.wDeviceInfo]    ;Get dev info
    mov rdi, qword [currentDTA] ;Get the DTA for this transfer in rdi
    test bl, charDevNoEOF   ;Does our device NOT generate EOF's on reads?
    jz rwExitOk    ;If it does, jump to exit as if EOF has been hit
    test bl, charDevNulDev  ;Is our device the NUL device?
    jz .notNul
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
    call vConSwapDriver    ;Prepare CON Useage!
    ;Get current offset into buffer (if one exists)
    mov rsi, qword [vConHdlOff]
    test rsi, rsi   ;Any chars in the buffer?
    jnz .tfrBuf ;If so, we want to keep tfring those chars to user DTA
    cmp byte [vConInBuf], 80h ;Is this buffer full?
    je .oldBuf  ;If so, we set up the buffer function to allow editing of buffer
    ;Else, reset the buffer
    mov word [vConInBuf], 0FF80h ;Byte 0=>length of buf, byte 1 => chars in buf
.oldBuf:
;Preserve the dta and number of chars to tfr
    push rcx
    push rdi
    lea rdx, vConInBuf
    call buffCharInput_BE   ;Get con buffered input
    pop rdi
    pop rcx
    lea rsi, qword [vConInBuf + 2]  ;Get the address of the data area of buffer
    cmp byte [rsi], EOF
    jne .tfrBuf ;If not equal, start copying over the buffer to the user DTA
    mov byte [rdi], EOF ;Store EOF at start of user DTA
    mov al, LF
    call charOut_B.in   ;Echo CRLF
    xor esi, esi    ;Set ZF = ZE
    jmp short .exit
.tfrBuf:
    lodsb   ;Get the char across from rsi to rdi with a copy in al
    stosb
    cmp al, CR 
    jne .noCRLF
    mov byte [rsi], LF  ;Store an LF in source to go one more time around
.noCRLF:
    cmp al, LF  ;Compare if al is LF
    loopne .tfrBuf  ;Copy the LF over if so and exit and dec ecx one more time
    jne .exit   ;If the reason for exiting loop was ecx = 0, skip the following
    ;This only applies if the reason for exiting the loop is al=LF
    call charOut_B.in   ;Echo CRLF
    xor esi, esi
    or al, 1    ;Set ZF = NZ
.exit:
    call vConRetDriver
    mov qword [vConHdlOff], rsi ;Store the offset (or 0 value)
    jmp rwExitOk    ;Exit ok! ecx has # chars tfred and ZF=ZE if @ EOF

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
    mov ah, critCharDev | critData ;Char device, data error signature
    call charDevErr   ;ah = has part of the error 
    ;al now has the response
    cmp al, critIgnore
    je .binNoError ;Simply proceed as normal
    mov rdi, rdx    ;Get back the buffer if it is a retry operation
    cmp al, critFail
    jne .binary ;If not fail, re-try the operation (ecx isn't touched)
    ;Fallthrough here for fail!
.failExit:
    mov rdi, qword [currentSFT]
    xor ecx, ecx
    mov eax, errAccDen
    stc ;Set carry flag to get caught as a error by caller
    return
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
    call checkBreak ;Check we don't have a ^C pending on CON
    call goDriver   ;If no ^C found (which exits DOS) Make request!
    movzx edi, word [primReqHdr + ioReqPkt.status] ;Get status word in di
    test edi, drvErrStatus  ;Did an error occur?
    jz .asciiNoError
    call charDevErr    ;Call Int 44h
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
    xor ecx, ecx
    stc
    ret
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
    return 
rwExitBad:
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:

setCurrentSFT:
;Set the pointer in rdi as current SFT 
    mov qword [currentSFT], rdi
    return 
getCurrentSFT:
;Get the current SFT pointer in rdi
    mov rdi, qword [currentSFT]
    return 
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
    return 
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
    return ;Else just exit here
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
    return 

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
    return
.thisTable:
    push rax
    push rdx
    mov eax, sft_size
    mul ebx
    add rdi, rax    ;Shift rdi to go to SFT entry in current table
    pop rdx
    pop rax
    add rdi, sfth_size  ;Go past the header
    return
getJFTPtr:    ;Int 4Fh AX=1220h
;Return a zero extended value in rdi for the SFT entry
;Input: bx = JFT handle (we zero extend)
;Output: CF=NC => rdi = Points to an SFT ndx or -1 => free space
;        CF=CY => al = Error code, Fail
    movzx ebx, bx   ;Ensure we zero extended
    cmp bx, word [maxHndls] ;0-19 acceptable ONLY!
    jb .ok
    mov al, errBadHdl
    stc
    return
.ok:
    mov rdi, qword [currentPSP]
    lea rdi, qword [rdi + psp.jobFileTbl + rbx] ;Use rbx as index in tbl
    clc
    return

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
;Input: bx = File handle (gets zero extended)
;Output: CF=NC: rdi = SFT pointer
;        CF=CY: Error, ax=Error code
    call getJFTPtr    ;Get the ptr to the value in rdi
    jb .fail
    cmp byte [rdi], -1  ;Is this JFT entry unassigned?
    jne .ok
.fail:
    mov al, errBadHdl
    stc
    return
.ok:
    push rbx    ;Preserve the JFT handle
    movzx ebx, byte [rdi]  ;Get byte entry into rbx
    call getSFTPtrfromSFTNdx    ;Get SFT pointer in rdi
    pop rbx 
    return

getBytesTransferred:
    mov ecx, dword [tfrCntr]   ;Get bytes left to transfer
    neg ecx ;Multiply by -1
    add ecx, dword [tfrLen]     ;Add total bytes to transfer
    return ;Return bytes transferred in ecx

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
    return