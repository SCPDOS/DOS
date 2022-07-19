;-----------------------------------:
;    File Handle Kernel routines    :
;-----------------------------------:

createFileHdl:     ;ah = 3Ch, handle function
    return 
openFileHdl:       ;ah = 3Dh, handle function
    return 
closeFileHdl:      ;ah = 3Eh, handle function
;Input: bx = file handle to close
    call getSFTPtr  ;Get a pointer to the SFT in rdi
    jc extErrExit   ;If CF is set, al has error code, exit!
    call setCurrentSFT  ;Set this as the current SFT
    ;Check count to see if we need to check share mode
    cmp word [rdi], 1   ;Opened once only, not shared
    je .skipNetCheck
    ;Now check sharing mode
    mov ax, word [rdi + sft.wOpenMode]  ;Get the share mode bits
    and al, 0F0h    ;And wipe out the other bits
    cmp al, denyRWShare | denyWriteShare | denyReadShare | denyNoneShare
    pushfq  ;Save the result of this for after closing the file
.skipNetCheck:
    call closeMain  ;Call close main!
    jc extErrExit   ;If an error, exit through error exit
    popfq
    je .exitOk  ;If sharing mode was net FCB, it had no JFT entry, skip nulling
    call getJFTPtr  ;Get the pointer to the JFT entry in rdi
    mov byte [rdi], -1  ;Free JFT entry
.exitOk:
    call getUserRegs
    and byte [rsi + callerFrame.flags], ~1  ;Clear CF
    xor al, al
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
    jc extErrExit ;al (eax) has error code for bad file handle
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
;Input: bx = Handle to duplicate
;Output: If ok then ax = New handle
    call findFreeJFT    ;First find a free space in the JFT
    jc extErrExit   ;Exit if no space
    ;rsi points to the free space
.duplicateCommon:
    call getJFTPtr  ;Get a pointer to the JFT entry in rdi for bx
    xchg rsi, rdi
    lodsb   ;Move over the SFT ndx from the old to the new position
    stosb
    dec rsi
    dec rdi
    ;rdi now points to new position
    ;rsi points to old position
    ;al has SFT ndx
    mov rsi, rdi    ;Move rsi to point to the new position jft position
    movzx ebx, al   ;Move SFTndx into ebx
    call getSFTPtrfromSFTNdx    ;Get the pointer to the SFT in rdi
    inc word [rdi + sft.wNumHandles]    ;Increase the number of handles in SFT
    ;Now we must return in ax the entry in the JFT 
    mov rdi, qword [currentPSP]
    lea rdi, qword [rdi + psp.jobFileTbl]   ;Point to head of table
    sub rsi, rdi    ;Get the difference of the two in si
    mov eax, esi
    call getUserRegs
    mov word [rsi + callerFrame.rax], ax
    and byte [rsi + callerFrame.flags], ~1  ;Clear CF
    return
forceDuplicateHdl: ;ah = 46h, handle function
;Input: bx = Handle to duplicate
;       cx = Handle to close and replace with a duplicate of bx
    ;First we close cx
    xchg ebx, ecx ;Swap cx and bx
    push rbx
    push rcx
    call closeFileHdl   ;Close handle 
    pop rcx
    pop rbx
    retc    ;The error code is set by errExtExit and CF is set on callerFrame
    ;Else, close was ok, lets duplicate now
    call getJFTPtr  ;Get a pointer to bx in rdi, destination for copy
    jc extErrExit   ;Return bad with error code in al
    xchg ebx, ecx   ;Now get source to duplicate in ebx
    mov rsi, rdi    ;Put the free space ptr in rsi
    jmp short duplicateHandle.duplicateCommon
findFirstFileHdl:  ;ah = 4Eh, handle function, Find First Matching File
    
findNextFileHdl:   ;ah = 4Fh, handle function, Find Next Matching File
renameFile:        ;ah = 56h
createUniqueFile:  ;ah = 5Ah, attempts to make a file with a unique filename
createNewFile:     ;ah = 5Bh

lockUnlockFile:    ;ah = 5Ch
    jmp extErrExit
setHandleCount:    ;ah = 67h
    jmp extErrExit
commitFile:        ;ah = 68h, flushes buffers for handle to disk 
    ;Input: bx = File to flush
    call getSFTPtr  ;Get sft pointer in rdi
    jc extErrExit
    call setCurrentSFT
    ;Now we check if the device is a char, disk or net file
    movzx eax, word [rdi + sft.wDeviceInfo]
    test ax, devRedirDev
    jnz .notNet
    ;Commit file net redir call and exit
    mov eax, 1107h
    int 4fh
    jc extErrExit
    jmp .exitOk
.notNet:
    test ax, devCharDev
    jz .blokDev
    ;Here we simply update date/time fields in the SFT structure before exiting
    call readDateTimeRecord ;Update DOS internal Time/Date variables
    jc extErrExit  ;If we fail to get time/date, fail the request
    ;Build date and time words
    call getDirDTwords  ;Get date time words packed in eax
    mov dword [rdi + sft.wTime], eax    ;Store them at the same time
    jmp .exitOk
.blokDev:
    mov rbp, qword [rdi +sft.qPtr]  ;Get DPB pointer in rbp
    call setWorkingDPB
    call updateDirectoryEntryForFile    ;Update the directory entry
    jc extErrExit
    call flushFile  ;Now the file gets flushed
    jc extErrExit
.exitOk:
    xor al, al
    call getUserRegs
    and byte [rsi + callerFrame], ~1    ;Clear CF
    return
;-----------------------------------:
;       Main File IO Routines       :
;-----------------------------------:
closeMain: ;Int 4Fh AX=1201h
;Gets the directory entry for a file
;Input: qword [currentSFT] = SFT to operate on (for FCB ops, use the SDA SFT)
;If CF=CY on return: Error, return error with al = error code
;Preserve all regs except eax and rdi
; If CF=NC on return: eax = Unknown
;                     rdi = current SFT ptr
    mov rdi, qword [currentSFT] ;Get the sft pointer
    test word [rdi + sft.wDeviceInfo], devRedirDev ;Is this a network drive?
    jnz .physical
    ;Here we beep out the request to the network redirector (Int 4Fh AX=1106h)
    mov eax, 1106h  ;Make request
    int 4fh ;Beep!
    return  ;Returns with CF set or clear as appropriate
.physical:  
; We make a request to the dev dir to close the device
; If the device is disk, we then update the directory entry for the disk file
    call dosCrit1Enter  ;Enter critical section 1
    push rbx
    push rsi
    mov rsi, qword [rdi + sft.qPtr] ;Get driver or DPB ptr in rsi
    test word [rdi + sft.wDeviceInfo], devCharDev
    jnz .charClose   ;Char devs aren't affected by directory work
    ;rsi has DPB pointer here
    ;rdi has the SFT pointer
    push rbp
    mov rbp, rsi ;Move the dpb pointer into rbp
    call setWorkingDPB  ;Set the working dpb to rbp
    call updateDirectoryEntryForFile
    pop rbp
    ;If CF is set, Fail was requested and ax has an error code
    jc .exit
    call flushFile
    jc .exit    ;If something went wrong, exit
    movzx ecx, byte [rsi + dpb.bUnitNumber]    ;Get the unit number in cl
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get driver ptr
.charClose:
    ;Now rsi = Device Driver Header and rdi = Current SFT header
    ;We now decrement handle count in SFT structure
    call decrementOpenCount ;rdi = current SFT, returns ax = old handle count
    dec ax  ;If this is zero, then we need to set wNumHandles to zero
    jnz .driverClose
    inc word [rdi + sft.wNumHandles]    ;Now make it zero again as it is -1
.driverClose:
    xchg ecx, eax ;Now store this because DOS returns in cx (according to RBIL)
    ;and if the device is a disk device, cl will have the unit number
    ;We first check if the driver supports oper/close requests
    test word [rsi + drvHdr.attrib], devDrvHdlCTL   ;Support Close?
    jnz .exit  ;If not, immediately jump to exit, all is well
    ;rsi has device driver ptr for device, make request
    call primReqCloseSetup  ;rbx gets header ptr, rsi has driver ptr
    call goDriver   ;Make request
    ;Don't check the status here, as we are simply informing the driver 
    ; of an operation. Nothing should be able to go wrong. 
    ;Functionally, an ignore if anything does go wrong.
.exit:
    pop rsi
    pop rbx
    call dosCrit1Exit
    return

decrementOpenCount: ;Int 4Fh AX = 1208h
;Input: rdi = SFT pointer
;Output: ax = Original wNumHandles count
    pushfq
    movzx eax, word [rdi + sft.wNumHandles]
    dec eax     ;Decrement count
    jnz .exit                           ;If the count is not zero, exit
    dec eax    ;If it is zero, now we make it -1
.exit:
    popfq
    xchg ax, word [rdi + sft.wNumHandles] ;RBIL says ax returns og num hdls
    return


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
    call readDiskFile   ;Called with rbp = Working DPB and rdi = CurrentSFT
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
;rdi = Current SFT
;rbp = WorkingDPB
    mov byte [errorLocus], eLocDsk  ;Error is with a disk device operation
    mov byte [rwFlag], 0    ;Read operation
    ;We have the following vars setup:
    ;tfrLen, tfrCntr, qPtr, workingDPB, workingDrv, currByteF/S, currSectF/C, 
    ;currClustF
    ;Now convert currSectC to disk sector by using currClustF
    ;Using currClustF as a counter, we walk the fat from startingCluster
    mov edx, currClustF ;Use edx as teh counter reg
    mov eax, dword [rsi + sft.dStartClust]  ;Get starting cluster
    xor ebx, ebx    ;Use ebx to contain the old cluster number
    mov ecx, dword [tfrLen] ;Get the tfrlen if we are past the end of the file
.goToCurrentCluster:
    mov ebx, eax    ;Save current cluster
    call walkFAT    ;Get in eax the next cluster
    jc .badExit   ;This can only return Fail
    cmp eax, -1 ;Are we past the end of the file?
    je rwExitOk ;Exit with no bytes transferred (and dont wanna flush anything)
    dec edx ;Decrement counter
    jnz .goToCurrentCluster
;Now we fall out with ebx = Current cluster
    mov eax, ebx    ;Get the current cluster in eax
    call getStartSectorOfCluster    ;Get the start sector on the disk in rax
    ;Now we add the offset to this
    movzx ebx, byte [currSectC] ;Get the sector offset into the cluster
    add rax, rbx    ;And finally get the absolute cluster on the disk
    mov qword [currSectD], rax  ;Save the current Sector on Disk in var
;Main
.mainRead:
    call getBufForData  ;Get bufHdr ptr in rbx and currBuf var for sector in rax
    jc .badExit
    mov rsi, rbx    ;Move the buffer pointer into rsi
    movzx ebx, word [currByteS] ;Get the byte offset into the current sector
    add rsi, rbx    ;Shift rsi by that amount into the sector
    ;Now we read the smallest of the following from the sector buffer:
    ; 1) Sector size, 2) Bytes left in File, 3) Bytes left to read from Request
    mov ecx, dword [rdi + sft.dFileSize]
    sub ecx, dword [rdi + sft.dCurntOff] ;Get bytes left to read in file in ecx
    mov ebx, dword [tfrCntr]
    cmp ecx, ebx    ;Is bytes left to read in file > bytes user has left?
    cmova ecx, ebx  ;Move ebx into ecx if so
    movzx ebx, word [rbp + dpb.wBytesPerSector]  ;Compare to sector size
    cmp ecx, ebx  ;ecx > sector size?
    cmova ecx, ebx  ;Move it into ecx if so
    push rdi
    mov rdi, qword [currentDTA]
    call readWriteBytesBinary
    mov qword [currentDTA], rdi ;rdi has been shifted by ecx on entry amount
    pop rdi
    mov ecx, dword [tfrCntr]   ;Get number of bytes left to transfer in ecx
    test ecx, ecx  ;Are we at the end yet?
    jz rwExitOk ;Exit if so!
    call getNextSectorOfFile    ;Get the next sector of the file
    jc .badExit
    jz rwExitOk ;ecx has the number of bytes left to transfer. ZF=ZE => EOF
    ;Else repeat
    ;currSectD has been updated, we now set currByteS = 0
    mov word [currByteS], 0 ;We start reading now from the start of the sector
    mov rax, qword [currSectD]  ;Get the next sector to read from
    jmp short .mainRead
.badExit:
    ;When a disk error occurs within the bit where vars have changed,
    ; we need to update the SFT before returning
    mov ecx, dword [tfrCntr]    ;Get the bytes left to transfer
    xor al, al  ;Set ZF flag
    call rwExitOk   ;We call this
    stc ;All calls which end up here return Fail!
    ret
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
    and byte [rdi + sft.wDeviceInfo], ~(blokFileToFlush|charDevNoEOF) ;OR
    ;Next char dev read should give EOF.
.skipbitClear:  ;Or skip that entirely
    call updateCurrentSFT   ;Return with CF=NC and ecx=Bytes transferred
    return 
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
    mov rbp, qword [rdi + sft.qPtr] ;Get DPB ptr in rbp
    mov qword [workingDPB], rbp
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
;Input: rbx = Valid SFT ndx number (byte, zero extended)
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

findFreeJFT:
;Input: [currentPSP] = Task whose PSP we will look through
;If there are no free spaces, then we return with al = errNhl and CF=CY
;Else, a pointer to the free space in rsi and al = -1
    push rcx
    mov rsi, qword [currentPSP]
    movzx ecx, word [maxHndls]
    lea rsi, qword [rsi + psp.jobFileTbl]   ;Point to start of table
.search:
    lodsb
    cmp al, -1
    je .exit
    dec ecx
    jnz .search
    mov al, errNhl  ;No free handles buddy, sorry
    stc ;Set error bit
.exit:
    pop rcx
    return