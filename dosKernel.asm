;-----------------------------------:
;       Misc System routines        :
;-----------------------------------:
criticalDOSError:
;Will swap stacks and enter int 44h safely and handle passing the right data 
; to the critical error handler.
; Called with ax, di and rsi set as required by Int 44h (caller decides)
;               AH = Critical Error Bitfield
;               Bit 7 = 0 - Disk Error, Bit 7 = 1 - Char Device Error
;               Bit 6 - Reserved
;               Bit 5 = 0 - IGNORE not allowed, Bit 5 = 1 - IGNORE allowed
;               Bit 4 = 0 - RETRY not allowed, Bit 4 = 1 - RETRY allowed
;               Bit 3 = 0 - FAIL not allowed, Bit 3 = 1 - FAIL allowed
;               Bits [2-1] = Affected Disk Error
;                     0 0   DOS area
;                     0 1   FAT area
;                     1 0   Directory area
;                     1 1   Data area
;               Bit 0 = 0 - Read Operation, Bit 0 = 1 - Write Operation
;               AL  = Failing drive number if AH[7] = 0
;               DIL = Error code for errorMsg
;               RSI = EA of Device Header for which device the error occured
;Return:
;               AL = 0 - Ignore the Error       (Ignore)
;                  = 1 - Retry the Operation    (Retry)
;                  = 2 - Terminate the Program  (Abort)
;                  = 3 - Fail the DOS call      (Fail)
; Return response from int 44h in al
    cli ;Disable Interrupts
    mov byte [critErrFlag], 1   ;Set flag for critical error
    mov qword [xInt44hRSP], rsp
    mov rsp, qword [oldRSP] ;Get the old RSP value
    int 44h ;Call critical error handler
    mov rsp, qword [xInt44hRSP] ;Return to the stack of the function that failed
    mov byte [critErrFlag], 0   ;Clear critical error flag
    sti ;Reenable Interrupts
    ret
findLRUBuffer: 
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

findDirtyBufferForDrive:
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

findSectorInBuffer:
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
findDPB:
;Finds the DPB for a given drive
;Input:  dl = Drive number (0=A, 1=B etc...)
;Output: al = 00, rbp = Pointer to the DPB
;        al = -1, Failed, no DPB for device, rbx destroyed
    mov rbx, qword [dpbHeadPtr]
.fd1:
    xor al, al
    cmp byte [rbp + dpb.bDriveNumber], dl
    je .fd2
    mov rbp, qword [rbp + dpb.qNextDPBPtr]
    mov al, -1
    cmp rbp, -1 ;If rbx followed last item in list, no DPB exists for dl
    jne .fd1
.fd2:
    ret
;-----------------------------------:
;        File Handle routines       :
;-----------------------------------:
readBinaryByteFromFile:
;Reads a byte from a SFT entry, does not translate it. 
;Read or RW permissions are checked at the INT 41h level
;Entry: rbx = SFT entry pointer
;       rdx = Address of the data buffer to read to
;       ecx = Number of bytes to read
;Exit: If CF = NC : All ok!
;       rbx = SFT entry pointer
;       al = 8 bit binary value read from device/file
;      If CF = CY : Error!
;       rbx = SFT entry pointer
;       al = Error code to ret if user returns fail from int 44h or no int 44h
;
; !!! Use the disk request header for all file handle IO !!!
;
    test word [rbx + sft.wDeviceInfo], devCharDev
    jnz .readBinaryByteFromCharDevice
.readBinaryByteFromHardFile:
;Disk files are accessed from here
;Use the sector buffers if the data is already buffered,
; else use the dpb to fill a sector buffer


.readBinaryByteFromCharDevice:
;Devices are accessed from here
    mov rbp, qword [rbx + sft.qPtr] ;Get device driver header pointer
    push rbx
    lea rbx, charReqHdr
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], drvREAD
    mov word [rbx + ioReqPkt.status], 0
    mov qword [rbx + ioReqPkt.bufptr], rdx
    mov dword [rbx + ioReqPkt.tfrlen], ecx

    call qword [rbp + drvHdr.strPtr]
    call qword [rbp + drvHdr.intPtr]
    mov eax, dword [rbx + ioReqPkt.tfrlen] ;Get number of bytes read
    test word [rbx + ioReqPkt.status], 8000h    ;Test the error bit is set
    pop rbx
    jz .readBinaryByteExitGood  ;Error bit not set, all good!
.readBinaryByteExitGood:
    ret
;-----------------------------------:
;        Interrupt routines         :
;-----------------------------------:
terminateProcess:   ;Int 40h
    iretq
terminateHandler:   ;Int 42h
ctrlCHandler:       ;Int 43h
    iretq
absDiskWrite:       ;Int 46h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    push rax
    push rbx
    push rdx
    push rbp
    mov ah, drvWRITE
    add ah, byte [verifyFlag]   ;Change to Write/Verify if set
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    push rax
    push rbx
    push rdx
    push rbp
    mov ah, drvREAD
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
    push rax    ;Save drive number, cmdcde and start LBA
    push rbx
    push rdx
    mov ah, 32h ;Get DPB
    mov dl, al
    int 41h
    mov rbp, rbx    ;Get dpb ptr in rbp
    pop rdx
    pop rbx
    pop rax

    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov byte [diskReqHdr + ioReqPkt.cmdcde], ah
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov qword [diskReqHdr + ioReqPkt.bufptr], rbx
    mov qword [diskReqHdr + ioReqPkt.strtsc], rdx
    mov dword [diskReqHdr + ioReqPkt.tfrlen], ecx
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr] ;Get driver pointer

    lea rbx, diskReqHdr
    call qword [rdx + drvHdr.strPtr]  ;Call with ptr to request block in rbx
    call qword [rdx + drvHdr.intPtr]
    pop rbp
    pop rdx
    pop rbx
    pop rax
    test word [diskReqHdr + ioReqPkt.status], 8000h
    je .absDiskError
    clc
    ret
.absDiskError:
    mov al, byte [diskReqHdr + ioReqPkt.status] ;Get low byte into al
    mov ah, 80h ;Attachment failure
    cmp al, 0Ch ;Gen error
    je .absExit
    mov ah, 40h ;Seek error
    cmp al, 06h
    je .absExit
    mov ah, 08h ;Bad CRC
    cmp al, 04h
    je .absExit
    mov ah, 04h ;Sector not found
    cmp al, 08h
    je .absExit
    xor ah, ah  ;Write Protect Violation
    test al, al
    je .absExit
    mov ah, 02h ;Other Error
.absExit:
    stc
    ret

terminateResident:  ;Int 47h
inDosHandler:       ;Int 48h
;Called when DOS idle
    iretq
fastOutput:         ;Int 49h
;Called with char to transfer in al
    push rax
    mov ah, 0Eh
    int 30h
    pop rax
    iretq
passCommand:        ;Int 4Eh, hooked by COMMAND.COM
    iretq
multiplex:          ;Int 4Fh, kept as iretq for now
    iretq
;-----------------------------------:
;        Main Kernel routines       :
;-----------------------------------:
functionDispatch:   ;Int 41h Main function dispatcher
;ah = Function number, all other registers have various meanings
    cli ;Halt external interrupts
    cld ;Ensure all string ops occur in the right direction
    cmp ah, kernelDispatchTableL/8    ;Number of functions
    ja .fdExitBad
    ;Cherry pick functions
    cmp ah, 33h ;CTRL+BREAK check
    jb .fsbegin   ;If below skip these checks
    je ctrlBreakCheck
    cmp ah, 64h
    je setDriverLookahead  ;Reserved, but avoids usual Int 41h spiel
    ja .fsbegin   ;If above, do usual Int41 entry
    cmp ah, 51h
    je getCurrProcessID    ;This and below are exactly the same
    cmp ah, 62h
    je getPSPaddr          ;Calls the above function
    cmp ah, 50h
    je setCurrProcessID
.fsbegin:
    pushDOS ;Push the usual prologue registers
    mov rax, qword [oldRSP]
    mov qword [oldoldRSP], rax
    inc byte [inDOS]    ;Increment in DOS flag
    mov qword [oldRSP], rsp
;Here, we want to save oldRSP in the callers PSP
    cmp byte [inDOS], 1 ;Check how many times we are in DOS
    jne .fsb1   ;If this is first entry, save rsp in callers PSP
    mov rax, qword [currentPSP] ;Get current PSP address
    mov qword [rax + psp.rspPtr], rsp    ;Save rsp on callers stack
.fsb1:
    pop rax     ;Get old rax back
    push rax    ;and push it back onto the stack
    lea rsp, critStakTop
    sti         ;Reenable interrupts

    mov byte [int48Flag], 1 ;Make it ok to trigger Int 48h

    mov qword [oldRBX], rbx ;Need to do this as I might switch stacks later
    movzx ebx, ah   ;Move the function number bl zero extended to rbx
    shl ebx, 3      ;Multiply the function number by 8 for offset into table
    push rax        ;Push rax onto the stack
    lea rax, qword [kernelDispatchTable]
    add rbx, rax    ;Add dispatch table offset into rbx
    pop rax
    mov rbx, qword [rbx]    ;Get the address from the dispatch table

    test ah, ah     ;Simple Terminate function?
    jz .fddiskOp
    cmp ah, 59h     ;Extended Error report?
    je .fdGoToFunction  ;Bypass code that clears the error report
    cmp ah, 0Ch     ;Are we a char function?
    ja .fddiskOp
;Char operations here
    test byte [critErrFlag], 1  ;Are we in critical error?
    jnz .fdGoToFunction         ;If we are, stay on Critical Error Stack
    lea rsp, IOStakTop          ;Otherwise, switch to IO stack
    jmp short .fdGoToFunction
.fddiskOp:
    ;Disk operations go here
    ;Clear up error info
    mov byte [errorLocus], 1    ;Reset to generic, unknown locus
    mov byte [critErrFlag], 0   ;Clear the Critical Error Flag
    mov byte [errorDrv], -1     ;Set the drive which caused the error to none

    mov byte [int48Flag], 0     ;Turn off the ability to trigger Int 48h
    lea rsp, DiskStakTop        ;Swap the stack to the Disk Transfer Stack
    test byte [breakFlag], -1   ;Test if set
    jz .fdGoToFunction
; HANDLE CTRL+BREAK HERE!
.fdGoToFunction:
    xchg rbx, qword [oldRBX]    ;Put the call addr in oldRBX and get oldRBX back
    ;Potentially point rbp to caller reg frame for easy access of registers 
    ;
    ;IF YOU USE RAX AND DONT NEED A RETURN VALUE IN AL, 
    ;ENSURE YOU READ AL FROM THE STACK FRAME BEFORE RETURNING TO PRESERVE AL!!!
    ;
    call qword [oldRBX]     ;Call the desired function, rax contains ret code
.fdExit:
    cli     ;Redisable interrupts
    ;???
    dec byte [inDOS]            ;Decrement the inDOS count
    mov rsp, qword [oldRSP]     ;Point rsp to old stack
    mov byte [rsp], al   ;Put the ret code into its pos on the register frame
    mov rax, qword [oldoldRSP]
    mov qword [oldRSP], rax
    popDOS  ;Pop the frame
    iretq
.fdExitBad:
    mov ah, 0
    iretq

;========================================:
;            Kernel Functions            :
;========================================:
simpleTerminate:   ;ah = 00h
    ret
diskReset:         ;ah = 0Dh
;Flush all dirty buffers to disk
    mov rbp, qword [bufHeadPtr]
.drCheckBuffer:
    test byte [rbp + bufferHdr.bufferFlags], dirtyBuffer
    jz .drGotoNextBuffer
.drFlushBuffer:
    call flushBuffer    ;Called with rbp = buffer header
    jc .drError
.drGotoNextBuffer:
    mov rbp, qword [rbp + bufferHdr.nextBufPtr]
    cmp rbp, -1     ;If rbp points to -1, exit
    jne .drCheckBuffer
    ret
.drError:
;Abort/Retry/Ignore
;Abort returns to DOS, 
;Retry retries the write on the buffer, 
;Ignore marks the buffer as clean and proceeds as normal
    mov al, byte [rbp + bufferHdr.bufferFlags]
    and al, 0Fh ;Clear the upper nybble
    mov ah, 31h ;Disk Error, Ignore,Retry and Write operation
    cmp al, dosBuffer
    je .drErrorMain
    add ah, 2
    cmp al, fatBuffer
    je .drErrorMain
    add ah, 2
    cmp al, dirBuffer
    add ah, 2
.drErrorMain:
    mov al, byte [rbp + bufferHdr.driveNumber]
    mov rsi, qword [rbp + bufferHdr.driveDPBPtr]
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr]
    mov di, word [diskReqHdr + drvReqHdr.status]    ;Disk error occured!
    and di, 0FFh    ;Only bottom byte
    mov word [errorExt], di     ;Save driver error code
    add word [errorExt], 13h    ;Add offset to driver error codes
    mov byte [errorDrv], al     ;Save the drive on which the error occured
    mov byte [errorLocus], 2    ;Error in Block Device Request code
    mov byte [errorClass], 11   ;Media error occured (bad BPB or other) code
    mov byte [errorAction], 1   ;Retry request code
    call criticalDOSError       ;Critical error handler
    test al, al ;Ignore the troublesome buffer and mark it as free
    jz .drIgnore
    cmp al, 1   ;Retry flushing the buffer
    je .drFlushBuffer
    int 43h     ;Abort and fail both abort through int 43h
.drIgnore:
    mov byte [rbp + bufferHdr.driveNumber], -1  ;Mark buffer as free
    jmp .drGotoNextBuffer

selectDisk:        ;ah = 0Eh
;Called with dl = drive number, 0 = A, 1 = B etc...
    mov al, byte [numLRemDrives]    ;Value 1 based
    mov bl, byte [lastdrvNum]       ;Value 1 based
    dec al
    dec bl
    cmp bl, al
    cmova eax, ebx    ;If bl > al, move bl to al
    cmp dl, al  ;If dl is bigger than al
    ja .error
    mov byte [currentDrv], dl   ;Only save dl if it is a valid number
    ret ;al = lastdrv as retcode
.error:
    mov rbp, qword [oldRSP]
    or qword [rbp + callerFrame.flags], 1   ;Set the CY flag
    mov eax, 15                 ;Invalid drive error
    mov word [errorExt], ax     
    mov byte [errorLocus], 1    ;Not appropriate
    mov byte [errorClass], 8    ;Drive not found
    mov byte [errorAction], 7   ;Retry after user intervention
    ret
getCurrentDisk:    ;ah = 19h, get current default drive
    mov al, byte [currentDrv]
    ret
FATinfoDefault:    ;ah = 1Bh
    xor dl, dl
FATinfoDevice:     ;ah = 1Ch
;Return in:
;   al = Number of sectors per cluster
;   edx = Number of clusters
;   cx =  Size of a clsuter
    test dl, dl
    jz .fidSkipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    jmp short .fidMain
.fidSkipdefault:
    dec dl ;Decrement the drive letter since 0 = Default, 1 = A etc...
.fidMain:
;Walk the dpb chain manually
    call findDPB    ;Get in rbp the dpb pointer for drive dl
    test al, al
    jz .fidDPBFound
;Else, we at an error.
;Simply return with CY set and error code in al with extended error info
    mov rbp, qword [oldRSP]
    or qword [rbp + callerFrame.flags], 1   ;Set the CY flag
    mov eax, 15                 ;Invalid drive error
    mov word [errorExt], ax     
    mov byte [errorLocus], 1    ;Not appropriate
    mov byte [errorClass], 8    ;Drive not found
    mov byte [errorAction], 7   ;Retry after user intervention
    ret
.fidDPBFound:
    mov al, byte [rbp + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov edx, dword [rbp + dpb.dClusterCount]
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    mov ebx, 1
    shl ebx, cl
    mov ecx, ebx    ;Save the value in ecx
    lea rbx, qword [rbp + dpb.bMediaDescriptor]
    mov rbp, qword [oldRSP]
    mov qword [rbp + callerFrame.rdx], rdx
    mov word [rbp + callerFrame.rcx], cx
    mov qword [rbp + callerFrame.rbx], rbx
    ret
;===============================
setIntVector:      ;ah = 25h
;Called with:
;   rdx = Pointer to interrupt handler
;   al = Interrupt number
    mov ebp, eax ;al has interrupt number which we need to save
    and ebp, 0FFh   ;Zero everything but the bottom byte
;First call to get default BIOS segement selector and attribute word
    mov bl, al  ;Set interrupt number 
    mov eax, 0F007h ;Get the descriptor
    int 35h
    mov esi, eax    ;Move segment selector info to esi
    mov ecx, ebp    ;Get the interrupt number into cl
;dx preserves the attribute word
    mov rbp, qword [oldRSP]
    mov rbx, qword [rbp + callerFrame.rdx]  ;Pointer passed in rdx
    mov eax, 0F008h ;Set descriptor
    int 35h

    mov al, byte [rbp + callerFrame.rax]    ;Preserve low byte of rax
    ret
createNewPSP:      ;ah = 26h
    ret
setResetVerify:    ;ah = 2Eh, turns ALL writes to write + verify
    mov byte [verifyFlag], al
    and byte [verifyFlag], 1       ;Only save the bottom bit
    ret
getDOSversion:     ;ah = 30h
    mov rdx, qword [oldRSP]
    xor ah, ah ;Continue the mainline PC-DOS identification line
    mov byte [rdx + callerFrame.rbx + 1], ah    ;Clear bh 
    mov ax, word [dosMajor] ;Major and minor version in al,ah resp.
    mov word [rdx + callerFrame.rax], ax    ;Save ax
    ret
terminateStayRes:  ;ah = 31h
    ret
;-------------------------------------------
getCurrentDPBptr:  ;ah = 1Fh, simply calls int 41h ah = 32h with dl = 0
    xor dl, dl
getDeviceDPBptr:   ;ah = 32h
;On entry: dl = Drive number
;On exit: rbx = DPB pointer
    test dl, dl
    jnz .gddpskipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    jmp short .gddpcommon
.gddpskipdefault:
    ;Decrement the drive letter since 0 = Default, 1 = A etc...
    dec dl
.gddpcommon:
    call findDPB ;Get in rbp the dpb pointer for drive dl
    test al, al
    jz .gddpMediaCheck
;Put in here error info
    mov word [errorExt], 15 ;Invalid drive spec
    mov byte [errorLocus], 2    ;Block device driver
    mov byte [errorClass], 8    ;Drive not found
    mov byte [errorAction], 7   ;Retry after intervention
    ret ;Return. al = -1
.gddpMediaCheck:
;Media Check Section
    mov byte [diskReqHdr + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov byte [diskReqHdr + mediaCheckReqPkt.unitnm], dl
    mov byte [diskReqHdr + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [diskReqHdr + mediaCheckReqPkt.status], 0
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + mediaCheckReqPkt.medesc], al
    mov al, dl  ;Save device number in al
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr]
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    call [rdx + drvHdr.strPtr]
    call [rdx + drvHdr.intPtr]
    test word [diskReqHdr + mediaCheckReqPkt.status], 8000h
    jnz .gddpError
    mov dl, al
    cmp byte [diskReqHdr + mediaCheckReqPkt.medret], 1 ;Certified no change
    je .gddpretdbp
    cmp byte [diskReqHdr + mediaCheckReqPkt.medret], 0
    jne .gddpBuildBPB   ;This means Media changed declared
    call findDirtyBufferForDrive
    test rbx, -1    ;This is the case if no dirty buffers for drive
    jne .gddpretdbp ;If there is a dirty buffer for the drive, dont build bpb
.gddpBuildBPB:
;BPB Build Section, only here if need a new bpb, i.e. sure of a new device
    call findLRUBuffer  ;Get lru buffer pointer in rbx
    cmp dl, byte [rbx + bufferHdr.driveNumber]  ;Does buffer belong to old drv?
    je .gddpBuildBPBInvalidateBuffer    ;Yes, immediately invalidate data
    ;If no, flush the data to disk.
.gddpBuildBPBFlush:
    mov rsi, rbp    ;Save rbp as pointer to old dl drive dpb
    mov rbp, rbx    ;Get buffer header pointer in rbp
    call flushBuffer    ;Flush the buffer to disk, rbx preserved
    mov rbp, rsi    ;Return old drive dpb pointer to rbp
    jc .gddpErrorType2  ;rbx points to buffer header
.gddpBuildBPBInvalidateBuffer:
    ;Write new buffer header
    mov byte [rbx + bufferHdr.driveNumber], dl
    mov byte [rbx + bufferHdr.bufferFlags], dataBuffer
    mov qword [rbx + bufferHdr.bufferLBA], 0
    mov byte [rbx + bufferHdr.bufFATcopy], 1
    mov dword [rbx + bufferHdr.bufFATsize], 0
    mov qword [rbx + bufferHdr.driveDPBPtr], rbp
    lea rbx, qword [rbx + bufferHdr.dataarea]
    ;Build BPB request
    mov byte [diskReqHdr + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov byte [diskReqHdr + bpbBuildReqPkt.unitnm], dl
    mov byte [diskReqHdr + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [diskReqHdr + bpbBuildReqPkt.status], 0 
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + bpbBuildReqPkt.medesc], al
    mov qword [diskReqHdr + bpbBuildReqPkt.bufptr], rbx ;Put lru pointer in rbx
    mov rdx, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    call [rdx + drvHdr.strPtr]
    call [rdx + drvHdr.intPtr]
    test word [diskReqHdr + bpbBuildReqPkt.status], 8000h
    jnz .gddpError
    mov rsi, qword [diskReqHdr + bpbBuildReqPkt.bpbptr]
    ;rbp points to dpb so we good to go
    ;Call int 41h ah=53h Build DPB without reentering Int 41h
    ;Since this function doesnt modify the caller stack, it is safe to do so
    call createDPB 
.gddpretdbp: 
    mov byte [rbp + dpb.bAccessFlag], -1    ;Clear access flag
    mov rdx, qword [oldRSP]
    mov qword [rdx + callerFrame.rbx], rbp  ;Here, all paths have rbp as dpbptr
    xor al, al  ;Set al = 0 to indicate rbx=dpb pointer
    ret
.gddpretdpbFail:
    mov rdx, qword [oldRSP]
    or qword [rdx + callerFrame.flags], 1   ;Set CF=CY
    mov word [errorExt], 83 ;Fail on INT 44h error code
    ret
.gddpError:
;Abort, Retry, Ignore are the only acceptible responses
;Entered with rbp = dpb for failing drive
;             rdx = driver header that caused fault
    mov rsi, rdx    ;rdx points to driver header in both cases
    mov rbx, qword [oldRSP]
    mov al, byte [rbx + callerFrame.rdx]    ;Get low byte = dl = Drive number
    mov dl, al  ;Save in dl
    test al, al
    jnz .gddpE0
    mov al, byte [currentDrv]
    jmp short .gddpE1
.gddpE0:
    dec al
.gddpE1:
    mov ah, 36h ;Read operation, data area, abort/retry/ignore, disk error
    mov di, word [diskReqHdr + drvReqHdr.status]   ;Get low byte of status
    and di, 0FFh    ;Save lo byte only
    mov word [errorExt], di     ;Save driver error code
    add word [errorExt], 13h    ;Add offset to driver error codes
    mov byte [errorDrv], al     ;Save the drive on which the error occured
    mov byte [errorLocus], 2    ;Error in Block Device Request code
    mov byte [errorClass], 11   ;Media error occured (bad BPB or other) code
    mov byte [errorAction], 1   ;Retry request code
    call criticalDOSError   ;Critical error handler
    test al, al
    jz .gddpretdbp  ;Ignore error, return, rbp has old dpb pointer
    cmp al, 1
    je getDeviceDPBptr ;Reenter the function, dl has drive code
    int 43h ;Else, restart DOS
.gddpErrorType2:
;Error flushing the old buffer
;   rbx = Buffer header for data transfer
    mov ah, 39h ;Write operation, abort/retry/ignore/fail, disk error
    cmp byte [rbx + bufferHdr.bufferFlags], dosBuffer
    je .gddpErrorType2main
    or ah, 2h   ;Set bit 1
    cmp byte [rbx + bufferHdr.bufferFlags], fatBuffer
    je .gddpErrorType2main
    mov ah, 3Dh ;Set bit 2, clear bit 1
    cmp byte [rbx + bufferHdr.bufferFlags], dirBuffer
    je .gddpErrorType2main
    or ah, 2h   ;Set bit 2 and 1
.gddpErrorType2main:    
    mov di, word [diskReqHdr + drvReqHdr.status]   ;Get low byte of status
    and di, 0FFh    ;Save lo byte only
    mov word [errorExt], di     ;Save driver error code
    add word [errorExt], 13h    ;Add offset to driver error codes
    mov al, byte [rbx + bufferHdr.driveNumber]
    mov byte [errorDrv], al
    mov byte [errorLocus], 2    ;Error in Block Device Request code
    mov byte [errorClass], 11   ;Media error occured (bad disk write) code
    mov byte [errorAction], 1   ;Retry request code
    mov rsi, qword [rbx + bufferHdr.driveDPBPtr]
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get device driver header in rsi
    call criticalDOSError   ;Critical error handler
    cmp byte [rbx + bufferHdr.bufferFlags], fatBuffer
    je .gddpErrorType2FatDir
    cmp byte [rbx + bufferHdr.bufferFlags], dirBuffer
    je .gddpErrorType2FatDir
    test al, al
    jz .gddpBuildBPBInvalidateBuffer ;Ignore error, invalidate data
    cmp al, 1
    je .gddpBuildBPBFlush   ;Retry flush, rbx has buffer pointer
    cmp al, 3
    je .gddpretdpbFail
    int 43h ;al = 2, means just abort
.gddpErrorType2FatDir:
    test al, al ;Ignore converted to fail
    jz .gddpretdpbFail
    cmp al, 1
    je .gddpBuildBPBFlush   ;Retry flush, rbx has buffer pointer
    cmp al, 3
    je .gddpretdpbFail
    int 43h ;al = 2, means just abort
;-------------------------------------------

ctrlBreakCheck:    ;ah = 33h
    test al, al
    jz .cbcget  ;Get the state
    mov byte [breakFlag], dl    ;Set the state
.cbcget:
    mov dl, byte [breakFlag]    ;Get the state
    ret
getInDOSflagPtr:   ;ah = 34h
    lea rdx, inDOS
    mov rbx, qword [oldRSP]
    mov qword [rbx + callerFrame.rbx], rdx  ;save ptr in rbx
    ret
getIntVector:      ;ah = 35h
;Called with:
;   al = Interrupt Number
;Returns:
;   rbx = Pointer to interrupt handler
    mov bl, al  ;Get the interrupt vector number into bl
    mov eax, 0F007h
    int 35h
    mov rdx, qword [oldRSP]
    mov qword [rdx + callerFrame.rbx], rbx  ;Save pointer in rbx
    mov al, byte [rdx + callerFrame.rax]    ;Get the low byte in al
    ret
getDiskFreeSpace:  ;ah = 36h
    test dl, dl
    jz .gdfsSkipdefault
    mov dl, byte [currentDrv]   ;Get current drive code, 0 = A, 1 = B etc...
    jmp short .gdfsMain
.gdfsSkipdefault:
    dec dl ;Decrement the drive letter since 0 = Default, 1 = A etc...
.gdfsMain:
    call findDPB ;Get in rbp the dpb pointer for drive dl
    test al, al
    jz .gdfsDPBFound
;Else, we at an error.
;Simply return with CY set and error code in al with extended error info
    mov word [errorExt], 15     ;Invalid drive error
    mov byte [errorLocus], 1    ;Not appropriate
    mov byte [errorClass], 8    ;Drive not found
    mov byte [errorAction], 7   ;Retry after user intervention
    mov rbp, qword [oldRSP]
    mov word [rbp + callerFrame.rax], -1    ;Set ax=FFFFh
    or qword [rbp + callerFrame.flags], 1   ;Set CF=CY
    ret
.gdfsDPBFound:
    mov al, byte [rbp + dpb.bMaxSectorInCluster]
    inc al  ;Since bMaxSectorInCluster is one less than the number of sec/clus
    mov edx, dword [rbp + dpb.dClusterCount]
    mov cl, byte [rbp + dpb.bBytesPerSectorShift]
    mov ebx, 1
    shl ebx, cl
    mov ecx, ebx    ;Save the value in ecx
    mov ebx, dword [rbp + dpb.dNumberOfFreeClusters]    ;Ger # free clusters
    mov rbp, qword [oldRSP]
    mov qword [rbp + callerFrame.rdx], rdx
    mov word [rbp + callerFrame.rcx], cx
    mov qword [rbp + callerFrame.rbx], rbx
    ret

loadExecChild:     ;ah = 4Bh, EXEC
terminateClean:    ;ah = 4Ch, EXIT
getRetCodeChild:   ;ah = 4Dh, WAIT, get ret code of subprocess
    ret
setCurrProcessID:  ;ah = 50h, set current process ID (Set current PSP)
    mov qword [currentPSP], rbx ;Set the pointer
    ret
getCurrProcessID:  ;ah = 51h, get current process ID (Get current PSP)
    mov rbx, qword [oldRSP]
    mov rdx, qword [currentPSP]
    mov qword [rbx + callerFrame.rbx], rdx   ;Set the caller pointer
    ret 
getSysVarsPtr:     ;ah = 52h
    lea rdx, sysVarsPtr
    mov rbx, qword [oldRSP]
    mov qword [rbx + callerFrame.rbx], rdx
    ret
createDPB:         ;ah = 53h, generates a DPB from a given BPB
;Only translates the data that can be garnered from a BPB to the DPB
;This is done so that the other fields of the DPB can be recycled
;Input: rsi = ptr to the BPB
;       rbp = ptr to the DPB
;bMediaDescriptor
    mov al, byte [rsi + bpb.media]
    mov byte [rbp + dpb.bMediaDescriptor], al
;bAccessFlag
    mov byte [rbp + dpb.bAccessFlag], -1    ;Not accessed
;dFirstFreeCluster
    mov dword [rbp + dpb.dFirstFreeCluster], 0  ;Start searching from start
;dNumberOfFreeClusters
    mov dword [rbp + dpb.dNumberOfFreeClusters], -1 ;Unknown
;bBytesPerSectorShift
    mov ax, word [rsi + bpb.bytsPerSec]
    mov cl, 7   ;Start with 128 byte sectors (not supported, min 512)
    shr ax, cl  ;Shift down
.cd0:
    shr ax, 1
    jz .cd1
    inc cl
    jmp short .cd0
.cd1:
    mov byte [rbp + dpb.bBytesPerSectorShift], cl
;bMaxSectorInCluster
    mov al, byte [rsi + bpb.secPerClus]
    dec al  ;Subtract one to get the max number of the last sector in a cluster
    mov byte [rbp + dpb.bMaxSectorInCluster], al
;bSectorsPerClusterShift
    inc al
    xor cl, cl
.cd2:
    shr al, 1
    jz .cd3
    inc cl
    jmp short .cd2
.cd3:
    mov byte [rbp + dpb.bSectorsPerClusterShift], cl
;dFAToffset, number of hidden sectors + number of reserved sectors
    movzx eax, word [rsi + bpb.revdSecCnt]
    add eax, dword [rsi + bpb.hiddSec] 
    mov dword [rbp + dpb.dFAToffset], eax
;bNumberOfFATs
    mov al, byte [rsi + bpb.numFATs]
    mov byte [rbp + dpb.bNumberOfFATs], al
;wNumberRootDirSectors
    movzx eax, word [rsi + bpb.rootEntCnt] ;Must be 0 on FAT 32
    shl eax, 5  ;Multiply by 32
    movzx ecx, word [rsi + bpb.bytsPerSec]
    dec ecx
    add eax, ecx
    xor edx, edx    ;Clear for divide
    div ecx ;Divide 0:eax by ecx, (e)ax has number of clusters
    mov word [rbp + dpb.wNumberRootDirSectors], ax  ;0 for FAT32
;dFATlength, get the FAT length
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax   ;If FATsz16 = 0, then use FATsz32
    cmovz eax, ebx  ;Only move 32bit value if sz16 was 0
    mov dword [rbp + dpb.dFATlength], eax
;Complex cases below...
;dClusterHeapOffset, start sector of the data area
    movzx eax, word [rsi + bpb.FATsz16]
    mov ebx, dword [rsi + bpb32.FATsz32]
    test eax, eax
    cmovz eax, ebx
;eax = FATsz
    movzx ebx, word [rsi + bpb.totSec16]
    mov ecx, dword [rsi + bpb.totSec32]
    test ebx, ebx
    cmovz ebx, ecx 
;ebx = TotSec
    mov cl, byte [rsi + bpb.numFATs]
    xor edx, edx    ;Use edx = NumFATs * FATsz temporarily
.cd4:
    add edx, eax
    dec cl
    jnz .cd4
    mov eax, edx    ;Store product in eax
    movzx edx, word [rsi + bpb.revdSecCnt]  ;Get reserved sectors in volume
    add eax, edx
    movzx edx, word [rbp + dpb.wNumberRootDirSectors]
    add eax, edx    ;This adds nothing if FAT32
    ;eax = BPB_ResvdSecCnt + (BPB_NumFATs * FATSz) + RootDirSectors
    ;aka eax = Start sector of the data area in volume
    mov dword [rbp + dpb.dClusterHeapOffset], eax
;dClusterCount
    sub ebx, eax    ;ebx = Number of sectors in the data area
    mov eax, ebx    ;Move number of sectors in data area into eax
    xor edx, edx
    mov ebx, 1
    mov cl, byte [rbp + dpb.bSectorsPerClusterShift]
    shl ebx, cl ;Get sectors per cluster
    div ebx ;Data area sector / sectors per cluster = cluster count
    inc eax ;Maximum valid cluster value is eax + 1
    mov dword [rbp + dpb.dClusterCount], eax    ;eax = Cluster count
;dFirstUnitOfRootDir
    cmp eax, fat16MaxClustCnt  ;If above, its FAT32
    mov eax, dword [rsi + bpb32.RootClus]   ;Just save this if FAT32
    ja .cd5
    ;Else, we need to find the first sector of the root directory
    ;Get the start sector of data area in volume 
    ; and sub the number of sectors in the root directory
    mov eax, dword [rbp + dpb.dClusterHeapOffset]
    movzx ebx, word [rbp + dpb.wNumberRootDirSectors]
    sub eax, ebx    ;eax now has start sector of root dir
.cd5:
    mov dword [rbp + dpb.dFirstUnitOfRootDir], eax
;Exit epilogue
    mov rbx, qword [oldRSP]
    mov al, byte [rbx + callerFrame.rax]        ;Return original al value 
    ret
getVerifySetting:  ;ah = 54h
    mov al, byte [verifyFlag]   ;al is the return value in this case
    ret
createPSP:         ;ah = 55h, creates a PSP for a program
getExtendedError:  ;ah = 59h
getCritErrorInfo:  ;ah = 5Dh
networkServices:   ;ah = 5Eh, do nothing
networkRedirection:;ah = 5Fh, do nothing
    ret
getPSPaddr:        ;ah = 62h, gives PSP addr/Process ID
    mov rbx, qword [oldRSP]
    mov rdx, qword [currentPSP]
    mov qword [rbx + callerFrame.rbx], rdx  ;Save the current psp in rbx
    ret
                    ;ah = 63h, reserved
setDriverLookahead:;ah = 64h, reserved
getsetDiskSerial:  ;ah = 69h, get/set disk serial number
return:
    ret

kernelDispatchTable:
    dq simpleTerminate     ;AH = 00H, PROCESS MANAGEMENT
    dq stdinReadEcho       ;AH = 01H, CHAR IO
    dq stdoutWrite         ;AH = 02H, CHAR IO
    dq stdauxRead          ;AH = 03H, CHAR IO
    dq stdauxWrite         ;AH = 04H, CHAR IO
    dq stdprnWrite         ;AH = 05H, CHAR IO
    dq directCONIO         ;AH = 06H, CHAR IO
    dq waitDirectInNoEcho  ;AH = 07H, CHAR IO
    dq waitStdinNoEcho     ;AH = 08H, CHAR IO
    dq printString         ;AH = 09H, CHAR IO
    dq buffStdinInput      ;AH = 0AH, CHAR IO
    dq checkStdinStatus    ;AH = 0BH, CHAR IO
    dq clearbuffDoFunc     ;AH = 0CH, CHAR IO
    dq diskReset           ;AH = 0DH, DISK MANAGEMENT
    dq selectDisk          ;AH = 0EH, DISK MANAGEMENT
    dq openFileFCB         ;AH = 0FH, FILE OPERATION       FCB
    dq closeFileFCB        ;AH = 10H, FILE OPERATION       FCB
    dq findFirstFileFCB    ;AH = 11H, FILE OPERATION       FCB
    dq findNextFileFCB     ;AH = 12H, FILE OPERATION       FCB
    dq deleteFileFCB       ;AH = 13H, FILE OPERATION       FCB
    dq sequentialReadFCB   ;AH = 14H, RECORD OPERATION     FCB
    dq sequentialWriteFCB  ;AH = 15H, RECORD OPERTAION     FCB
    dq createFileFCB       ;AH = 16H, FILE OPERATION       FCB
    dq renameFileFCB       ;AH = 17H, FILE OPERATION       FCB
    dq return              ;AH = 18H, RESERVED
    dq getCurrentDisk      ;AH = 19H, DISK MANAGEMENT
    dq setDTA              ;AH = 1AH, RECORD OPERATION     F/H
    dq FATinfoDefault      ;AH = 1BH, DISK MANAGEMENT
    dq FATinfoDevice       ;AH = 1CH, DISK MANAGEMENT
    dq return              ;AH = 1DH, RESERVED
    dq return              ;AH = 1EH, RESERVED
    dq getCurrentDPBptr    ;AH = 1FH, RESERVED INTERNAL, GET CURR DRIVE DPB PTR
    dq return              ;AH = 20H, RESERVED
    dq randomReadFCB       ;AH = 21H, RECORD OPERATION     FCB
    dq randomWriteFCB      ;AH = 22H, RECORD OPERATION     FCB
    dq getFileSizeFCB      ;AH = 23H, FILE OPERATION       FCB
    dq setRelRecordFCB     ;AH = 24H, RECORD OPERATION     FCB
    dq setIntVector        ;AH = 25H, MISC. SYS. FUNCTION
    dq createNewPSP        ;AH = 26H, PROCESS MANAGEMENT
    dq randBlockReadFCB    ;AH = 27H, RECORD OPERATION     FCB
    dq randBlockWriteFCB   ;AH = 28H, RECORD OPERATION     FCB
    dq parseFilenameFCB    ;AH = 29H, FILE OPERATION       FCB
    dq getDate             ;AH = 2AH, TIME AND DATE
    dq setDate             ;AH = 2BH, TIME AND DATE
    dq getTime             ;AH = 2CH, TIME AND DATE
    dq setTime             ;AH = 2DH, TIME AND DATE
    dq setResetVerify      ;AH = 2EH, DISK MANAGEMENT
    dq getDTA              ;AH = 2FH, RECORD OPERATION     F/H
    dq getDOSversion       ;AH = 30H, MISC. SYS. FUNCTION
    dq terminateStayRes    ;AH = 31H, PROCESS MANAGEMENT
    dq getDeviceDPBptr     ;AH = 32H, RESERVED INTERNAL, GET DEVICE DPB PTR
    dq ctrlBreakCheck      ;AH = 33H, MISC. SYS. FUNCTION
    dq getInDOSflagPtr     ;AH = 34H, RESERVED INTERNAL, GET PTR TO INDOS FLAG
    dq getIntVector        ;AH = 35H, MISC. SYS. FUNCTION
    dq getDiskFreeSpace    ;AH = 36H, DISK MANAGEMENT
    dq getsetSwitchChar    ;AH = 37H, RESERVED INTERNAL, CHANGE SWITCH CHAR
    dq getsetCountryInfo   ;AH = 38H, MISC. SYS. FUNCTION
    dq makeDIR             ;AH = 39H, DIRECTORY OPERATION
    dq removeDIR           ;AH = 3AH, DIRECTORY OPERATION
    dq changeCurrentDIR    ;AH = 3BH, DIRECTORY OPERATION
    dq createFileHdl       ;AH = 3CH, FILE OPERATION       HANDLE
    dq openFileHdl         ;AH = 3DH, FILE OPERATION       HANDLE
    dq closeFileHdl        ;AH = 3EH, FILE OPERATION       HANDLE
    dq readFileHdl         ;AH = 3FH, RECORD OPERATION     HANDLE
    dq writeFileHdl        ;AH = 40H, RECORD OPERATION     HANDLE
    dq deleteFileHdl       ;AH = 41H, FILE OPERATION       HANDLE
    dq movFileReadPtr      ;AH = 42H, RECORD OPERATION     HANDLE
    dq changeFileModeHdl   ;AH = 43H, FILE OPERATION       HANDLE
    dq ioctrl              ;AH = 44H, MISC. SYS. FUNCTION
    dq duplicateHandle     ;AH = 45H, FILE OPERATION       HANDLE
    dq forceDuplicateHdl   ;AH = 46H, FILE OPERATION       HANDLE
    dq getCurrentDIR       ;AH = 47H, DIRECTORY OPERATION
    dq allocateMemory      ;AH = 48H, MEMORY MANAGEMENT
    dq freeMemory          ;AH = 49H, MEMORY MANAGEMENT
    dq reallocMemory       ;AH = 4AH, MEMORY MANAGEMENT
    dq loadExecChild       ;AH = 4BH, PROCESS MANAGEMENT
    dq terminateClean      ;AH = 4CH, PROCESS MANAGEMENT
    dq getRetCodeChild     ;AH = 4DH, PROCESS MANAGEMENT
    dq findFirstFileHdl    ;AH = 4EH, FILE OPERATION       HANDLE
    dq findNextFileHdl     ;AH = 4FH, FILE OPERATION       HANDLE
    dq setCurrProcessID    ;AH = 50H, RESERVED INTERNAL, SET CURRENT PROCESS ID
    dq getCurrProcessID    ;AH = 51H, RESERVED INTERNAL, GET CURRENT PROCESS ID
    dq getSysVarsPtr       ;AH = 52H, RESERVED INTERNAL, GET SYSVARS POINTER
    dq createDPB           ;AH = 53H, RESERVED INTERNAL, TRANSLATE A BPB TO DPB
    dq getVerifySetting    ;AH = 54H, DISK MANAGEMENT
    dq createPSP           ;AH = 55H, RESERVED INTERNAL, CREATE A PSP
    dq renameFile          ;AH = 56H, FILE OPERATION       HANDLE
    dq getSetFileDateTime  ;AH = 57H, FILE OPERATION       HANDLE
    dq getsetMallocStrat   ;AH = 58H, MEMORY MANAGEMENT
    dq getExtendedError    ;AH = 59H, MISC. SYS. FUNCTION
    dq createUniqueFile    ;AH = 5AH, FILE OPERATION       HANDLE
    dq createNewFile       ;AH = 5BH, FILE OPERATION       HANDLE
    dq lockUnlockFile      ;AH = 5CH, RECORD OPERATION     HANDLE
    dq getCritErrorInfo    ;AH = 5DH, RESERVED INTERNAL, GET CRIT. ERROR DATA
    dq networkServices     ;AH = 5EH, RESERVED NETWORK FUNCTION
    dq networkRedirection  ;AH = 5FH, RESERVED NETWORK FUNCTION
    dq trueName            ;AH = 60H, RESERVED INTERNAL, GET TRUE NAME
    dq return              ;AH = 61H, RESERVED
    dq getPSPaddr          ;AH = 62H, PROCESS MANAGEMENT
    dq return              ;AH = 63H, RESERVED
    dq setDriverLookahead  ;AH = 64H, RESERVED INTERNAL, DRIVER LOOKAHEAD
    dq getExtLocalInfo     ;AH = 65H, MISC. SYS. FUNCTION
    dq getsetGlobalCP      ;AH = 66H, MISC. SYS. FUNCTION
    dq setHandleCount      ;AH = 67H, FILE OPERAITON       F/H
    dq commitFile          ;AH = 68H, FILE OPERATION       HANDLE
    dq getsetDiskSerial    ;AH = 69H, RESERVED INTERNAL, GET/SET DISK SER. NUM
kernelDispatchTableL  equ $ - kernelDispatchTable 
