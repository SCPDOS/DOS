;Driver Primitives, functions for Disk IO and calling a device driver
; are placed here (Int 45h Int 46h and goDriver)

dosDefCritErrHdlr:
;The DOS default critical error handler always returns FAIL
    mov al, critFail
    iretq

goDriver:   ;Executes the driver packet pointed to by rbx
;Input: rsi = Ptr to the driver to handler the call!
;       rbx = Ptr to the request header for the driver call!
    call dosCrit2Enter
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    call dosCrit2Exit
    ret

setupPhysicalDiskRequest:
;Ensure that al has valid disk number
;Prepares working vars with data (Drv, DPB and CDS)
;If CF=NC, rbp = DPB ptr.
;If CF=CY, exit error
    test al, al
    jc .diskError
    cmp al, byte [lastdrvNum]
    jae .diskError
    push rax
    inc al  ;Get 1 based number for drive for get CDS
    call getCDS ;Gets Current CDS in Working CDS variable
    pop rax
    jc .error
    mov byte [workingDrv], al   ;Save al as the working drive number
    mov rbp, qword [workingCDS]  ;Get the current CDS pointer
    test qword [rbp +cds.wFlags], cdsRedirDrive   ;On a network?
    jnz .netError
    mov rbp, qword [rbp + cds.qDPBPtr]  ;Get the DPB pointer for request
    call setWorkingDPB ;Save the DPB as the working DPB
    ret
.netError:
    mov word [errorExCde], errNoNet ;Network request not supported
    jmp short .error
.diskError:
    mov word [errorExCde], errBadDrv
.error:
    mov byte [errorLocus], eLocDsk
    mov byte [errorAction], eActRetUsr
    mov byte [errorClass], eClsBadFmt
    stc
    ret

setupAbsDiskEntry:
;Prepares to sets up the CDS and DPB for the request
    inc byte [inDOS]
    sti ;Renable interrupts once inDOS and RSP are set
    cld ;Set string ops in the right direction
    call setupPhysicalDiskRequest
    jc .exit    ;Error exit
    mov dword [rbp + dpb.dNumberOfFreeClusters], -1 ;We prob. will invalidate
    push rsi
    push rax
    lea rsi, buffer1  ;Point to one of the pathspaces
    mov byte [rsi], al  ;Construct a path
    add byte [rsi], "A" ;Convert to ASCII char
    mov byte [rsi + 1], ":" ;Path Colon
    clc
;++++++++++++++++++++++++++++
;CRITICAL ENTRY, CHECK IF CAN DO DIRECT DISK IO!
;Entered with path in rsi (ah=03h)
    mov eax, 0300h  
    int 4Ah ;If ret with CF=CY, DO NOT PROCEED WITH ACCESS
;++++++++++++++++++++++++++++
    pop rax
    pop rsi
.exit:
    jc setupPhysicalDiskRequest.netError    ;Recycle error
    ret

absDiskWrite:       ;Int 46h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    cli
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, AuxStakTop
    push rbp
    call setupAbsDiskEntry
    jc absDiskExit
    call primReqWriteSetup
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    cli 
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, AuxStakTop
    push rbp    ;Save to use rbp as DPB pointer
    call setupAbsDiskEntry
    jc absDiskExit
    call primReqReadSetup
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
    call absDiskDriverCall
    jz absDiskExit  ;Skip error code checking
    mov al, byte [primReqHdr + ioReqPkt.status] ;Get low byte into al
    ;DOS uses the following pairs in a table
    ;AH/AL= 80/02, 40/06, 02/0C, 10/04, 04/08, 03/00
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
absDiskExit:
    pop rbp
    cli
    dec byte [inDOS]
    mov rsp, qword [oldRSP]
    sti ;Reenable interrupts
    ret ;Return from interrupt without popping flags!
absDiskDriverCall:
;Input: rbp = Transacting DPB, ecx = Number of sectors to transfer
;Output: ZF=ZE => No error, ZF=NZ => Error 
;       eax = Status word from request
;       ecx = Number of sectors transferred
    push rbx
    push rsi
    ;Get number of sectors to transfer in ecx (if not in ecx already)
    mov ecx, dword [primReqHdr + ioReqPkt.tfrlen]
    ;Prepare for goDriver now
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Point to device driver
    lea rbx, primReqHdr
    call goDriver   ;Make request
    pop rsi
    pop rbx
    mov eax, dword [primReqHdr + ioReqPkt.tfrlen]   ;Get actual num tfrd
    sub ecx, eax    ;Get positive difference of the two 
    movzx eax, word [primReqHdr + ioReqPkt.status]
    test ax, drvErrStatus   ;Is error bit set?
    ret

getDiskDPB:
;Gets the disk DPB if the Disk is physical
;Otherwise will return a pointer to the drive DPB
;Called with rdi pointing to the CDS
;CF=NC => RBP=DPBptr, CF=CY => Error exit
    mov rbp, qword [rdi + cds.qDPBPtr]  ;Get current DPB pointer
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov [workingDrv], al    ;Save working drive number in working drive variable
    call setWorkingDPB
    push rdi    ;Save the CDS ptr
    call ensureDiskValid   ;Ensures the DPB is up to date and rebuilds if needed
    pop rdi
    jc .exitBad ;Preserve CF
    jnz .exit
    ;Here re-init all CDS's that refer to the dpb if the disk was switched
    mov cl, byte [lastdrvNum]
    xor rax, rax
    dec rax ; -1 means start of root dir and never accessed (i.e. reset path)!
    mov rsi, qword [rdi + cds.qDPBPtr]  ;Get DPB ptr
    mov rdi, qword [cdsHeadPtr] ;Get start of CDS array
.checkCDS:
    cmp rsi, qword [rdi + cds.qDPBPtr]
    jne .next
    test qword [rdi + cds.qDPBPtr], rax ;Is this DPB entry empty?
    jz .next    ;IF yes, skip it
    mov dword [rdi + cds.qDPBPtr], eax  ;Reset start cluster!
.next:
    add rdi, cds_size
    dec cl
    jnz .checkCDS
.exit:
    clc
.exitBad:
    ret

ensureDiskValid:
;Do a media check, if need be to rebuild the DPB, do it!
;On entry: rbp = DPB (and working DPB = DPB)
;On exit: CF=NC => Passed, CF=CY => Fail
; IF CF=NC => ZF=ZE=> DPB Rebuilt, ZF=NZ => DPB not rebuilt
.medChk:
    call diskDrvMedCheck    ;Prepare disk io packet for media check
    lea rbx, primReqHdr
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver   ;Request!
    movzx rdi, word [primReqHdr + mediaCheckReqPkt.status]
    test edi, drvErrStatus
    jnz .diskDrvCritErrMedChk
.medChkIgnore:
    mov al, byte [workingDrv]   ;Get the drive number for test
    xor ah, ah
    xchg byte [rbp + dpb.bAccessFlag], ah   ;Clear access flag, get old access flag
    test byte [rbx + mediaCheckReqPkt.medret], ah ;Carry flag always cleared!
    js .invalidateBuffers  ;If byte is -1, freebuffers and buildbpb
    jnz .exit ;If zero, check for dirty buffers for drv, if found, exit
    call testDirtyBufferForDrive  ;If CF=CY, dirty buffer found. DO NOT GET NEW BPB!
    cmc ;Compliment CF to ensure we return CF=NC if dirty buffer found
    jc .invalidateBuffers   ;Exit ONLY if a dirty buffer found!
    ;ZF=NZ from test for dirty buffers
.exit:
    ret
.invalidateBuffers:    ;Invalidate all buffers on all drives using this dpb
    mov byte [diskChange], -1   ;In disk Change!
    call freeBuffersForDPB    ;Free all the buffers with the DPB in rbp
    ;Get a buffer to read BPB into in rdi
    mov cl, dosBuffer
    xor eax, eax   ;Dummy read sector 0 in
    call getBuffer ;Get a disk buffer
    jc .exit    ;Immediately exit with the carry flag set
    mov rdi, rbx
.repeatEP:
    call diskDrvGetBPB  ;Prepare to get BPB
    lea rbx, primReqHdr
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver   ;Request!
    movzx eax, word [primReqHdr + bpbBuildReqPkt.status]
    test eax, drvErrStatus
    jnz .diskDrvCritErrBPB
    ;Now rebuild the dpb fields for this drive
    mov rsi, qword [rbx + bpbBuildReqPkt.bufptr]    ;Get ptr to BPB
    call createDPB  
    ;Adjust the buffer header information
    mov eax, dword [rbp + dpb.dFATlength]
    mov dword [rbx + bufferHdr.bufFATsize], eax
    mov al, byte [rbp + dpb.bNumberOfFATs]
    mov byte [rbx + bufferHdr.bufFATsize], al
    xor ah, ah
    mov byte [diskChange], ah   ;Clear Disk Change flag and Set ZF and clear CF
    ret
.diskDrvCritErrMedChk:
;Critical Errors fall through here
    ;rbp has dpb ptr, di has status word, rsi points to the driver
    mov qword [xInt44RDI], rdi  ;Save rdi
    mov qword [tmpDPBPtr], rbp  ;Save current DPB ptr here
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov ah, critRead | critDOS | critFailOK | critRetryOK | critIgnorOK
    mov byte [Int44bitfld], ah  ;Save the permissions in var
    call criticalDOSError
    mov rdi, qword [xInt44RDI]
    mov rbp, qword [tmpDPBPtr]
    cmp al, critRetry
    je .medChk
    cmp al, critIgnore
    je .medChkIgnore
    mov word [errorExCde], errFI44  ;Replace with Fail on Int 44h
    stc ;Set error flag to indicate fail
    ret ;And exit from function with CF set

.diskDrvCritErrBPB:
    ;eax has status word, rbp has dpb ptr
    ;rdi has buffer header pointer, rsi points to the driver
    mov qword [xInt44RDI], rdi  ;Save rdi
    mov qword [tmpDPBPtr], rbp  ;Save current DPB ptr here
    mov edi, eax    ;Transfer the status word over
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov ah, critRead | critDOS | critFailOK | critRetryOK ;Set bits
    mov byte [Int44bitfld], ah  ;Save the permissions in var
    call criticalDOSError
    mov rdi, qword [xInt44RDI]
    mov rbp, qword [tmpDPBPtr]
    cmp al, critRetry
    je .repeatEP
    ;Else we fail (Ignore=Fail here)
    mov word [errorExCde], errFI44  ;Replace with Fail on Int 44h
    stc ;Set error flag to indicate fail
    ret ;And exit from function with CF set
;+++++++++++++++++++++++++++++++++++++++++++++++++
;           Primitive Driver Requests
;+++++++++++++++++++++++++++++++++++++++++++++++++
;First are Disk requests, then Char device requests
;All Disk Driver Requests come with at least rbp pointing to DPB
;All Char Requests come with rsi pointing to the Char device driver

diskDrvMedCheck:
;Prepare the diskIO packet for mediacheck
;rbp has DPB pointer for device to check media on
    push rax
    mov byte [primReqHdr + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqHdr + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [primReqHdr + mediaCheckReqPkt.unitnm], al
    mov byte [primReqHdr + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [primReqHdr + mediaCheckReqPkt.status], 0
diskDrvCommonExit:
    pop rax
    ret

diskDrvGetBPB:
;rbp has DPB pointer for device
;rdi has sector buffer header pointer for transfer
    push rax
    lea rax, qword [rdi + bufferHdr.dataarea]   ;Get the data area
    mov qword [primReqHdr + bpbBuildReqPkt.bufptr], rdi
    mov byte [primReqHdr + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqHdr + bpbBuildReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [primReqHdr + bpbBuildReqPkt.unitnm], al
    mov byte [primReqHdr + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [primReqHdr + bpbBuildReqPkt.status], 0
    jmp short diskDrvCommonExit

primReqWriteSetup:
    push rax
    mov ah, drvWRITE    ;Command code
    add ah, byte [verifyFlag]   ;Add verify if needed to be added
    jmp short primReqRWCommon
primReqReadSetup:
    push rax
    mov ah, drvREAD
primReqRWCommon:
;Sets up the IO request packet with:
; rbp = DPB ptr | NullPtr if a char dev
; rbx = Data storage buffer ptr
; ecx = Number of sectors to transfer
; rdx = Starting sector to read/write from/to
; ah = Command code
; All regs preserved
    mov qword [primReqHdr + ioReqPkt.bufptr], rbx   ;Buffer
    mov dword [primReqHdr + ioReqPkt.tfrlen], ecx   ;Number of sectors
    mov byte [primReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    and eax, 0000FF00h  ;Clear the upper word (status word) and al
    mov dword [primReqHdr + ioReqPkt.unitnm], eax   ;Clear unit number field
    test rbp, rbp   ;If RBP is the null ptr, skip the Disk fields
    jz diskDrvCommonExit    ;If char request, exit!
    ;Disk operations only here!
    mov qword [primReqHdr + ioReqPkt.strtsc], rdx   ;Start sector
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqHdr + ioReqPkt.medesc], al ;Store medesc!
    mov al, byte [rbp + dpb.bUnitNumber]    ;Get the unit number
    mov byte [primReqHdr + ioReqPkt.unitnm], al ;Store the unit number
    jmp diskDrvCommonExit   ;Jump popping rax