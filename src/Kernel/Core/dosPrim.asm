;Driver Primitives, functions for Disk IO and calling a device driver
; are placed here (Int 25h Int 26h and goDriver)

dosDefCritErrHdlr:
;The DOS default critical error handler always returns FAIL
    mov al, critFail
    iretq
goDriverChar:
;Shortcut for Char requests, make a driver request
; with rsi pointing to the SFT entry as qPtr is the driver pointer
    mov rsi, qword [rsi + sft.qPtr]
goDriver:   ;Executes the driver packet pointed to by rbx
;Input: rsi = Ptr to the driver to handle the call!
;       rbx = Ptr to the request header for the driver call!
    call dosCrit2Enter
    call qword [rsi + drvHdr.strPtr]  ;Passing rbx through here
    call qword [rsi + drvHdr.intPtr]
    call dosCrit2Exit
    return

;The NUL driver lives here as it is implemented by the Kernel by default!
nulStrat:
    mov word [rbx + drvReqHdr.status], drvDonStatus    ;Set done bit directly
nulIntr:
    return

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
    test qword [rbp + cds.wFlags], cdsRedirDrive   ;On a network?
    jnz .netError
    mov rbp, qword [rbp + cds.qDPBPtr]  ;Get the DPB pointer for request
    call setWorkingDPB ;Save the DPB as the working DPB
    return
.netError:
    mov word [errorExCde], errNoNet ;Network request not supported
    jmp short .error
.diskError:
    mov word [errorExCde], errBadDrv
.error: ;This error setting needs to remain as is to allow for Int 25/46
    mov byte [errorLocus], eLocDsk
    mov byte [errorAction], eActRetUsr
    mov byte [errorClass], eClsBadFmt
    stc
    return

setupAbsDiskEntry:
;Prepares to sets up the CDS and DPB for the request
    inc byte [inDOS]
    sti ;Renable interrupts once inDOS and RSP are set
    cld ;Set string ops in the right direction
    call setupPhysicalDiskRequest
    retc    ;Error exit
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;We prob. will invalidate
    push rsi
    push rax
    lea rsi, buffer1  ;Point to one of the pathspaces
    mov byte [rsi], al  ;Construct a path
    add byte [rsi], "A" ;Convert to ASCII char
    mov word [rsi + 1], ":" ;Path Colon and terminating zero
    clc
;++++++++++++++++++++++++++++
;CRITICAL ENTRY, CHECK IF CAN DO DIRECT DISK IO!
;Entered with path in rsi (ah=03h)
    mov eax, 0300h  
    int 2Ah ;If ret with CF=CY, DO NOT PROCEED WITH ACCESS
;++++++++++++++++++++++++++++
    pop rax
    pop rsi
.exit:
    jc setupPhysicalDiskRequest.netError    ;Recycle error
    return

absDiskWrite:       ;Int 26h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    cli
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, DiskStakTop
    push rbp
    push rbx    ;Save to use rbx as the driver pointer
    call setupAbsDiskEntry
    jc absDiskExit
    call primReqWriteSetup
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 25h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    cli 
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, DiskStakTop
    push rbp    ;Save to use rbp as DPB pointer
    push rbx    ;Save to use rbx as the driver pointer
    call setupAbsDiskEntry
    jc absDiskExit
    call primReqReadSetup   ;Setup request header and get reqhdr in rbx
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
; and primary request header in rbx
    call absDiskDriverCall
    jz absDiskExit  ;Skip error code checking
    mov al, byte [primReqPkt + ioReqPkt.status] ;Get low byte into al
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
    pop rbx
    pop rbp
    cli
    mov rsp, qword [oldRSP]
    sti ;Reenable interrupts
    lea rcx, absStackJuggle
    pushfq
    pop rdx ;Get the current flags
    xchg qword [rsp], rcx   ;Get the old return address in rcx
    xchg rdx, qword [rsp + 2*8]  ;Swap old flags with current flags
    iretq ;Return from interrupt (to align the stack as needed)
absStackJuggle:
    cli
    push rdx    ;Push the flags on the stack
    push rcx    ;Push the return address on the stack
    pushfq
    xor ecx, ecx  ;Zero the registers without affecting the flags
    mov edx, ecx
    dec byte [inDOS]    ;Only now do we leave DOS
    popfq
    sti
    return

absDiskDriverCall:
;Input: rbp = Transacting DPB, ecx = Number of sectors to transfer
;       rbx = Request header address
;Output: ZF=ZE => No error, ZF=NZ => Error 
;       eax = Status word from request
;       ecx = Number of sectors transferred
    push rsi
    ;Get number of sectors to transfer in ecx (if not in ecx already)
    mov ecx, dword [primReqPkt + ioReqPkt.tfrlen]
    ;Prepare for goDriver now
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Point to device driver
    call goDriver   ;Make request
    pop rsi
    mov eax, dword [primReqPkt + ioReqPkt.tfrlen]   ;Get actual num tfrd
    sub ecx, eax    ;Get positive difference of the two 
    movzx eax, word [primReqPkt + ioReqPkt.status]
    test ax, drvErrStatus   ;Is error bit set?
    return

getDiskDPB:
;Gets the disk DPB if the Disk is valid
;Otherwise will return a pointer to the drive DPB
;Called with rdi pointing to the CDS
;CF=NC => RBP=WorkingDPB=DPBptr, CF=CY => Error exit
    mov rbp, qword [rdi + cds.qDPBPtr]  ;Get current DPB pointer
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get 0 based drive number
    mov [workingDrv], al    ;Save working drive number in working drive variable
    call setWorkingDPB
    push rdi    ;Save the CDS ptr
    call ensureDiskValid   ;Ensures the DPB is up to date and rebuilds if needed
    pop rdi
    retc
    jnz .exit
    ;Here re-init all CDS's that refer to the dpb if the disk was switched
    movzx ecx, byte [lastdrvNum]
    xor eax, eax
    dec eax ; -1 means start of root dir and never accessed (i.e. reset path)!
    mov rsi, qword [rdi + cds.qDPBPtr]  ;Get DPB ptr
    mov rdi, qword [cdsHeadPtr] ;Get start of CDS array
.checkCDS:
;Redir are skipped as they are not associated with a DPB
    test word [rdi + cds.wFlags], cdsRedirDrive
    jnz .next
    cmp qword [rdi + cds.qDPBPtr], rsi  ;If the dpb ptr matches, reset
    jne .next   ;Else, goto next
    mov dword [rdi + cds.dStartCluster], eax  ;Reset start cluster!
.next:
    add rdi, cds_size
    dec ecx
    jnz .checkCDS
.exit:
    clc
    return

ensureDiskValid:
;Do a media check, if need be to rebuild the DPB, do it!
;On entry: rbp = DPB (and working DPB = DPB)
;On exit: CF=NC => Passed, CF=CY => Fail
; IF CF=NC => ZF=ZE=> DPB Rebuilt, ZF=NZ => DPB not rebuilt
    call primReqMedCheckSetup    ;Prepare disk io packet for media check
;Return in rbx the req hdr address
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver   ;Request!
    movzx edi, word [rbx + mediaCheckReqPkt.status]
    test edi, drvErrStatus
    jnz .diskDrvCritErr
    movzx eax, byte [workingDrv]   ;Get the drive number for test
    xchg byte [rbp + dpb.bAccessFlag], ah   ;Clear access flag, get old flag
    cmp byte [rebuildDrv], al   ;Do we need to rebuild this drive?
    jne .notForce
    ;Here we are forced to rebuild the DPB. In principle, the medium has
    ; not changed but the new volume needs to be updated. 
    ;The driver can use this opportunity to synchronise its BPB with the 
    ; new label.  
    mov byte [rebuildDrv], -1   ;Now reset this flag as we are doing our job.
    jmp .resetDPB   ;Don't need to flush as the disk is the same.
.notForce:
    or byte [rbx + mediaCheckReqPkt.medret], ah ;Carry flag always cleared!
    js short .invalidateBuffers  ;If either byte is -1, freebuffers and buildbpb
    retnz ;If zero, check for dirty buffers for drv, if found, exit
    ;Here we check for any dirty buffers
    ;If dirty buffer found, dont get new DPB
    mov rdi, qword [bufHeadPtr]
.checkBuffer:
    cmp al, byte [rdi + bufferHdr.driveNumber]              ;IS this buffer for us?
    jne .gotoNextBuffer ;If no, goto next buffer
    test byte [rdi + bufferHdr.bufferFlags], dirtyBuffer    ;Is this buffer dirty?
    jz .gotoNextBuffer  ;If no, goto next buffer
    clc 
    return
.gotoNextBuffer:
    mov rdi, qword [rdi]    ;Get buffer link pointer
    cmp rdi, -1
    jne .checkBuffer        ;Check for this buffer
    ;If we get here, we found no dirty buffers for our drive
    ;We use the reference bit to keep track of which buffers we've gone through
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;Reset number of free to unknown
    call markBuffersAsUnreferenced  ;We're going to walk through so clear ref bit
.dirtyLoop:
    or byte [rdi + bufferHdr.bufferFlags], refBuffer    ;Set this buffer as referenced
    cmp al, byte [rdi + bufferHdr.driveNumber]          ;Is this buffer for us?
    jne .skipDirtyCheck
    test byte [rdi + bufferHdr.bufferFlags], dirtyBuffer    ;Is this dirty?
    je .dirtyBufferError
    ;Set reference bit and drive to free
    mov word [rdi + bufferHdr.driveNumber], (refBuffer << 8) | freeBuffer 
    call makeBufferMostRecentlyUsedGetNext  ;Move this up, get next buffer
.skipDirtyCheck:
    call findUnreferencedBuffer ;Get the next unreferenced buffer
    jnz .dirtyLoop  ;Now repeat for this buffer too
.exit:
    return
.invalidateBuffers:    ;Invalidate all buffers on all drives using this dpb
    call freeBuffersForDrive    ;Free all the buffers with the DPB in rbp
.resetDPB:    ;If no buffers found, skip freeing them as theres nothing to free!
    mov byte [rbp + dpb.bAccessFlag], -1 ;Mark DPB as inaccurate now
    ;Get a buffer to read BPB into in rdi
    xor eax, eax   ;Dummy read sector 0 in
    call getBufForDOS ;Get a disk buffer for DOS
    retc    ;Immediately exit with the carry flag set
    lea rdi, qword [rbx + bufferHdr.dataarea]
.repeatEP:
    call primReqGetBPBSetup  ;Prepare to get BPB, get request header in rbx
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rsi to driverhdr
    call goDriver   ;Request!
    movzx edi, word [rbx + mediaCheckReqPkt.status]
    test edi, drvErrStatus
    jnz .diskDrvCritErr
    ;Now rebuild the dpb fields for this drive
    mov rsi, qword [rbx + bpbBuildReqPkt.bufptr]    ;Get ptr to BPB
    push rbx
    call createDPB  ;Modifies rbx and clears the free cluster count
    pop rbx
    ;Adjust the buffer header information
    mov eax, dword [rbp + dpb.dFATlength]
    mov dword [rbx + bufferHdr.bufFATsize], eax
    mov al, byte [rbp + dpb.bNumberOfFATs]
    mov byte [rbx + bufferHdr.bufFATsize], al
    xor ah, ah    ;Set ZF and clear CF
    mov byte [rbp + dpb.bAccessFlag], ah ;DPB now ready to be used
    return
.diskDrvCritErr:
;Critical Errors fall through here
    ;rbp has dpb ptr, di has status word, rsi points to the driver
    mov dword [rbp + dpb.dFreeClustCnt], -1 ;Reset freecluster count
    mov qword [tmpDPBPtr], rbp  ;Save current DPB ptr here
    mov ah, critRead | critFAT | critFailOK | critRetryOK
    mov byte [Int24bitfld], ah  ;Save the permissions in var
    movzx edi, dil  ;Clear the upper bytes, save only error code
    call diskDevErrBitfield ;Goto disk crit error, but with bitfield set
    mov rbp, qword [tmpDPBPtr]
    cmp al, critRetry
    je ensureDiskValid
.errorExitBad:
    stc     ;Set error flag to indicate fail
    return  ;And exit

.dirtyBufferError:
    push rbp
    mov rbp, qword [rbp + dpb.qDriverHeaderPtr] ;Get the ptr to the driver
    test word [rbp + drvHdr.attrib], devDrvHdlCTL
    pop rbp
    jz .errorExitBad    ;Just return fail if bit not set
    ;rbp points to the dpb still
    push rdi
    mov rdi, qword [primReqPkt + mediaCheckReqPkt.desptr]   ;Get the pointer into rdi
    mov qword [errorVolLbl], rdi    ;Save the erroring volume label pointer
    pop rdi ;Get back the buffer pointer
    mov byte [Int24bitfld], critRead | critDOS | critRetryOK | critFailOK
    mov byte [rwFlag], 1    ;A write was the cause of the error
    mov eax, drvBadDskChnge ;Set the driver error code to bad disk change
    call diskDevErr
    cmp al, critFail    ;Did the user select fail?
    je .errorExitBad    ;If so, exit with CF set
    jmp getDiskDPB  ;Now we try again
;+++++++++++++++++++++++++++++++++++++++++++++++++
;           Primitive Driver Requests
;+++++++++++++++++++++++++++++++++++++++++++++++++
;First are Disk requests, then Char device requests
;All Disk Driver Requests come with at least rbp pointing to DPB
;All Char Requests come with rsi pointing to the Char device driver

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
; rdx = Starting sector to read/write from/to | Undefined if a char dev
; ~~~~ Set by entry function ~~~~ 
; ah = Command code
; All regs preserved EXCEPT rbx.
; Return: rbx = Transfer Address
    mov qword [primReqPkt + ioReqPkt.bufptr], rbx   ;Buffer
    mov dword [primReqPkt + ioReqPkt.tfrlen], ecx   ;Number of sectors/bytes
    mov byte [primReqPkt + ioReqPkt.hdrlen], ioReqPkt_size
    and eax, 0000FF00h  ;Clear the upper word (status word) and al
    mov dword [primReqPkt + ioReqPkt.unitnm], eax   ;Clear unit number field
    test rbp, rbp   ;If RBP is the null ptr, skip the Disk fields
    jz primReqCommonExit    ;If char request, exit!
    ;Disk operations only here!
    mov qword [primReqPkt + ioReqPkt.strtsc], rdx   ;Start sector
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqPkt + ioReqPkt.medesc], al ;Store medesc!
    mov al, byte [rbp + dpb.bUnitNumber]    ;Get the unit number
    mov byte [primReqPkt + ioReqPkt.unitnm], al ;Store the unit number
primReqCommonExit:
;Returns in rbx the primary request header as these functions
; setup the request in the primary request header space
    pop rax
    lea rbx, primReqPkt ;Put in rbx the primary request header
    return

primReqMedCheckSetup:
;Prepare the diskIO packet for mediacheck
;rbp has DPB pointer for device to check media on
    push rax
    mov byte [primReqPkt + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqPkt + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [primReqPkt + mediaCheckReqPkt.unitnm], al
    mov byte [primReqPkt + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [primReqPkt + mediaCheckReqPkt.status], 0
    jmp short primReqCommonExit

primReqGetBPBSetup:
;rbp has DPB pointer for device
;rdi has sector buffer header pointer for transfer
    push rax
    lea rax, qword [rdi + bufferHdr.dataarea]   ;Get the data area
    mov qword [primReqPkt + bpbBuildReqPkt.bufptr], rdi
    mov byte [primReqPkt + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [primReqPkt + bpbBuildReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [primReqPkt + bpbBuildReqPkt.unitnm], al
    mov byte [primReqPkt + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [primReqPkt + bpbBuildReqPkt.status], 0
    jmp short primReqCommonExit

primReqOpenSetup:
;al = unit number if a disk device. Undefined otherwise
    push rax
    mov ah, drvOPEN
    jmp short primReqOCcommon
primReqCloseSetup:
;al = unit number if a disk device. Undefined otherwise
    push rax
    mov ah, drvCLOSE
primReqOCcommon:
    mov byte [primReqPkt + openReqPkt.hdrlen], openReqPkt_size
    cwde   ;Sign extend (but top bit is zero so zero extend)
    mov dword [primReqPkt + openReqPkt.unitnm], eax
    ;Cover unit number (if disk drive, cmdcde and status)
    jmp primReqCommonExit   ;Now simply exit

secdReqCharIOReq:
;Sets up the request packet to transfer 1 byte to/from the singleIOByt buffer.
;Input:
; ah =  Data Request code (Read/Write/Write with Verify)
; ecx = Length of buffer
; rdi = Buffer pointer
;Output: 
; rbx = Transfer Address 
    lea rbx, secdReqPkt
    mov byte [rbx + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [rbx + ioReqPkt.cmdcde], ah
    mov word [rbx + ioReqPkt.status], 0
    mov qword [rbx + ioReqPkt.bufptr], rdi
    mov dword [rbx + ioReqPkt.tfrlen], ecx
    return
;If the device which will process this request is a disk device
; then we will be requesting 1 sector of data to a disk buffer.
;Then we will read the appropriate byte from that buffer to the 
; singleIOByt buffer.
;Such a request will require additional setup with the following fields:
; .unitnm, .medesc, .bufptr, .strtsc
