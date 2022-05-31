;Driver Primitives, functions for Disk IO and calling a device driver
; are placed here (Int 45h Int 46h and goDriver)

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
    test qword [rbp +cds.wFlags], cdsNetDrive   ;On a network?
    jnz .netError
    mov rbp, qword [rbp + cds.qDPBPtr]  ;Get the DPB pointer for request
    mov qword [workingDPB], rbp ;Save the DPB as the working DPB
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
    push rbp
    call setupPhysicalDiskRequest
    jc .exit    ;Error exit
    mov byte [rbp + dpb.dNumberOfFreeClusters], -1 ;We prob. will invalidate
    push rsi
    push rax
    lea rsi, path1  ;Point to one of the pathspaces
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
    pop rbp
    jc setupPhysicalDiskRequest.netError    ;Recycle error
    ret

absDiskWrite:       ;Int 46h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    cli
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, DiskStakTop
    call setupAbsDiskEntry
    jc absDiskExit
    call diskWriteSetup
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    cli 
    mov qword [oldRSP], rsp ;Save the stack pointer in the var space
    lea rsp, DiskStakTop
    call setupAbsDiskEntry
    jc absDiskExit
    call diskReadSetup
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
    push rbx
    lea rbx, diskReqHdr
    mov rsi, qword [workingDPB] ;Get the dpb for the current drive
    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Point to device driver
    call goDriver   ;Make request
    pop rbx
    push rax
    push rcx
    mov eax, dword [diskReqHdr + ioReqPkt.tfrlen]   ;Get actual number transferred
    sub ecx, eax    ;Get positive difference of the two 
    movzx eax, word [diskReqHdr + ioReqPkt.status]
    test ax, drvErrStatus   ;Is error bit set?
    pop rcx
    pop rax
    jz absDiskExit  ;Skip error code checking
    mov al, byte [diskReqHdr + ioReqPkt.status] ;Get low byte into al
    ;DOS uses the following pairs in a table
    ;AH/AL= 80/02, 40/06, 02/0C, 10/04, 04/08, 03/00, 01/80
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
    cli
    dec byte [inDOS]
    mov rsp, qword [oldRSP]
    sti ;Reenable interrupts
    ret ;Return from interrupt without popping flags!
    

getDiskDPB:
;Gets the disk DPB if the Disk is physical
;Otherwise will return a pointer to the drive DPB
;Called with rdi pointing to the CDS
;CF=NC => RBP=DPBptr, CF=CY => Error exit
    mov rbp, qword [rdi + cds.qDPBPtr]  ;Get current DPB pointer
    mov al, byte [rbp + dpb.bDriveNumber]   ;Get drive number
    mov [workingDrv], al    ;Save working drive number in working drive variable
    call setDPBAsWorking
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
;On entry: rbp = DPB
;On exit: CF=NC => Passed, CF=CY => Fail
;         ZF=ZE=> DPB Rebuilt, ZF=NZ => DPB not rebuilt
.medChk:
    call diskDrvMedCheck    ;Prepare disk io packet for media check
    lea rbx, diskReqHdr
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver   ;Request!
    movzx rdi, word [diskReqHdr + mediaCheckReqPkt.status]
    test edi, drvErrStatus
    jnz diskDrvCritErr
    mov al, byte [workingDrv]   ;Get the drive number for test
    xor ah, ah
    xchg byte [rbp + dpb.bAccessFlag], ah   ;Clear access flag, get old access flag
    test byte [rbx + mediaCheckReqPkt.medret], ah ;Carry flag always cleared!
    js .invalidateBuffers  ;If byte is -1, freebuffers and buildbpb
    jnz .exit ;If zero, check for dirty buffers for drv, if found, exit
    call testDirtyBufferForDrive  ;If CF=CY, dirty buffer found. DO NOT GET NEW BPB!
    cmc ;Compliment the carry flag to ensure we return CF=NC if dirty buffer found
    jc .invalidateBuffers   ;Exit ONLY if a dirty buffer found!
    ;ZF=NZ from test for dirty buffers
.exit:
    ret
.invalidateBuffers:    ;Invalidate all buffers on all drives using this dpb
    mov byte [diskChange], -1   ;In disk Change!
    call freeBuffersForDPB    ;Free all the buffers with the DPB in rbp
    ;Get a buffer to read BPB into in rdi
    mov cl, dosBuffer
    call getBuffer ;Get a disk buffer
    jc diskDrvCritErr  ;Critical error if CF is set
    mov rdi, rbx
    call diskDrvGetBPB  ;Prepare to get BPB
    lea rbx, diskReqHdr
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver   ;Request!
    movzx rdi, word [diskReqHdr + bpbBuildReqPkt.status]
    test edi, drvErrStatus
    jnz diskDrvCritErr
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

;+++++++++++++++++++++++++++++++++++++++++++++++++
;           Primitive Driver Requests
;+++++++++++++++++++++++++++++++++++++++++++++++++
;First are Disk requests, then Char device requests
;All Disk Driver Requests come with at least rbp pointing to DPB
;All Char Requests come with rsi pointing to the Char device driver

diskDrvCritErr:
;Critical Errors fall through here
    stc
    ret

diskDrvMedCheck:
;Prepare the diskIO packet for mediacheck
;rbp has DPB pointer for device to check media on
    push rax
    mov byte [diskReqHdr + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [diskReqHdr + mediaCheckReqPkt.unitnm], al
    mov byte [diskReqHdr + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [diskReqHdr + mediaCheckReqPkt.status], 0
diskDrvCommonExit:
    pop rax
    ret

diskDrvGetBPB:
;rbp has DPB pointer for device
;rdi has sector buffer pointer for transfer
    push rax
    mov qword [diskReqHdr + bpbBuildReqPkt.bufptr], rdi
    mov byte [diskReqHdr + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + bpbBuildReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [diskReqHdr + bpbBuildReqPkt.unitnm], al
    mov byte [diskReqHdr + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [diskReqHdr + bpbBuildReqPkt.status], 0
    jmp short diskDrvCommonExit

diskWriteSetup:
    push rax
    mov ah, drvWRITE    ;Command code
    add ah, byte [verifyFlag]   ;Add verify if needed to be added
    jmp short diskRWCommon
diskReadSetup:
    mov ah, drvREAD
diskRWCommon:
;Sets up the IO request packet with non-DPB specific fields
    mov al, ioReqPkt_size
    mov qword [diskReqHdr + ioReqPkt.bufptr], rbx   ;Buffer
    mov dword [diskReqHdr + ioReqPkt.tfrlen], ecx   ;Number of sectors
    mov qword [diskReqHdr + ioReqPkt.strtsc], rdx   ;Start sector
    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    and eax, 0000FFFFh  ;Clear the upper word (status word)
    mov dword [diskReqHdr + ioReqPkt.unitnm], eax
    jmp diskDrvCommonExit   ;Jump popping rax
