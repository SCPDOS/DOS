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

absDiskWrite:       ;Int 46h
;al = Drive number
;rbx = Memory Buffer address to read from
;ecx = Number of sectors to write
;rdx = Start LBA to write to
    %if DEBUG
    ;Print DPB 
    debugEnterM
    lea rbp, .l0000
    call debPrintNullString
    jmp short .l0001
.l0000 db "Entering Int 46h",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    push rax
    push rbx
    push rdx
    push rsi
    mov ah, drvWRITE
    add ah, byte [verifyFlag]   ;Change to Write/Verify if set
    jmp short absDiskReadWriteCommon
absDiskRead:        ;Int 45h
;al = Drive number
;rbx = Memory Buffer address to write to
;ecx = Number of sectors to read
;rdx = Start LBA to read from
    %if DEBUG
    ;Print DPB 
    debugEnterM
    lea rbp, .l0000
    call debPrintNullString
    jmp short .l0001
.l0000 db "Entering Int 45h",0Ah,0Dh,0
.l0001:
    debugExitM
    %endif
    push rax
    push rbx
    push rdx
    push rsi
    mov ah, drvREAD
absDiskReadWriteCommon:
;Entered with the appropriate function number in ah
    push rdx    ;Save start LBA
    push rax
    call walkDPBchain   ;Get dpb ptr in rsi
    pop rax
    pop rdx

    mov byte [diskReqHdr + ioReqPkt.hdrlen], ioReqPkt_size
    mov byte [diskReqHdr + ioReqPkt.unitnm], al
    mov byte [diskReqHdr + ioReqPkt.cmdcde], ah
    mov word [diskReqHdr + ioReqPkt.status], 0
    mov al, byte [rsi + dpb.bMediaDescriptor]
    mov byte [diskReqHdr + ioReqPkt.medesc], al
    mov qword [diskReqHdr + ioReqPkt.bufptr], rbx
    mov qword [diskReqHdr + ioReqPkt.strtsc], rdx
    mov dword [diskReqHdr + ioReqPkt.tfrlen], ecx

    mov rsi, qword [rsi + dpb.qDriverHeaderPtr] ;Get driver pointer in rsi
    lea rbx, diskReqHdr ;Get ReqHeader pointer in rbx
    call goDriver   ;If carry set, command failed

    pop rsi
    pop rdx
    pop rbx
    pop rax
    jc .absDiskError
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
    movzx rdi, word [rbx + mediaCheckReqPkt.status]
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
    movzx rdi, word [rbx + bpbBuildReqPkt.status]
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
    push rbx
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    mov byte [rbx + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [rbx + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [rbx + mediaCheckReqPkt.unitnm], al
    mov byte [rbx + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [rbx + mediaCheckReqPkt.status], 0
diskDrvCommonExit:
    pop rbx
    pop rax
    ret

diskDrvGetBPB:
;rbp has DPB pointer for device
;rdi has sector buffer pointer for transfer

    push rax
    push rbx
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    mov qword [rbx + bpbBuildReqPkt.bufptr], rdi
    mov byte [rbx + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [rbx + bpbBuildReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [rbx + bpbBuildReqPkt.unitnm], al
    mov byte [rbx + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [rbx + bpbBuildReqPkt.status], 0
    jmp short diskDrvCommonExit

diskWriteSetup:
    mov ah, drvWRITE
    add ah, byte [verifyFlag]
    jmp short diskRWCommon
diskReadSetup:
    mov ah, drvREAD
diskRWCommon:
;Sets up the IO request packet with non-DPB specific fields
    mov al, ioReqPkt_size
