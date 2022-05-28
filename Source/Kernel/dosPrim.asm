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
    call ensureDiskValid   ;Ensures the DPB is up to date and rebuilds if needed
.bad:
    stc
.exit:
    ret

ensureDiskValid:
;Do a media check, if need be to rebuild the DPB, do it!
    ret



;+++++++++++++++++++++++++++++++++++++++++++++++++
;           Primitive Driver Requests
;+++++++++++++++++++++++++++++++++++++++++++++++++
;First are Disk requests, then Char device requests
;All Disk Driver Requests come with at least rbp pointing to DPB
;All Char Requests come with rsi pointing to the Char device driver

diskDrvCritErr:
;Critical Errors fall through here

diskDrvMedCheck:
;rbp has DPB pointer for device to check media on
;On ret: CF=CY => Req new BPB and make new DPB, CDS etc. CF=NC => Same disk!
    push rax
    push rbx
    push rsi
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    mov byte [rbx + mediaCheckReqPkt.hdrlen], mediaCheckReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [rbx + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [rbx + mediaCheckReqPkt.unitnm], al
    mov byte [rbx + mediaCheckReqPkt.cmdcde], drvMEDCHK
    mov word [rbx + mediaCheckReqPkt.status], 0
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver
    movzx rdi, word [rbx + mediaCheckReqPkt.status]
    test edi, 8000h
    jnz diskDrvCritErr
    test byte [rbx + mediaCheckReqPkt.medret], -1 ;Carry flag always cleared!
    js .requestBPB  ;If byte is -1, set carry flag on return
    jnz .exit ;If it is not-zero, it must be 1, so not changed
;Come here, if unsure. Check dirty buffers for a drive, if found, exit
    mov al, byte [rbp + dpb.bDriveNumber]
    call testDirtyBufferForDrive  ;If CF=CY, dirty buffer found. DO NOT GET NEW BPB!
.requestBPB:    ;Jump here if need to set carry flag from clear
    cmc ;Compliment CF from above tests
.exit:
    pop rsi
diskDrvCommonExit:
    pop rbx
    pop rax
    ret

diskDrvGetBPB:
;rbp has DPB pointer for device
;rdi has sector buffer pointer for transfer
;Returns a BPB pointer in rdi if CF=NC. An error occured if CF=CY
    push rax
    push rbx
    lea rbx, diskReqHdr ;rbx needs to point to diskReqHdr
    mov qword [rbx + bpbBuildReqPkt.bufptr], rdi
    mov byte [rbx + bpbBuildReqPkt.hdrlen], bpbBuildReqPkt_size
    mov al, byte [rbp + dpb.bMediaDescriptor]
    mov byte [rbx + mediaCheckReqPkt.medesc], al
    mov al, byte [rbp + dpb.bDriveNumber]
    mov byte [rbx + bpbBuildReqPkt.unitnm], al
    mov byte [rbx + bpbBuildReqPkt.cmdcde], drvBUILDBPB
    mov word [rbx + bpbBuildReqPkt.status], 0
    mov rsi, qword [rbp + dpb.qDriverHeaderPtr] ;Now point rdx to driverhdr
    call goDriver
    movzx rdi, word [rbx + bpbBuildReqPkt.status]
    test edi, 8000h ;Clears CF
    jnz diskDrvCritErr
    mov rdi, qword [rbx + bpbBuildReqPkt.bpbptr]
    jmp short diskDrvCommonExit

